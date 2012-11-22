(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Corresponding to dynamics/b2island.cpp. *)
structure BDDIsland :> BDDISLAND =
struct
  open BDDSettings
  open BDDTypes
  open BDDMath
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  structure T = BDDDynamicsTypes
  structure D = BDDDynamics
  structure CS = BDDContactSolver

  fun listutil_sift _ nil = (nil, nil)
    | listutil_sift f (h :: t) =
      let val (ts, fs) = listutil_sift f t
      in
          if f h
          then (h :: ts, fs)
          else (ts, h :: fs)
      end

  fun partition_contacts_to_vector l =
      let
          fun is_nonstatic c =
              let val ba = D.F.get_body (D.C.get_fixture_a c)
                  val bb = D.F.get_body (D.C.get_fixture_b c)
              in case (D.B.get_typ ba, D.B.get_typ bb) of
                  (T.Static, _) => false
                | (_, T.Static) => false
                | _ => true
              end
          (* PERF: Too much allocation. *)
          val (nonstatic, static) = listutil_sift is_nonstatic l
      in
          Vector.fromList (nonstatic @ static)
      end

  fun report (world : ('b, 'f, 'j) D.world,
              solver : ('b, 'f, 'j) CS.contact_solver) : unit =
      (* PERF: There's some cost to iterating over the
         contacts here and preparing the function arguments,
         which probably cannot be optimized out when the
         solver is 'ignore'. Might want to use option
         interally and skip this whole call when the client
         hasn't set a listener, like Box2D does. *)
      CS.app_contacts (solver, D.W.get_post_solve world)

  (* Port note: Passing world instead of contact listener, as
     the listener is flattened into that object. *)
  (* Port note: The list arguments arrive reversed relative
     to the order they are added in Box2D code. I'm pretty
     sure the algorithm is indifferent to the order. *)
  (* In World I assume that solve_island does not modify the
     presence of bodies in its internal array (which is currently
     true); Box2D iterates over them at the end to remove the
     island flag from static bodies. *)
  fun solve_island (bodies : ('b, 'f, 'j) D.body list,
                    contacts : ('b, 'f, 'j) D.contact list,
                    joints : ('b, 'f, 'j) D.joint list,
                    world : ('b, 'f, 'j) D.world,
                    step : BDDDynamicsTypes.time_step,
                    gravity : BDDMath.vec2,
                    allow_sleep : bool) : unit =
      let
          (* XXX PERF should be unnecessary, but makes traces more like box2d. *)
          val bodies = rev bodies
          val contacts = rev contacts
          val joints = rev joints

          val bodies = Vector.fromList bodies
          val joints = Vector.fromList joints

          val () = dprint (fn () => "Solve island with " ^
                           Int.toString (Vector.length bodies) ^ " bodies and " ^
                           Int.toString (length contacts) ^ " contacts\n")
          (* Contacts are partitioned so that constraints with
             static bodies are solved last. *)
          (* Is this necessary? *)
           val contacts = partition_contacts_to_vector contacts

          val h = #dt step

          (* Integrate velocities and apply damping. *)
          fun onebody (ii, b) =
              let
                  val () = D.B.set_island_index (b, ii)
                  val sweep = D.B.get_sweep b
                  val c = sweepc sweep
                  val a = sweepa sweep
                  val v = ref (D.B.get_linear_velocity b)
                  val w = ref (D.B.get_angular_velocity b)
              in
                  (* Store positions for continuous collision. *)
                  sweep_set_c0 (sweep, c);
                  sweep_set_a0 (sweep, a);
                  (case D.B.get_typ b of
                       T.Dynamic =>
                       let
                       in
                           (* Integrate velocities. *)
                           v := (!v) :+: h *: (gravity (* TODO gravity_scale *) :+:
                                               D.B.get_inv_mass b *:
                                               D.B.get_force b);
                           w := (!w) + h * (D.B.get_inv_i b * D.B.get_torque b);

                           (* Apply damping.
                              ODE: dv/dt + c * v = 0
                              Solution: v(t) = v0 * exp(-c * t)
                              Time step: v(t + dt) =
                              v0 * exp(-c * (t + dt)) =
                              v0 * exp(-c * t) * exp(-c * dt) =
                              v * exp(-c * dt)
                              v2 = exp(-c * dt) * v1
                              Taylor expansion:
                              v2 = (1.0f - c * dt) * v1 *)
                           v := clampr (1.0 - h * D.B.get_linear_damping b, 0.0, 1.0) *: (!v);
                           w := (!w) * (clampr (1.0 - h * D.B.get_angular_damping b, 0.0, 1.0))
                       end
                     | _ => ());
                  {c = c, a = a, v = !v, w = !w}
              end

          val vectors = Vector.mapi onebody bodies
          val count = Vector.length vectors
          val positionsc = Array.tabulate (count, fn ii => #c (Vector.sub(vectors, ii)))
          val positionsa = Array.tabulate (count, fn ii => #a (Vector.sub(vectors, ii)))
          val velocitiesv = Array.tabulate (count, fn ii => #v (Vector.sub(vectors, ii)))
          val velocitiesw = Array.tabulate (count, fn ii => #w (Vector.sub(vectors, ii)))

          val pre_solver = CS.pre_contact_solver (step, contacts,
                                                  positionsc, positionsa,
                                                  velocitiesv, velocitiesw)

          val solver = CS.initialize_velocity_constraints pre_solver
          val () = if #warm_starting step
                   then CS.warm_start solver
                   else ()

          (* Initialize velocity constraints. *)
          val () = Vector.app (fn j =>
                               D.J.init_velocity_constraints (j, step))
                              joints

          (* Solve velocity constraints. *)
          val () = for 0 (#velocity_iterations step - 1)
              (fn i =>
               let in
                   Vector.app (fn j =>
                               D.J.solve_velocity_constraints (j, step))
                              joints;
                   dprint (fn () => "* Vel iter " ^ Int.toString i ^ "\n");
                   CS.solve_velocity_constraints solver
               end)

          (* Post-solve (store impulses for warm starting). *)
          val () = CS.store_impulses solver

          (* Integrate positions. *)
          fun integrate_onebody ii =
               let
                 val c = ref (Array.sub(positionsc, ii))
                 val a = ref (Array.sub(positionsa, ii))
                 val v = ref (Array.sub(velocitiesv, ii))
                 val w = ref (Array.sub(velocitiesw, ii))

                 (* Check for large velocities. *)
                 val translation : vec2 = h *: (!v)
                 val () = if dot2 (translation, translation) >
                             max_translation_squared
                          then
                              v := (max_translation / vec2length translation) *: (!v)
                          else ()
                 val rotation = h * (!w)
                 val () = if rotation * rotation > max_rotation_squared
                          then
                              w := (!w) * (max_rotation / Real.abs rotation)
                          else ()

                 (* Integrate *)
                 val () = c := !c :+: h *: !v
                 val () = a := !a + h * !w
               in
                   Array.update (positionsc, ii, !c);
                   Array.update (positionsa, ii, !a);
                   Array.update (velocitiesv, ii, !v);
                   Array.update (velocitiesw, ii, !w)
               end
          val () = for 0 (count - 1) integrate_onebody

          (* Iterate over constraints. *)
          fun iterate n =
            if n = #position_iterations step
            then ()
            else
            let val contacts_okay =
                  CS.solve_position_constraints solver
                val joints_okay = ref true
                val () = Vector.app
                    (fn j =>
                     let val joint_okay =
                           D.J.solve_position_constraints
                           (j, contact_baumgarte)
                     in
                         joints_okay := (!joints_okay andalso joint_okay)
                     end) joints
            in
              (* Exit early if the position errors are small. *)
              if contacts_okay andalso !joints_okay
              then ()
              else iterate (n + 1)
            end
          val () = iterate 0


          (* Copy state buffers back to the bodies *)
          fun copy_back_one ii =
              let
                  val body = Vector.sub (bodies, ii)
                  val sweep = D.B.get_sweep body
              in
                  sweep_set_a (sweep, Array.sub(positionsa, ii));
                  sweep_set_c (sweep, Array.sub(positionsc, ii));
                  D.B.set_linear_velocity (body, Array.sub(velocitiesv, ii));
                  D.B.set_angular_velocity (body, Array.sub(velocitiesw, ii));
                  D.B.synchronize_transform body
              end

          val () = report (world, solver)

      in
          if allow_sleep
          then
          let val min_sleep_time = ref BDDSettings.max_float
              val lin_tol_sqr = BDDSettings.linear_sleep_tolerance *
                  BDDSettings.linear_sleep_tolerance
              val ang_tol_sqr = BDDSettings.angular_sleep_tolerance *
                  BDDSettings.angular_sleep_tolerance

              fun sleep_one_body (b : ('b, 'f, 'j) D.body) : unit =
                  case D.B.get_typ b of
                      T.Static => ()
                    | _ =>
                      (* Stays awake if it's not allowed to auto_sleep,
                         or it has too much velocity. *)
                      if not (D.B.get_flag (b, D.B.FLAG_AUTO_SLEEP)) orelse
                         (D.B.get_angular_velocity b *
                          D.B.get_angular_velocity b > ang_tol_sqr) orelse
                         dot2(D.B.get_linear_velocity b,
                              D.B.get_linear_velocity b) > lin_tol_sqr
                      then
                          let in
                              D.B.set_sleep_time (b, 0.0);
                              min_sleep_time := 0.0
                          end
                      else
                          let in
                              D.B.set_sleep_time (b,
                                                  D.B.get_sleep_time b +
                                                  #dt step);
                              min_sleep_time := Real.min(!min_sleep_time,
                                                         D.B.get_sleep_time b)
                           end
          in
              Vector.app sleep_one_body bodies;
              (* The whole island goes to sleep. *)
              if !min_sleep_time > BDDSettings.time_to_sleep
              then Vector.app (fn body =>
                               D.B.clear_flag (body, D.B.FLAG_AWAKE)) bodies
              else ()
          end
          else ()
      end

  exception BDDIsland
  fun assert b = if b then () else raise BDDIsland

  fun solve_island_toi (bodies : ('b, 'f, 'j) D.body list,
                        contacts : ('b, 'f, 'j) D.contact list,
                        world : ('b, 'f, 'j) D.world,
                        step : BDDDynamicsTypes.time_step) : unit =
      let
          val bodies = rev bodies
          val contacts = rev contacts

(*           val solver = CS.contact_solver *)
      in
          raise Fail "unimplemented"
      end



end
