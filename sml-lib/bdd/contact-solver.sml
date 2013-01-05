(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Corresponding to dynamics/contacts/b2contactsolver.cpp *)
structure BDDContactSolver :> BDDCONTACT_SOLVER =
struct

  exception BDDContactSolver of string
  open BDDMath BDDTypes BDDSettings BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*: &*:
  structure D = BDDDynamics

  type velocity_constraint_point =
      { r_a : BDDMath.vec2,
        r_b : BDDMath.vec2,
        normal_impulse : real ref,
        tangent_impulse : real ref,
        normal_mass : real,
        tangent_mass : real,
        velocity_bias : real }

  type velocity_constraint =
      { points : velocity_constraint_point Array.array,
        normal : BDDMath.vec2,
        normal_mass : BDDMath.mat22,
        k : BDDMath.mat22,
        index_a : int,
        index_b : int,
        inv_mass_a : real,
        inv_mass_b : real,
        inv_i_a : real,
        inv_i_b : real,
        friction : real,
        restitution : real,
        tangent_speed : real,
        contact_index : int }

  datatype manifold_enum = Circles | FaceA | FaceB

  type position_constraint =
       { manifold : manifold,
         index_a : int,
         index_b : int,
         inv_mass_a : real,
         inv_mass_b : real,
         local_center_a : BDDMath.vec2,
         local_center_b : BDDMath.vec2,
         inv_i_a : real,
         inv_i_b : real,
         radius_a : real,
         radius_b : real
       }

  (* Parameterized by user data, since it uses the internal
     polymorpic types. *)
  type ('b, 'f, 'j) pre_contact_solver =
      { step : BDDDynamicsTypes.time_step,
        positionsc : BDDMath.vec2 Array.array,
        positionsa : real Array.array,
        velocitiesv : BDDMath.vec2 Array.array,
        velocitiesw : real Array.array,
        position_constraints : position_constraint Array.array,
        contacts : ('b, 'f, 'j) BDDDynamics.contact Vector.vector }

  type ('b, 'f, 'j) contact_solver =
      { step : BDDDynamicsTypes.time_step,
        positionsc : BDDMath.vec2 Array.array,
        positionsa : real Array.array,
        velocitiesv : BDDMath.vec2 Array.array,
        velocitiesw : real Array.array,
        position_constraints : position_constraint Array.array,
        velocity_constraints : velocity_constraint Array.array,
        contacts : ('b, 'f, 'j) BDDDynamics.contact Vector.vector }


  fun pre_contact_solver
      ({step = time_step : BDDDynamicsTypes.time_step,
        positionsc : BDDMath.vec2 Array.array,
        positionsa : real Array.array,
        velocitiesv : BDDMath.vec2 Array.array,
        velocitiesw : real Array.array},
       contacts : ('b, 'f, 'j) BDDDynamics.contact Vector.vector)
      : ('b, 'f, 'j) pre_contact_solver =
    let
        (* Initialize position independent portions of the constraints. *)
        fun onecontact (ii : int, contact : ('b, 'f, 'j) BDDDynamics.contact) =
          let
            val fixture_a = D.C.get_fixture_a contact
            val fixture_b = D.C.get_fixture_b contact
            val shape_a = D.F.get_shape fixture_a
            val shape_b = D.F.get_shape fixture_b
            val radius_a = BDDShape.get_radius shape_a
            val radius_b = BDDShape.get_radius shape_b
            val body_a = D.F.get_body fixture_a
            val body_b = D.F.get_body fixture_b
            val manifold = valOf (D.C.get_manifold contact)

(*            val local_points = case BDDCollision.manifold_points manifold of
                                   OnePoint {local_point, ...} => Array.fromList [local_point]
                                 | TwoPoints (pt1, pt2) =>
                                   Array.fromList [#local_point pt1, #local_point pt2]
*)

            val pc = { index_a = D.B.get_island_index body_a,
                       index_b = D.B.get_island_index body_b,
                       inv_mass_a = D.B.get_inv_mass body_a,
                       inv_mass_b = D.B.get_inv_mass body_b,
                       local_center_a = sweeplocalcenter (D.B.get_sweep body_a),
                       local_center_b = sweeplocalcenter (D.B.get_sweep body_b),
                       inv_i_a = D.B.get_inv_i body_a,
                       inv_i_b = D.B.get_inv_i body_b,
                       manifold = manifold,
                       radius_a = radius_a,
                       radius_b = radius_b }
          in
              pc
          end

        val position_constraints =
            Array.tabulate (Vector.length contacts,
                            fn ii => onecontact (ii, Vector.sub(contacts, ii)))
    in
      { step = time_step,
        positionsc = positionsc,
        positionsa = positionsa,
        velocitiesv = velocitiesv,
        velocitiesw = velocitiesw,
        position_constraints = position_constraints,
        contacts = contacts }
    end


fun initialize_velocity_constraints ({ step,
                                      positionsc,
                                      positionsa,
                                      velocitiesv,
                                      velocitiesw,
                                      position_constraints,
                                      contacts } : ('b, 'f, 'j) pre_contact_solver) =
    let
        fun onecontact (ii, contact) =
            let
                val pc = Array.sub(position_constraints, ii)
                val radius_a = #radius_a pc
                val radius_b = #radius_b pc
                val fixture_a = D.C.get_fixture_a contact
                val fixture_b = D.C.get_fixture_b contact
                val body_a = D.F.get_body fixture_a
                val body_b = D.F.get_body fixture_b
                val manifold = valOf (D.C.get_manifold contact)
                val points = BDDCollision.manifold_points manifold

                val index_a = D.B.get_island_index body_a
                val index_b = D.B.get_island_index body_b

                val m_a = D.B.get_inv_mass body_a
                val m_b = D.B.get_inv_mass body_b
                val i_a = D.B.get_inv_i body_a
                val i_b = D.B.get_inv_i body_b
                val friction = D.C.get_friction contact
                val restitution = D.C.get_restitution contact
                val tangent_speed = D.C.get_tangent_speed contact
                val local_center_a = #local_center_a pc
                val local_center_b = #local_center_b pc

                val c_a = Array.sub(positionsc, index_a)
                val a_a = Array.sub(positionsa, index_a)
                val v_a = Array.sub(velocitiesv, index_a)
                val w_a = Array.sub(velocitiesw, index_a)

                val c_b = Array.sub(positionsc, index_b)
                val a_b = Array.sub(positionsa, index_b)
                val v_b = Array.sub(velocitiesv, index_b)
                val w_b = Array.sub(velocitiesw, index_b)

                val q_a = rotation a_a
                val q_b = rotation a_b

                val xf_a = transform (c_a :-: (q_a @*: local_center_a),
                                      q_a)
                val xf_b = transform (c_b :-: (q_b @*: local_center_b),
                                      q_b)

                val world_manifold =
                    BDDCollision.create_world_manifold (manifold,
                                                        xf_a, radius_a,
                                                        xf_b, radius_b)

                val normal = #normal world_manifold

                fun one_vc_point (cp, wp) =
                    let
                        val (normal_impulse, tangent_impulse) =
                            if #warm_starting step
                            then ((#dt_ratio step) * (!(#normal_impulse cp)),
                                  (#dt_ratio step) * (!(#tangent_impulse cp)))
                            else (0.0, 0.0)

                        val r_a = wp :-: c_a
                        val r_b = wp :-: c_b
                        val rn_a = cross2vv(r_a, normal)
                        val rn_b = cross2vv(r_b, normal)
                        val k_normal = m_a + m_b + i_a * rn_a * rn_a + i_b * rn_b * rn_b
                        val normal_mass =
                            if k_normal > 0.0 then 1.0 / k_normal else 0.0

                        val tangent = cross2vs(normal, 1.0)
                        val rt_a = cross2vv(r_a, tangent)
                        val rt_b = cross2vv(r_b, tangent)
                        val k_tangent = m_a + m_b + i_a * rt_a * rt_a + i_b * rt_b * rt_b
                        val tangent_mass =
                            if k_tangent > 0.0 then 1.0 / k_tangent else 0.0

                        (* Set up a velocity bias for restitution. *)
                        val v_rel : real = dot2(normal,
                                                v_b :+: cross2sv(w_b, r_b) :-:
                                                v_a :-: cross2sv(w_a, r_a))
                        val velocity_bias =
                            if v_rel < ~ velocity_threshold
                            then ~restitution * v_rel
                            else 0.0

                    in
                        {r_a = r_a,
                         r_b = r_b,
                         normal_mass = normal_mass,
                         tangent_mass = tangent_mass,
                         velocity_bias = velocity_bias,
                         normal_impulse = ref normal_impulse,
                         tangent_impulse = ref tangent_impulse
                        }
                    end

                val vc_points = BDDCollision.map2_one_or_two
                                    one_vc_point points (#points world_manifold)

                val (k, normal_mass, points) =
                    (* If we have two points, then prepare the block solver. *)
                    case vc_points of
                        TwoPoints (vcp1, vcp2) =>
                        let
                            val rn1_a = cross2vv(#r_a vcp1, normal)
                            val rn1_b = cross2vv(#r_b vcp1, normal)
                            val rn2_a = cross2vv(#r_a vcp2, normal)
                            val rn2_b = cross2vv(#r_b vcp2, normal)

                            val k11 = m_a + m_b + i_a * rn1_a * rn1_a + i_b * rn1_b * rn1_b
                            val k22 = m_a + m_b + i_a * rn2_a * rn2_a + i_b * rn2_b * rn2_b
                            val k12 = m_a + m_b + i_a * rn1_a * rn2_a + i_b * rn1_b * rn2_b

                            (* Ensure a reasonable condition number. *)
                            val MAX_CONDITION_NUMBER = 1000.0
                        in
                            if k11 * k11 <
                               MAX_CONDITION_NUMBER * (k11 * k22 - k12 * k12)
                            then (* K is safe to invert. *)
                                let
                                    val k = mat22with (k11, k12, k12, k22)
                                    val normal_mass = mat22inverse k
                                in
                                    (k, normal_mass, Array.fromList [vcp1, vcp2])
                                end
                            else
                                (* The constraints are redundant; just use one.
                                     TODO_ERIN: use deepest? *)
                                (mat22with (0.0, 0.0, 0.0, 0.0),
                                 mat22with (0.0, 0.0, 0.0, 0.0),
                                 Array.fromList [vcp1])
                        end
                    (* PERF uninitialized *)
                    | OnePoint vcp =>
                      (mat22with (0.0, 0.0, 0.0, 0.0),
                       mat22with (0.0, 0.0, 0.0, 0.0),
                       Array.fromList [vcp])

            in
                { points = points,
                  normal = normal,
                  normal_mass = normal_mass,
                  k = k,
                  index_a = index_a,
                  index_b = index_b,
                  inv_mass_a = m_a,
                  inv_mass_b = m_b,
                  inv_i_a = i_a,
                  inv_i_b = i_b,
                  friction = friction,
                  restitution = restitution,
                  tangent_speed = tangent_speed,
                  contact_index = ii }
            end

        val velocity_constraints =
            Array.tabulate (Vector.length contacts,
                         fn ii => onecontact (ii, Vector.sub(contacts, ii)))
    in
        { step = step,
          positionsc = positionsc,
          positionsa = positionsa,
          velocitiesv = velocitiesv,
          velocitiesw = velocitiesw,
          position_constraints = position_constraints,
          velocity_constraints = velocity_constraints,
          contacts = contacts }
    end


fun warm_start ({ step,
                 positionsc,
                 positionsa,
                 velocitiesv,
                 velocitiesw,
                 velocity_constraints,
                 ... } : ('b, 'f, 'j) contact_solver) =
    let
        fun warm_start_one ({ index_a, index_b, normal, points,
                              inv_mass_a = m_a, inv_i_a = i_a,
                              inv_mass_b = m_b, inv_i_b = i_b,
                              ... }
                            : velocity_constraint) : unit =
          let
            val tangent = cross2vs(normal, 1.0)

            fun warm_point ({ normal_impulse,
                              tangent_impulse,
                              r_a, r_b, ... } : velocity_constraint_point) : unit =
              let
                val p : vec2 =
                    !normal_impulse *: normal :+: !tangent_impulse *: tangent
                val v_a = Array.sub(velocitiesv, index_a)
                val w_a = Array.sub(velocitiesw, index_a)
                val v_b = Array.sub(velocitiesv, index_b)
                val w_b = Array.sub(velocitiesw, index_b)

              in
                  Array.update (velocitiesw, index_a,
                                w_a - i_a * cross2vv(r_a, p));
                  Array.update (velocitiesv, index_a,
                                v_a :-: m_a *: p);
                  Array.update (velocitiesw, index_b,
                                w_b + i_b * cross2vv(r_b, p));
                  Array.update (velocitiesv, index_b,
                                v_b :+: m_b *: p)
              end
          in
            Array.app warm_point points
          end
    in
        Array.app warm_start_one velocity_constraints
    end


  (* Port note: Inner case analysis in solve_one_velocity_constraint,
     for two points. The Box2D code has a for(;;) loop, but this just
     appears to be so that the code can 'break' early. Here we just
     return. *)
  fun solve_loop (b : vec2,
                  vc : velocity_constraint,
                  a : vec2,
                  normal : vec2,
                  v_a, inv_mass_a, w_a, inv_i_a,
                  v_b, inv_mass_b, w_b, inv_i_b,
                  cp1 : velocity_constraint_point,
                  cp2 : velocity_constraint_point) : unit =
  let
    (* Only used in assertions. *)
    val ERROR_TOL : real = 1e~3

    (* Case 1: vn = 0
       0 = A * x' + b'
       Solve for x':
       x' = - inv(A) * b'
    *)
    val x : vec2 = vec2neg (#normal_mass vc +*: b)

    (* Port note: The body of each case is the same, and
       only depends on x. *)
    fun resubstitute_and_apply x =
      let
        (* Resubstitute for the incremental impulse *)
        val d : vec2 = x :-: a
        (* Apply incremental update *)
        val p1 : vec2 = vec2x d *: normal
        val p2 : vec2 = vec2y d *: normal
      in
        v_a := !v_a :-: (inv_mass_a *: (p1 :+: p2));
        w_a := !w_a - (inv_i_a * (cross2vv (#r_a cp1, p1) +
                                  cross2vv (#r_a cp2, p2)));
        v_b := !v_b :+: (inv_mass_b *: (p1 :+: p2));
        w_b := !w_b + (inv_i_b * (cross2vv (#r_b cp1, p1) +
                                  cross2vv (#r_b cp2, p2)));
        (* Accumulate *)
        #normal_impulse cp1 := vec2x x;
        #normal_impulse cp2 := vec2y x
      end

  in
    if vec2x x >= 0.0 andalso vec2y x >= 0.0
    then
    let
    in
        resubstitute_and_apply x;

        (* Postconditions *)
        (* PERF all this is just assertion when B2_DEBUG_SOLVER *)
        let val dv1 = !v_b :+: cross2sv(!w_b, #r_b cp1) :-: !v_a :-:
                cross2sv(!w_a, #r_a cp1)
            val dv2 = !v_b :+: cross2sv(!w_b, #r_b cp2) :-: !v_a :-:
                cross2sv(!w_a, #r_a cp2)
            (* Compute normal activity *)
            val vn1 = dot2(dv1, normal)
            val vn2 = dot2(dv2, normal)
        in
            if Real.abs(vn1 - #velocity_bias cp1) < ERROR_TOL andalso
               Real.abs(vn2 - #velocity_bias cp2) < ERROR_TOL
            then ()
            else raise BDDContactSolver "assertion failure"
        end
    end
    else
    let
        (* Case 2: vn1 = 0 and x2 = 0
             0 = a11 * x1' + a12 * 0 + b1'
           vn2 = a21 * x1' + a22 * 0 + b2'
        *)
        val x = vec2 (~ (#normal_mass cp1) * vec2x b, 0.0)
        val vn1 = 0.0
        val vn2 = vec2y (mat22col1 (#k vc)) * vec2x x + vec2y b
    in
        if vec2x x >= 0.0 andalso vn2 >= 0.0
        then
        let in
          resubstitute_and_apply x;

          (* Postcondtions *)
          (* PERF all assertion *)
          let
            val dv1 : vec2 = !v_b :+: cross2sv(!w_b, #r_b cp1) :-:
                !v_a :-: cross2sv(!w_a, #r_a cp1)
            (* Compute normal velocity *)
            val vn1 = dot2(dv1, normal)
          in
            dprint (fn () => "Case 2: dv1 " ^ vtos dv1 ^ " vn1 " ^ rtos vn1 ^ "\n");
            if Real.abs(vn1 - #velocity_bias cp1) < ERROR_TOL
            then ()
            else raise BDDContactSolver "assertion failure"
          end
        end
        else
        let
            (* Case 3: vn2 = 0 and x1 = 0
               vn1 = a11 * 0 + a12 * x2' + b1'
                 0 = a21 * 0 + a22 * x2' + b2'
            *)
            val x : vec2 = vec2 (0.0, ~ (#normal_mass cp2) * vec2y b)
            val vn1 = vec2x(mat22col2(#k vc)) * vec2y x + vec2x b
            val vn2 = 0.0
        in
            if vec2y x >= 0.0 andalso vn1 >= 0.0
            then
            let in
                resubstitute_and_apply x;

                (* Postconditions *)
                (* PERF all assertion *)
                let
                  val dv2 = !v_b :+: cross2sv(!w_b, #r_b cp2) :-:
                      !v_a :-: cross2sv(!w_a, #r_a cp2)
                  val vn2 = dot2(dv2, normal)
                in
                  if Real.abs(vn2 - #velocity_bias cp2) < ERROR_TOL
                  then ()
                  else raise BDDContactSolver "assertion failure"
                end
            end
            else
            let
              (* Case 4: x1 = 0 and x2 = 0
                 vn1 = b1
                 vn2 = b2 *)
              val x = vec2 (0.0, 0.0)
              val vn1 = vec2x b
              val vn2 = vec2y b
            in
              if vn1 >= 0.0 andalso vn2 >= 0.0
              then resubstitute_and_apply x
              else
                  (* No solution; give up. This is hit sometimes,
                     but it doesn't seem to matter. *)
                  ()
            end
        end
    end
  end

  (* Port note: Body of loop in SolveVelocityConstraints. *)
  fun solve_one_velocity_constraint velocitiesv velocitiesw
      (ii, vc as { index_a, index_b, normal, friction, points,
                   inv_mass_a, inv_mass_b, inv_i_a, inv_i_b, ... }) : unit =
  let
      val point_count = Array.length points

      val w_a : real ref = ref (Array.sub(velocitiesw, index_a))
      val w_b : real ref = ref (Array.sub(velocitiesw, index_b))
      val v_a : vec2 ref = ref (Array.sub(velocitiesv, index_a))
      val v_b : vec2 ref = ref (Array.sub(velocitiesv, index_b))
      val tangent : vec2 = cross2vs (normal, 1.0)

      val () = dprint (fn () => "Solve vel: v_a " ^ vtos (!v_a) ^
                       " v_b " ^ vtos (!v_b) ^
                       " w_a " ^ rtos (!w_a) ^
                       " w_b " ^ rtos (!w_b) ^
                       " norm " ^ vtos normal ^ "\n")

      val () = assert (point_count = 1 orelse point_count = 2)

      (* Solve tangent constraints. *)
      fun one_tangent_constraint (vcp : velocity_constraint_point) : unit =
        let
            (* Relative velocity at contact. *)
            val dv : vec2 = !v_b :+: cross2sv(!w_b, #r_b vcp) :-: !v_a :-:
                cross2sv(!w_a, #r_a vcp)
            (* Compute tangent force *)
            val vt : real = dot2(dv, tangent) - (#tangent_speed vc)
            val lambda : real = #tangent_mass vcp * ~vt
            (* Clamp the accumulated force *)
            val max_friction : real = friction * !(#normal_impulse vcp)
            val new_impulse : real = clampr(!(#tangent_impulse vcp) + lambda,
                                            ~max_friction,
                                            max_friction)
            val lambda = new_impulse - !(#tangent_impulse vcp)

            (* Apply contact impulse *)
            val p : vec2 = lambda *: tangent
        in
            v_a := !v_a :-: (inv_mass_a *: p);
            w_a := !w_a - (inv_i_a * cross2vv(#r_a vcp, p));
            v_b := !v_b :+: (inv_mass_b *: p);
            w_b := !w_b + (inv_i_b * cross2vv(#r_b vcp, p));
            #tangent_impulse vcp := new_impulse
        end
  in
      Array.app one_tangent_constraint (#points vc);
      (* Solve normal constraints. *)
      (case point_count of
        1 =>
          let
            val vcp = Array.sub(#points vc, 0)
            (* Relative velocity at contact *)
            val dv : vec2 = !v_b :+: cross2sv(!w_b, #r_b vcp) :-:
                !v_a :-: cross2sv(!w_a, #r_a vcp)
            (* Compute normal impulse *)
            val vn : real = dot2(dv, normal)
            val lambda : real = ~(#normal_mass vcp) * (vn - #velocity_bias vcp)
            (* Clamp the accumulated impulse *)
            val new_impulse : real =
                Real.max(!(#normal_impulse vcp) + lambda, 0.0)
            val lambda = new_impulse - !(#normal_impulse vcp)

            (* Apply contact impulse. *)
            val p : vec2 = lambda *: normal
          in
            v_a := !v_a :-: (inv_mass_a *: p);
            w_a := !w_a - (inv_i_a * cross2vv (#r_a vcp, p));
            v_b := !v_b :+: (inv_mass_b *: p);
            w_b := !w_b + (inv_i_b * cross2vv (#r_b vcp, p));
            #normal_impulse vcp := new_impulse
          end
      | 2 =>
          (* Block solver developed in collaboration with Dirk Gregorius
             (back in 01/07 on Box2D_Lite).

             Build the mini LCP for this contact patch

             vn = A * x + b, vn >= 0, , vn >= 0, x >= 0
             and vn_i * x_i = 0 with i = 1..2

             A = J * W * JT and J = ( -n, -r1 x n, n, r2 x n )
             b = vn_0 - velocityBias

             The system is solved using the "Total enumeration method"
             (s. Murty). The complementary constraint vn_i * x_i
             implies that we must have in any solution either
             vn_i = 0 or x_i = 0. So for the 2D contact problem the cases
             vn1 = 0 and vn2 = 0, x1 = 0 and x2 = 0, x1 = 0
             and vn2 = 0, x2 = 0 and vn1 = 0 need to be tested.
             The first valid solution that satisfies the problem is chosen.

             In order to account of the accumulated impulse 'a'
             (because of the iterative nature of the solver which only
             requires that the accumulated impulse is clamped and not
             the incremental impulse) we change the impulse variable (x_i).

             Substitute:

             x = x' - a

             Plug into above equation:

             vn = A * x + b
                = A * (x' - a) + b
                = A * x' + b - A * a
                = A * x' + b'
             b' = b - A * a
          *)
          let
            val cp1 = Array.sub(#points vc, 0)
            val cp2 = Array.sub(#points vc, 1)
            val a : vec2 = vec2(!(#normal_impulse cp1),
                                !(#normal_impulse cp2))

            val () = assert (vec2x a >= 0.0 andalso vec2y a >= 0.0)

            (* Relative velocity at contact *)
            val dv1 : vec2 = !v_b :+: cross2sv(!w_b, #r_b cp1) :-:
                !v_a :-: cross2sv(!w_a, #r_a cp1)
            val dv2 : vec2 = !v_b :+: cross2sv(!w_b, #r_b cp2) :-:
                !v_a :-: cross2sv(!w_a, #r_a cp2)

            (* Compute normal velocity *)
            val vn1 : real = dot2(dv1, normal)
            val vn2 : real = dot2(dv2, normal)

            val b : vec2 = vec2(vn1 - #velocity_bias cp1,
                                vn2 - #velocity_bias cp2)
            val b : vec2 = b :-: (#k vc +*: a)
          in
            solve_loop (b, vc, a, normal,
                        v_a, inv_mass_a, w_a, inv_i_a,
                        v_b, inv_mass_b, w_b, inv_i_b,
                        cp1, cp2)
          end
      | _ => raise BDDContactSolver "can only solve 1 or 2-point contacts");

      Array.update(velocitiesv, index_a, !v_a);
      Array.update(velocitiesw, index_a, !w_a);
      Array.update(velocitiesv, index_b, !v_b);
      Array.update(velocitiesw, index_b, !w_b);

      dprint (fn () => "      aft alv " ^ vtos (!v_a) ^
              " aav " ^ rtos (!w_a) ^
              " blv " ^ vtos (!v_b) ^
              " bav " ^ rtos (!w_b) ^ "\n")
  end

  fun solve_velocity_constraints
          ({ velocity_constraints, velocitiesv, velocitiesw, ... }
           : ('b, 'f, 'j) contact_solver) =
      Array.appi (solve_one_velocity_constraint velocitiesv velocitiesw) velocity_constraints

  fun store_impulses (solver : ('b, 'f, 'j) contact_solver) : unit =
    Array.app
    (fn ({contact_index, points, ... } : velocity_constraint) =>
        let
            val point_count = Array.length points
            val manifold = valOf (D.C.get_manifold (Vector.sub(#contacts solver, contact_index)))
        in
            for 0 (point_count - 1)
                (fn j =>
                    let
                        val { local_point, id, normal_impulse, tangent_impulse } =
                            BDDCollision.get_one_or_two
                                (BDDCollision.manifold_points manifold, j)
                    in
                        normal_impulse := !(#normal_impulse (Array.sub (points, j)));
                        tangent_impulse := !(#tangent_impulse (Array.sub (points, j)))
                    end)
        end ) (#velocity_constraints solver)
(*    Array.app
    (fn ({contact_index, points, ... } : velocity_constraint) =>
        let
            val point_count = Array.length points
            val mbe_manifold = D.C.get_manifold (Vector.sub(#contacts solver, contact_index))
        in
            case mbe_manifold of
                NONE => ()
              | SOME manifold =>
                BDDCollision.appi_one_or_two
                    (fn (j, pt) =>
                        let
                            val { normal_impulse, tangent_impulse, ... } = pt
                        in
                            normal_impulse := !(#normal_impulse (Array.sub (points, j)));
                            tangent_impulse := !(#tangent_impulse (Array.sub (points, j)))
                        end) (BDDCollision.manifold_points manifold)
        end ) (#velocity_constraints solver) *)

  (* Port note: A class in Box2D; it's just a function that
     returns multiple values. *)
  fun position_solver_manifold (pc : position_constraint, xf_a, xf_b, index : int) :
      { normal : vec2,  point : vec2, separation : real } =
    case #manifold pc of
        E_Circles {local_point, point } =>
          let
              val point_a = xf_a &*: local_point
              val point_b = xf_b &*: (#local_point point)
              val normal = vec2normalized (point_b :-: point_a)
          in
              { normal = normal,
                point = 0.5 *: (point_a :+: point_b),
                separation = dot2(point_b :-: point_a, normal) -
                             #radius_a pc - #radius_b pc
              }
          end
    | E_FaceA {points, local_normal, local_point} =>
          let
              val normal = transformr xf_a @*: local_normal
              val plane_point = xf_a &*: local_point
              val pt = BDDCollision.get_one_or_two (points, index)
              val clip_point = xf_b &*: (#local_point pt)
              val separation : real =
                  dot2(clip_point :-: plane_point, normal) - #radius_a pc - #radius_b pc
          in
              { normal = normal,
                point = clip_point,
                separation = separation }
          end
    | E_FaceB {points, local_normal, local_point } =>
          let
              val normal = transformr xf_b @*: local_normal
              val plane_point = xf_b &*: local_point
              val pt = BDDCollision.get_one_or_two (points, index)
              val clip_point = xf_a &*: (#local_point pt)
              val separation : real =
                  dot2(clip_point :-: plane_point, normal) - #radius_a pc - #radius_b pc
          in
              (* Ensure normal points from A to B. *)
              { normal = vec2neg normal,
                point = clip_point,
                separation = separation }
          end

  (* Sequential solver. *)
  fun solve_position_common { position_constraints,
                              velocitiesv,
                              velocitiesw,
                              positionsc,
                              positionsa }
                            baumgarte
                            slop_factor
                            mbe_toi_indices
                            : bool =
    let
      val min_separation = ref 0.0
      fun oneconstraint (pc : position_constraint) =
        let
            val index_a = #index_a pc
            val index_b = #index_b pc
            val local_center_a = #local_center_a pc
            val local_center_b = #local_center_b pc

            val (m_a, i_a) =
                case mbe_toi_indices of
                    NONE => (#inv_mass_a pc, #inv_i_a pc)
                  | SOME (toi_index_a, toi_index_b) =>
                    if index_a = toi_index_a orelse
                       index_a = toi_index_b
                    then (#inv_mass_a pc, #inv_i_a pc)
                    else (0.0, 0.0)

            val (m_b, i_b) =
                case mbe_toi_indices of
                    NONE => (#inv_mass_b pc, #inv_i_b pc)
                  | SOME (toi_index_a, toi_index_b) =>
                    if index_b = toi_index_a orelse
                       index_b = toi_index_b
                    then (#inv_mass_b pc, #inv_i_b pc)
                    else (0.0, 0.0)

            val c_a = ref (Array.sub(positionsc, index_a))
            val a_a = ref (Array.sub(positionsa, index_a))
            val c_b = ref (Array.sub(positionsc, index_b))
            val a_b = ref (Array.sub(positionsa, index_b))
        in
            (* Solve normal constraints. *)
            BDDCollision.appi_one_or_two
            (fn (j, _) =>
             let
                val q_a = rotation (!a_a)
                val q_b = rotation (!a_b)
                val xf_a = transform
                               (!c_a :-: (q_a @*: local_center_a),
                                q_a)
                val xf_b = transform
                               (!c_b :-: (q_b @*: local_center_b),
                                q_b)
                 val { normal : vec2, point : vec2, separation : real } =
                     position_solver_manifold (pc, xf_a, xf_b, j)

                 val r_a : vec2 = point :-: !c_a
                 val r_b : vec2 = point :-: !c_b

                 val () = dprint (fn () => "    pt " ^ vtos point ^
                                 " sep " ^ rtos separation ^
                                 " ra " ^ vtos r_a ^
                                 " rb " ^ vtos r_b ^ "\n")

                 (* Track max constraint error. *)
                 val () = if separation < !min_separation
                          then min_separation := separation
                          else ()

                 (* Prevent large corrections and allow slop. *)
                 val capital_c : real =
                     clampr (baumgarte * (separation + linear_slop),
                             ~max_linear_correction,
                             0.0)

                 (* Compute the effective mass. *)
                 val rn_a : real = cross2vv (r_a, normal)
                 val rn_b : real = cross2vv (r_b, normal)
                 val k = m_a + m_b + i_a * rn_a * rn_a + i_b * rn_b * rn_b

                 (* Compute normal impulse. *)
                 val impulse : real = if k > 0.0 then ~ capital_c / k else 0.0
                 val p : vec2 = impulse *: normal
             in
                 c_a := (!c_a) :-: (m_a *: p);
                 a_a := (!a_a) - (i_a * cross2vv (r_a, p));
                 c_b := (!c_b) :+: (m_b *: p);
                 a_b := (!a_b) + (i_b * cross2vv (r_b, p))
             end) (BDDCollision.manifold_points (#manifold pc));
            Array.update(positionsc, index_a, !c_a);
            Array.update(positionsa, index_a, !a_a);
            Array.update(positionsc, index_b, !c_b);
            Array.update(positionsa, index_b, !a_b)
        end
    in
      Array.app oneconstraint position_constraints;
      dprint (fn () => "  minsep: " ^ rtos (!min_separation) ^ "\n");
      (* We can't expect minSeparation >= -b2_linearSlop because we don't
         push the separation above -b2_linearSlop. *)
      !min_separation >= ~ slop_factor * linear_slop
    end

  fun solve_position_constraints (solver : ('b, 'f, 'j) contact_solver) =
      let
          val arg = { position_constraints = #position_constraints solver,
                      velocitiesv = #velocitiesv solver,
                      velocitiesw = #velocitiesw solver,
                      positionsc = #positionsc solver,
                      positionsa = #positionsa solver }
      in
          solve_position_common arg contact_baumgarte 3.0 NONE
      end

  fun solve_toi_position_constraints (presolver : ('b, 'f, 'j) pre_contact_solver,
                                      toi_index_a, toi_index_b) =
      let
          val arg = { position_constraints = #position_constraints presolver,
                      velocitiesv = #velocitiesv presolver,
                      velocitiesw = #velocitiesw presolver,
                      positionsc = #positionsc presolver,
                      positionsa = #positionsa presolver }
      in
          solve_position_common arg toi_baumgarte 1.5 (SOME (toi_index_a, toi_index_b))
      end


  (* Apply the function to every contact, paired with all of its
     impulses. *)
  fun app_contacts ({ contacts,
                      velocity_constraints,
                      ... } : ('b, 'f, 'j) contact_solver,
                    f : ('b, 'f, 'j) BDDDynamics.contact *
                        { normal_impulses : real array,
                          tangent_impulses : real array } -> unit) : unit =
      Vector.appi
      (fn (i, c : ('b, 'f, 'j) BDDDynamics.contact) =>
       let
           val vc : velocity_constraint = Array.sub(velocity_constraints, i)
           val points = #points vc
           val normal_impulses =
               Array.tabulate (Array.length points,
                               fn j =>
                               !(#normal_impulse (Array.sub(points, j))))
           val tangent_impulses =
               Array.tabulate (Array.length points,
                               fn j =>
                               !(#tangent_impulse (Array.sub(points, j))))
       in
           f (c, { normal_impulses = normal_impulses,
                   tangent_impulses = tangent_impulses })
       end) contacts


end
