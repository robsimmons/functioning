(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Main dynamics library. Ties together all the mutually-referential types
   like bodies, fixtures, contacts and joints.

   Corresponding to dynamics/b2world.cpp and
   dynamics/contacts/b2contactmanager.cpp. *)
functor BDDWorld(Arg : BDDWORLD_ARG) :>
  BDDWORLD where type fixture_data = Arg.fixture_data
             and type body_data = Arg.body_data
             and type joint_data = Arg.joint_data =
struct
  open Arg
  open BDDSettings
  open BDDTypes
  open BDDMath
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  exception BDDWorld of string

  structure T = BDDDynamicsTypes
  structure D = BDDDynamics
  structure Body = BDDBody(Arg)
  structure Fixture = BDDFixture(Arg)
  structure Joint = BDDJoint(Arg)
  structure Contact = BDDContact(Arg)
  structure DT = BDDDynamicsTypes(Arg)
  type filter = D.filter
  open DT

  fun !! (SOME x) = x
    | !! NONE = raise BDDWorld "Expected non-NONE value, like Box2D does"

  (* Port note: ContactManager is only used in World, so its data
     is flattened into that object. *)
  structure ContactManager :>
  sig
      val collide : world -> unit
      val find_new_contacts : world -> unit
      val destroy : world * contact -> unit
  end =
  struct

    val destroy = D.W.CM.destroy
    val find_new_contacts = D.W.CM.find_new_contacts

  (* This is the top level collision call for the time step. Here
     all the narrow phase collision is processed for the world
     contact list. *)
    fun collide world =
      let
        (* Update awake contacts. *)
        fun loop NONE = ()
          | loop (SOME c) =
          let
            val fixture_a = D.C.get_fixture_a c
            val fixture_b = D.C.get_fixture_b c
            val body_a = D.F.get_body fixture_a
            val body_b = D.F.get_body fixture_b

            (* Port note: Two paths to this in the original. *)
            fun common_case () =
              (* Port note: If these are nullProxy, then they get passed
                 to b2DynamicTree::GetFatAABB and assert. So this
                 is an unchecked access. These should only be NONE if
                 the corresponding body is inactive. *)
              let val proxy_a = !!(D.F.get_proxy fixture_a)
                  val proxy_b = !!(D.F.get_proxy fixture_b)
              in
                  if not (BDDBroadPhase.test_overlap (proxy_a, proxy_b))
                  (* Clear contacts that cease to overlap in the broad phase. *)
                  then let val next = D.C.get_next c
                       in destroy (world, c);
                          loop next
                       end
                  (* It persists. *)
                  else
                      let in
                          Contact.update (c, world);
                          loop (D.C.get_next c)
                      end
              end
          in
            if not (Body.get_awake body_a orelse Body.get_awake body_b)
            then loop (D.C.get_next c)
            else (* Is this contact flagged for filtering? *)
                if D.C.get_flag (c, D.C.FLAG_FILTER)
                then (* Should these bodies collide?
                        Port note: Both conditionals folded into one. *)
                    (if not (D.B.should_collide (body_b, body_a)) orelse
                        not (D.W.get_should_collide world (fixture_a, fixture_b))
                     then let val next = D.C.get_next c
                          in destroy (world, c);
                             loop next
                          end
                     else (* Clear the filtering flag. *)
                         let in
                             D.C.clear_flag (c, D.C.FLAG_FILTER);
                             common_case ()
                         end)
                else common_case ()
          end
      in
        loop (D.W.get_contact_list world)
      end

  end (* ContactManager *)


  structure World =
  struct

    open D.W
    type contact_impulse = T.contact_impulse
    datatype raycast_action = datatype T.raycast_action

    fun get_elapsed timer = (Time.-(Time.now(), timer))

    (* Should probably expose this *)
    fun default_collision_filter (fixture_a : fixture,
                                  fixture_b : fixture) : bool =
      let
          val filter_a = Fixture.get_filter fixture_a
          val filter_b = Fixture.get_filter fixture_b
      in
          if Fixture.filter_group_index filter_a =
             Fixture.filter_group_index filter_b andalso
             Fixture.filter_group_index filter_a <> 0
          then Fixture.filter_group_index filter_a > 0
          else Word16.andb
               (Fixture.filter_mask_bits filter_a,
                Fixture.filter_category_bits filter_b) <> 0w0 andalso
               Word16.andb
               (Fixture.filter_mask_bits filter_b,
                Fixture.filter_category_bits filter_a) <> 0w0
      end

    fun world (gravity, do_sleep) : world =
        D.W.new { flags = FLAG_CLEAR_FORCES,
                  body_list = NONE,
                  joint_list = NONE,
                  body_count = 0,
                  joint_count = 0,
                  gravity = gravity,
                  allow_sleep = do_sleep,

                  ground_body = NONE,
                  goodbye_joint_hook = ignore,
                  goodbye_fixture_hook = ignore,

                  inv_dt0 = 0.0,

                  warm_starting = true,
                  continuous_physics = true,

                  profile = { step = Time.zeroTime,
                              collide = Time.zeroTime,
                              solve = Time.zeroTime,
                              solve_toi = Time.zeroTime },

                  broad_phase = BDDBroadPhase.broadphase (),
                  contact_list = NONE,
                  contact_count = 0,
                  should_collide = default_collision_filter,
                  begin_contact = ignore,
                  end_contact = ignore,
                  pre_solve = ignore,
                  post_solve = ignore }


    fun get_proxy_count world =
        BDDBroadPhase.proxy_count (get_broad_phase world)

    val set_should_collide_filter = set_should_collide


    fun create_body (world : world, def) : body =
      if is_locked world
      then raise BDDWorld "Can't call create_body from callbacks."
      else
      let
          val body = D.B.new (def, world, get_body_list world)
          (* Add to doubly linked list as the new head. *)
          val () = case get_body_list world of
              NONE => ()
            | SOME b => D.B.set_prev (b, SOME body)
          val () = set_body_list (world, SOME body)
          val () = set_body_count (world, get_body_count world + 1)
      in
          body
      end

    fun create_joint (world : world,
                      def as { typ : Joint.joint_def,
                               user_data : joint_data,
                               body_a : body,
                               body_b : body,
                               collide_connected : bool }) : joint =
        if is_locked world
        then raise BDDWorld "Can't call create_joint from callbacks."
        else
        let
            val constructor = case typ of
                        D.J.MouseDef md => BDDMouseJoint.new md
                      | D.J.RevoluteDef rd => BDDRevoluteJoint.new rd
                      | D.J.PrismaticDef pd => BDDPrismaticJoint.new pd
                      | _ => raise BDDWorld "Unimplemented"
            val joint = D.J.new (constructor, def)
            (* Connect to the world list. *)
            val () = case get_joint_list world of
                NONE => ()
              | SOME j =>
                (D.J.set_prev (j, SOME joint);
                 D.J.set_next (joint, SOME j))
            val () = set_joint_list (world, SOME joint)
            val () = set_joint_count (world, get_joint_count world + 1)
            (* Connect to the bodies' doubly linked lists. *)
            val edge_a = D.J.get_edge_a joint
            (* edge_a.other got set in the call to D.J.new *)
            val () = case D.B.get_joint_list body_a of
                NONE => ()
              | SOME j =>
                (D.G.set_prev (j, SOME edge_a);
                 D.G.set_next (edge_a, SOME j))
            val () = D.B.set_joint_list (body_a, SOME edge_a)

            val edge_b = D.J.get_edge_b joint
            (* edge_b.other got set in the call to D.J.new *)
            val () = case D.B.get_joint_list body_b of
                NONE => ()
              | SOME j =>
                (D.G.set_prev (j, SOME edge_b);
                 D.G.set_next (edge_b, SOME j))
            val () = D.B.set_joint_list (body_b, SOME edge_b)
            fun one_edge ce = if D.B.eq ((D.E.get_other ce), body_a)
                              then D.C.flag_for_filtering ((D.E.get_contact ce))
                              else ()
            val () =
                if collide_connected then ()
                else oapp D.E.get_next one_edge (D.B.get_contact_list body_b)

        in joint
        end

    fun destroy_joint (world : world, joint : joint) : unit =
        if is_locked world
        then raise BDDWorld "Can't call destroy_joint from callbacks."
        else
        let
            (* Remove from the doubly linked list. *)
            val () = case D.J.get_prev joint of
                NONE => ()
              | SOME j => D.J.set_next (j, (D.J.get_next joint))

            val () = case D.J.get_next joint of
                NONE => ()
              | SOME j => D.J.set_prev (j, (D.J.get_prev joint))

            val () = case get_joint_list world of
                NONE => ()
              | SOME j => if D.J.eq (j, joint)
                          then set_joint_list (world, D.J.get_next joint)
                          else ()

            (* Disconnect from island graph. *)
            val body_a = D.J.get_body_a joint
            val body_b = D.J.get_body_b joint
            val edge_a = D.J.get_edge_a joint
            val edge_b = D.J.get_edge_b joint
            (* Wake up connected bodies. *)
            val () = D.B.set_awake (body_a, true)
            val () = D.B.set_awake (body_b, true)

            (* Remove from body a. *)
            val () = case D.G.get_prev edge_a of
                NONE => ()
              | SOME je => D.G.set_next (je, D.G.get_next edge_a)

            val () = case D.G.get_next edge_a of
                NONE => ()
              | SOME je => D.G.set_prev (je, D.G.get_prev edge_a)

            val () = case D.B.get_joint_list body_a of
                NONE => ()
              | SOME je => if D.G.eq (je, edge_a)
                           then D.B.set_joint_list (body_a,
                                                    D.G.get_next edge_a)
                           else ()

            val () = D.G.set_prev (edge_a, NONE)
            val () = D.G.set_next (edge_a, NONE)

            (* Remove from body b. *)
            val () = case D.G.get_prev edge_b of
                NONE => ()
              | SOME je => D.G.set_next (je, D.G.get_next edge_b)

            val () = case D.G.get_next edge_b of
                NONE => ()
              | SOME je => D.G.set_prev (je, D.G.get_prev edge_b)

            val () = case D.B.get_joint_list body_b of
                NONE => ()
              | SOME je => if D.G.eq (je, edge_b)
                           then D.B.set_joint_list (body_b,
                                                    D.G.get_next edge_b)
                           else ()

            val () = D.G.set_prev (edge_b, NONE)
            val () = D.G.set_next (edge_b, NONE)
            (* b2Joint::Destroy(j, &m_blockAllocator); *)
            val () = set_joint_count (world, get_joint_count world - 1)
            val () = if get_joint_count world < 0
                     then raise BDDWorld "negative joint count"
                     else ()

            (* If the joint prevents collisions, then flag any contacts for filtering. *)
            (* Port note: this is exactly the same code as in create_joint. *)
            fun one_edge ce = if D.B.eq ((D.E.get_other ce), body_a)
                              then D.C.flag_for_filtering ((D.E.get_contact ce))
                              else ()
        in
            if D.J.get_collide_connected joint
            then ()
            else oapp D.E.get_next one_edge (D.B.get_contact_list body_b)
        end

    fun oeq e (NONE, NONE) = true
      | oeq e (SOME a, SOME b) = e (a, b)
      | oeq _ _ = false

    fun destroy_body (body : body) : unit =
      let val world = D.B.get_world body
      in if is_locked world
         then raise BDDWorld "Can't call destroy_body from callbacks."
         else
         let
             (* Delete the attached joints. *)
             fun one_jointedge je =
                 let val j = !! (D.G.get_joint je)
                 in get_goodbye_joint_hook world j;
                    destroy_joint (world, j)
                 end
             val () = oapp D.G.get_next one_jointedge (D.B.get_joint_list body)
             val () = D.B.set_joint_list (body, NONE)

             (* Delete the attached contacts. *)
             fun one_contactedge ce =
                 ContactManager.destroy
                 (world, D.E.get_contact ce )

             val () = oapp D.E.get_next one_contactedge (D.B.get_contact_list
                                                         body)
             val () = D.B.set_contact_list (body, NONE)

             (* Delete the attached fixtures. This destroys broad-phase
                proxies. *)
             fun one_fixture f =
                 (get_goodbye_fixture_hook world f;
                  D.F.destroy_proxy (f, get_broad_phase world);
                  Body.destroy_fixture (body, f))
             val () = oapp D.F.get_next one_fixture (D.B.get_fixture_list body)
             val () = D.B.set_fixture_list (body, NONE)
             val () = D.B.set_fixture_count (body, 0)

             (* Remove from world body list *)
             val prev = D.B.get_prev body
             val next = D.B.get_next body
             val () = case prev of
                 NONE => ()
               | SOME prev => D.B.set_next (prev, next)
             val () = case next of
                 NONE => ()
               | SOME next => D.B.set_prev (next, prev)

             val () = if oeq D.B.eq (get_body_list world, SOME body)
                      then set_body_list (world, next)
                      else ()

             val () = set_body_count (world, get_body_count world - 1)
         in
             ()
         end
    end

    fun clear_forces (world : world) : unit =
        oapp Body.get_next (fn b =>
                            let in
                                D.B.set_force (b, vec2 (0.0, 0.0));
                                D.B.set_torque (b, 0.0)
                            end) (get_body_list world)

    fun query_aabb (world : world,
                    callback : fixture -> bool,
                    aabb : BDDTypes.aabb) : unit =
        BDDBroadPhase.query (get_broad_phase world,
                             (fn proxy =>
                              let val fixture = BDDBroadPhase.user_data proxy
                              in callback fixture
                              end),
                             aabb)

    fun ray_cast (world : world,
                  callback : { fixture : fixture, point : BDDMath.vec2,
                               normal : BDDMath.vec2, fraction : real } -> raycast_action,
                  point1 : BDDMath.vec2,
                  point2 : BDDMath.vec2) : unit =
        let val bp = get_broad_phase world
            fun cb (input as { p1 : BDDMath.vec2,
                               p2 : BDDMath.vec2,
                               max_fraction : real }, proxy) =
              let val fixture = BDDBroadPhase.user_data proxy
                  val hit = Fixture.ray_cast (fixture, input)
              in
                  case hit of
                      NONE => max_fraction
                    | SOME { normal, fraction } =>
                          let val point : vec2 =
                              (1.0 - fraction) *: p1 :+: fraction *: p2
                          (* TODO: Might want to propagate this datatype
                             deeper; it's better. *)
                          in case callback { fixture = fixture,
                                             point = point,
                                             normal = normal,
                                             fraction = fraction } of
                              IgnoreAndContinue => ~1.0
                            | Terminate => 0.0
                            | Clip r => r
                            | Don'tClip => 1.0
                          end
              end
        in
            BDDBroadPhase.ray_cast (bp, cb, { max_fraction = 1.0,
                                              p1 = point1,
                                              p2 = point2 })
        end
    (* Find islands, integrate and solve constraints, solve position
       constraints. *)
    fun solve (world : world, step : T.time_step) =
      let
        val () = dprint (fn () => "SOLVE.\n")

(*
        (* XXX just debug *)
        val () = oapp D.B.get_next
            (fn b =>
             let in
                 dprint (fn () =>
                         "Presolve sweep: " ^ sweeptos (D.B.get_sweep b) ^ "\n" ^
                         "            xf: " ^ xftos (D.B.get_xf b) ^ "\n")
             end)
            (get_body_list world)
        (* XXX end just debug *)
*)

        (* Clear all the island flags. *)
        val () = oapp D.B.get_next
                      (fn b => D.B.clear_flag(b, D.B.FLAG_ISLAND))
                      (get_body_list world)
        val () = oapp D.C.get_next
                      (fn c => D.C.clear_flag(c, D.C.FLAG_ISLAND))
                      (get_contact_list world)
        val () = oapp Joint.get_next
                      (fn j => D.J.clear_flag(j, D.J.FLAG_ISLAND))
                      (get_joint_list world)

        (* Build and simulate all awake islands. *)
        (* Port note: It's possible to avoid the need for an explicit
           stack here; just make a recursive call to |explore| instead
           of pushing to |stack|. Doing this can change the order that
           bodies are added, but should result in the same set.
           However, the body order is important if we want to exactly
           match the behavior of the original.

           The approach is to look at every body in the world and find
           all connected bodies using a depth-first graph traversal.
           Once the island flag is set, it means it has already been
           accounted for. The exception is static bodies: These
           participate in islands but don't count as edges. (This is
           okay because we know they never move.) They're treated
           somewhat specially in the following; for example, they
           can't be used as seeds. *)

       fun one_seed (seed : body) =
         if D.B.get_flag (seed, D.B.FLAG_ISLAND)
         (* Already explored? *)
         then ()
         else if not (Body.get_awake seed) orelse not (Body.get_active seed)
         then ()
         else if D.B.get_typ seed = T.Static
         (* Must be dynamic or kinematic. *)
         then ()
         else
         let
           (* Accumulates arguments for island solver. *)
           val bodies = ref nil
           val joints = ref nil
           val contacts = ref nil
           (* Perform a depth first search (DFS) on the constraint graph. *)

           val stack = ref [seed];
           val () = D.B.set_flag (seed, D.B.FLAG_ISLAND)

           fun explore () =
            case !stack of
                nil => ()
              | b :: stack' =>
              (stack := stack';
               if not (Body.get_active b)
               then raise BDDWorld "expected body to be active in stack"
               else
               let
                   (* Add to island. *)
                   val () = bodies := b :: !bodies
                   (* Make sure body is awake. *)
                   val () = D.B.set_flag (b, D.B.FLAG_AWAKE)
               in
                   (* To keep islands as small as possible, we don't
                      propagate islands across static bodies. *)
                   if D.B.get_typ b = T.Static
                   then ()
                   else
                   let
                     (* For each body we add, we look at its contacts and
                        joints, which might include other bodies in the
                        island. *)
                     fun one_cedge (ce : contactedge) =
                       let val contact = D.E.get_contact ce
                           val fixture_a = D.C.get_fixture_a contact
                           val fixture_b = D.C.get_fixture_b contact
                       in
                         dprint (fn () =>
                                 " .. edge " ^
                                 (if (D.C.get_flag (contact, D.C.FLAG_TOUCHING))
                                  then "touching"
                                  else "not-touching") ^ "\n");

                         (* Has this contact already been added to an
                            island? Is it enabled and touching? Are
                            both fixtures non-sensors? *)
                         if D.C.get_flag (contact, D.C.FLAG_ISLAND) orelse
                            not (D.C.get_flag
                                 (contact, D.C.FLAG_ENABLED)) orelse
                            not (D.C.get_flag
                                 (contact, D.C.FLAG_TOUCHING)) orelse
                            Fixture.is_sensor fixture_a orelse
                            Fixture.is_sensor fixture_b
                         then ()
                         else
                           let
                               val other : body =  (D.E.get_other ce)
                           in
                               D.C.set_flag (contact, D.C.FLAG_ISLAND);
                               contacts := contact :: !contacts;
                               (* Was the body already added to this island?
                                  Port note: Really we're testing to see if
                                  it belongs to any island, but since all
                                  links are symmetric, we would have been
                                  added as well if it were inserted into
                                  a previous island. *)
                               if D.B.get_flag (other, D.B.FLAG_ISLAND)
                               then ()
                               else (stack := (other :: (!stack));
                                     D.B.set_flag (other, D.B.FLAG_ISLAND))
                           end
                       end

                     fun one_jedge (je : jointedge) =
                       let val other = D.G.get_other je
                           val joint = !! (D.G.get_joint je)
                       in
                           (* If we've already visited this joint, or the
                              attached body is inactive, then skip *)
                           if D.J.get_flag (joint, D.J.FLAG_ISLAND) orelse
                              not (D.B.get_flag (other, D.B.FLAG_ACTIVE))
                           then ()
                           else
                           let in
                              joints := joint :: !joints;
                              D.J.set_flag (joint, D.J.FLAG_ISLAND);
                              (* If we haven't already visited it, explore
                                 the other body in the joint. *)
                              if D.B.get_flag (other, D.B.FLAG_ISLAND)
                              then ()
                              else (stack := (other :: (!stack));
                                     D.B.set_flag (other, D.B.FLAG_ISLAND))
                           end
                       end

                   in
                       oapp D.E.get_next one_cedge (D.B.get_contact_list b);
                       oapp D.G.get_next one_jedge (D.B.get_joint_list b)
                   end;
                   explore ()
               end)

         in
             explore ();
             BDDIsland.solve_island (!bodies, !contacts, !joints, world,
                                     step,
                                     get_gravity world, get_allow_sleep world);

             (* Post solve cleanup: Allow static bodies to participate in
                other islands. *)
             app (fn b => if D.B.get_typ b = T.Static
                          then D.B.clear_flag (b, D.B.FLAG_ISLAND)
                          else ()) (!bodies)
         end
       val () = oapp D.B.get_next one_seed (get_body_list world)

       fun one_sync (b : body) =
           (* If the body was not in an island, then it didn't move. *)
           if not (D.B.get_flag (b, D.B.FLAG_ISLAND)) orelse
              D.B.get_typ b = T.Static
           then ()
           (* Otherwise update its fixtures for the broad phase. *)
           else D.B.synchronize_fixtures (b, get_broad_phase world)
       val () = oapp D.B.get_next one_sync (get_body_list world)
    in
      ContactManager.find_new_contacts world
    end


    (* Find TOI contacts and solve them. *)
    fun solve_toi (world : world, step : T.time_step) : unit =
      let
          val () = dprint (fn () => "SOLVE_TOI()\n")

          fun onebody b =
              let in
                  D.B.clear_flag (b, D.B.FLAG_ISLAND);
                  sweep_set_alpha0 (D.B.get_sweep b, 0.0)
              end
          val () = oapp D.B.get_next onebody (get_body_list world)

          fun onecontact c =
            let in
                (* Invalidate TOI *)
                D.C.clear_flag (c, D.C.FLAG_TOI);
                D.C.clear_flag (c, D.C.FLAG_ISLAND);
                D.C.set_toi_count (c, 0);
                D.C.set_toi (c, 1.0)
            end
          val () = oapp D.C.get_next onecontact (get_contact_list world)

          (* for control flow *)
          exception Return
          exception ContinueLoop

          (* Find TOI events and solve them. *)
          fun loop () =
              let
                  (* Find the first TOI. *)
                  val mbe_min_contact : DT.contact option ref = ref NONE
                  val min_alpha_ref = ref 1.0
                  fun onecontact_toi c =
                      let
                          (* for control flow *)
                          exception Continue
                          (* Is this contact disabled? *)
                          val () = if not (D.C.get_enabled c)
                                   then raise Continue
                                   else ()
                          (* Prevent excessive sub-stepping. *)
                          val () = if D.C.get_toi_count c > max_substeps
                                   then raise Continue
                                   else ()
                          val alpha =
                              if D.C.get_flag (c, D.C.FLAG_TOI)
                              then D.C.get_toi c
                              else
                                  let
                                      val f_a = D.C.get_fixture_a c
                                      val f_b = D.C.get_fixture_b c

                                      (* Is there a sensor? *)
                                      val () = if D.F.get_sensor f_a orelse
                                                  D.F.get_sensor f_b
                                               then raise Continue
                                               else ()

                                      val b_a = D.F.get_body f_a
                                      val b_b = D.F.get_body f_b

                                      val type_a = D.B.get_typ b_a
                                      val type_b = D.B.get_typ b_b
                                      val () = assert (type_a = T.Dynamic orelse
                                                       type_b = T.Dynamic)

                                      val active_a = Body.get_awake b_a andalso type_a <> T.Static
                                      val active_b = Body.get_awake b_b andalso type_b <> T.Static

                                      (* Is at least one body active (awake and
                                         dynamic or kinematic)? *)
                                      val () = if (not active_a) andalso (not active_b)
                                               then raise Continue
                                               else ()

                                      val collide_a = Body.get_bullet b_a orelse
                                                      type_a <> T.Dynamic
                                      val collide_b = Body.get_bullet b_b orelse
                                                      type_b <> T.Dynamic

                                      (* Are these two non-bullet dynamic bodies? *)
                                      val () = if (not collide_a) andalso (not collide_b)
                                               then raise Continue
                                               else ()

                                      (* Compute the TOI for this contact. *)
                                      (* Put the sweeps onto the same time interval. *)
                                      val sweep_a = D.B.get_sweep b_a
                                      val sweep_b = D.B.get_sweep b_b
                                      val alpha0_a = sweepalpha0 sweep_a
                                      val alpha0_b = sweepalpha0 sweep_b
                                      val alpha0 =
                                          if alpha0_a < alpha0_b
                                          then (sweep_advance (sweep_a, alpha0_b);
                                                alpha0_b)
                                          else (sweep_advance (sweep_b, alpha0_a);
                                                alpha0_a)
                                      val () = assert (alpha0 < 1.0)

                                      val toi_input =
                                          { proxya = BDDDistance.shape_proxy (D.F.get_shape f_a),
                                            proxyb = BDDDistance.shape_proxy (D.F.get_shape f_b),
                                            sweepa = sweepcopy (sweep_a),
                                            sweepb = sweepcopy (sweep_b),
                                            tmax = 1.0 }
                                      val toires = BDDTimeOfImpact.time_of_impact toi_input
                                      val alpha =
                                          case toires of
                                              (BDDTimeOfImpact.STouching, beta) =>
                                                Real.min(alpha0 + (1.0 - alpha0) * beta, 1.0)
                                            | _ => 1.0
                                      val () = D.C.set_toi (c, alpha)
                                      val () = D.C.set_flag (c, D.C.FLAG_TOI)
                                  in
                                      alpha
                                  end
                          val () = if alpha < (!min_alpha_ref)
                                   then (mbe_min_contact := SOME c;
                                         min_alpha_ref := alpha)
                                   else ()
                      in
                          ()
                      end handle Continue => ()

                  val () = oapp D.C.get_next onecontact_toi (get_contact_list world)

                  val (min_contact, min_alpha) =
                      case (!mbe_min_contact, 1.0 - 10.0 * epsilon < (!min_alpha_ref)) of
                          (SOME c, false) => (c, !min_alpha_ref)
                        | _ =>
                          (* No more TOI events. Done! *)
                          raise Return

                  val () = dprint (fn () => "min_alpha: " ^ rtos min_alpha ^ "\n")

                  (* Advance the bodies to the TOI. *)
                  val f_a = D.C.get_fixture_a min_contact
                  val f_b = D.C.get_fixture_b min_contact
                  val b_a = D.F.get_body f_a
                  val b_b = D.F.get_body f_b

                  val backup_a = sweepcopy (D.B.get_sweep b_a)
                  val backup_b = sweepcopy (D.B.get_sweep b_b)

                  val () = D.B.advance (b_a, min_alpha)
                  val () = D.B.advance (b_b, min_alpha)

                  (* The TOI contact likely has some new contact points. *)
                  val () = Contact.update (min_contact, world)
                  val () = D.C.clear_flag (min_contact, D.C.FLAG_TOI)
                  val () = D.C.set_toi_count (min_contact, 1 + D.C.get_toi_count min_contact)

                  (* Is the contact solid? *)
                  val () = if (not (D.C.get_enabled min_contact)) orelse
                              (not (Contact.is_touching min_contact))
                           then
                               (* Restore the sweeps. *)
                               (D.C.set_enabled (min_contact, false);
                                D.B.set_sweep (b_a, backup_a);
                                D.B.set_sweep (b_b, backup_b);
                                D.B.synchronize_transform b_a;
                                D.B.synchronize_transform b_b;
                                raise ContinueLoop
                               )
                           else ()

                  val () = Body.set_awake (b_a, true)
                  val () = Body.set_awake (b_b, true)

                  (* Build the island *)
                  val bodies = ref [b_b, b_a]
                  val contacts = ref [min_contact]

                  val () = D.B.set_flag (b_a, D.B.FLAG_ISLAND)
                  val () = D.B.set_flag (b_b, D.B.FLAG_ISLAND)
                  val () = D.C.set_flag (min_contact, D.C.FLAG_ISLAND)

                  (* Get contacts on bodyA and bodyB. *)
                  fun onebody b =
                      case Body.get_type b of
                          T.Dynamic =>
                          let
                              exception Continue
                              fun onecontactedge ce =
                                  let
                                      (* TODO bodyCapacity? contactCapacity? *)
                                      val contact = (D.E.get_contact ce)

                                      (* Has this contact already been added to the island? *)
                                      val () = if D.C.get_flag (contact, D.C.FLAG_ISLAND)
                                               then raise Continue
                                               else ()

                                      (* Only add static, kinemetic, or bullet bodies. *)
                                      val other = (D.E.get_other ce)
                                      val () = if Body.get_type other = T.Dynamic andalso
                                                  (not (Body.get_bullet b)) andalso
                                                  (not (Body.get_bullet other))
                                               then raise Continue
                                               else ()

                                      (* Skip sensors. *)
                                      val sensor_a = Fixture.is_sensor (D.C.get_fixture_a contact)
                                      val sensor_b = Fixture.is_sensor (D.C.get_fixture_b contact)
                                      val () = if sensor_a orelse sensor_b
                                               then raise Continue
                                               else ()

                                      (* Tentatively advance the body to the TOI. *)
                                      val backup = sweepcopy (D.B.get_sweep other)
                                      val () = if not (D.B.get_flag (other, D.B.FLAG_ISLAND))
                                               then D.B.advance (other, min_alpha)
                                               else ()

                                      (* Update the contact points *)
                                      val () = Contact.update (contact, world)

                                      (* Was the contact disabled by the user? *)
                                      val () = if not (D.C.get_enabled contact)
                                               then (D.B.set_sweep (other, backup);
                                                     D.B.synchronize_transform other;
                                                     raise Continue
                                                    )
                                               else ()

                                      (* Are there contact points? *)
                                      val () = if not (Contact.is_touching contact)
                                               then (D.B.set_sweep (other, backup);
                                                     D.B.synchronize_transform other;
                                                     raise Continue
                                                    )
                                               else ()

                                      (* Add the contact to the island *)
                                      val () = D.C.set_flag (contact, D.C.FLAG_ISLAND)
                                      val () = contacts := contact :: !contacts

                                      (* Has the other body already been added to the island? *)
                                      val () = if D.B.get_flag (other, D.B.FLAG_ISLAND)
                                               then raise Continue
                                               else ()

                                      (* Add the other body to the island. *)
                                      val () = D.B.set_flag (other, D.B.FLAG_ISLAND)
                                      val () = if D.B.get_typ other <> T.Static
                                               then Body.set_awake (other, true)
                                               else ()

                                      val () = bodies := other :: !bodies

                                  in
                                      ()
                                  end handle Continue => ()

                              val () = oapp D.E.get_next onecontactedge (D.B.get_contact_list b)
                          in
                              ()
                          end
                        | _ => ()
                  val () = List.app onebody [b_a, b_b]

                  val dt = (1.0 - min_alpha) * (#dt step)
                  val substep = {dt = dt,
                                 inv_dt = 1.0 / dt,
                                 dt_ratio = 1.0,
                                 position_iterations = 20,
                                 velocity_iterations = #velocity_iterations step,
                                 warm_starting = false
                                }
                  val () = BDDIsland.solve_island_toi (!bodies, !contacts, world, substep)

                  (* Reset island flags and synchronize broad-phase proxies. *)
                  fun onebody body =
                      (D.B.clear_flag (body, D.B.FLAG_ISLAND);
                       case D.B.get_typ body of
                           T.Dynamic =>
                           let
                               val () = D.B.synchronize_fixtures (body, get_broad_phase world)
                               fun onece ce =
                                   ( D.C.clear_flag ((D.E.get_contact ce), D.C.FLAG_TOI);
                                     D.C.clear_flag ((D.E.get_contact ce), D.C.FLAG_ISLAND)
                                   )
                               val () = oapp D.E.get_next onece (D.B.get_contact_list body)
                           in
                               ()
                           end
                         | _ => ()
                      )
                  val () = List.app onebody (!bodies)

                  (* Commit fixture proxy movements to the broad-phase so that new
                     contacts are created. Also, some contacts can be destroyed. *)
                  val () = ContactManager.find_new_contacts world

              (* TODO substepping? *)

              in
                  loop ()
              end handle Return => ()
                       | ContinueLoop => loop ()
      in
          loop ()
      end

    fun step (world : world, dt : real,
              velocity_iterations : int, position_iterations : int) : unit =
      let
          (* DEBUG
          val () = print "bodies:\n"
          fun onebody b = print (xftos (Body.get_transform b) ^ "\n")
          val () = oapp Body.get_next onebody (D.W.get_body_list world) *)

          val step_timer = Time.now()
          (* XXX good, but not implemented in box2d *)
          (* val () = BDDBroadPhase.debugprint (fn _ => "?") (D.W.get_broad_phase world) *)

          (* If new fixtures were added, we need to find the new contacts. *)
          val () = if get_flag (world, FLAG_NEW_FIXTURE)
                   then (ContactManager.find_new_contacts world;
                         clear_flag (world, FLAG_NEW_FIXTURE))
                   else ()

          (* XXX all debug *)
(*          fun onecontact c =
        let
            val { point_count, ... } = Contact.get_manifold c
            val world_manifold = Contact.get_world_manifold c
            fun dprint f = print (f ())
        in
            dprint (fn () => " Contact! ");
            if Contact.is_touching c
            then dprint (fn () => "touching ")
            else ();
            dprint (fn () => itos point_count ^ " points: ");
            for 0 (point_count - 1)
            (fn i =>
             let val pt = Array.sub(#points world_manifold, i)
             in
                 dprint (fn () => vtos pt ^ ", ")
             end);

            dprint (fn () => "\n")
        end
          val () = oapp Contact.get_next onecontact (D.W.get_contact_list world)
*)

          (* XXX end all debug *)

          val () = set_flag (world, FLAG_LOCKED)

          val inv_dt = if dt > 0.0
                       then 1.0 / dt
                       else 0.0
          val step = { dt = dt,
                       velocity_iterations = velocity_iterations,
                       position_iterations = position_iterations,
                       inv_dt = inv_dt,
                       dt_ratio = get_inv_dt0 world * dt,
                       warm_starting = get_warm_starting world }
          (* Update contacts. This is where some contacts are destroyed. *)
          val timer = Time.now()
          val () = ContactManager.collide world
          val collide_time = get_elapsed timer

          (* XXX all debug *)
(*          fun onecontact2 c =
        let
            val { point_count, ... } = Contact.get_manifold c
            val world_manifold = Contact.get_world_manifold c
            fun dprint f = print (f ())
        in
            dprint (fn () => " post-collide Contact! ");
            if Contact.is_touching c
            then dprint (fn () => "touching ")
            else ();
            dprint (fn () => itos point_count ^ " points: ");
            for 0 (point_count - 1)
            (fn i =>
             let val pt = Array.sub(#points world_manifold, i)
                 (* val (x, y) = vectoscreen pt *)
             in
                 dprint (fn () => vtos pt ^ ", ")
             end);

            dprint (fn () => "\n")
        end
          val () = oapp Contact.get_next onecontact2 (D.W.get_contact_list world)
*)


          (* Integrate velocities, solve velocity constraints, and
             integrate positions. *)
          val timer = Time.now()
          val () = if dt > 0.0
                   then solve (world, step)
                   else ()
          val solve_time = get_elapsed timer

          (* XXX just debug *)
(*          val () = oapp D.B.get_next
              (fn b =>
               let in
                   dprint (fn () =>
                           "Postsolve sweep: " ^ sweeptos (D.B.get_sweep b) ^ "\n" ^
                           "             xf: " ^ xftos (D.B.get_xf b) ^ "\n")
               end)
              (get_body_list world) *)
          (* XXX end just debug *)

          (* XXX all debug *)
(*          fun onecontact2 c =
        let
            val { point_count, ... } = Contact.get_manifold c
            val world_manifold = Contact.get_world_manifold c
            fun dprint f = print (f ())
        in
            dprint (fn () => " post-solve Contact! ");
            if Contact.is_touching c
            then dprint (fn () => "touching ")
            else ();
            dprint (fn () => itos point_count ^ " points: ");
            for 0 (point_count - 1)
            (fn i =>
             let val pt = Array.sub(#points world_manifold, i)
                 (* val (x, y) = vectoscreen pt *)
             in
                 dprint (fn () => vtos pt ^ ", ")
             end);

            dprint (fn () => "\n")
        end
          val () = oapp Contact.get_next onecontact2 (D.W.get_contact_list world)
*)



          (* Handle TOI events. *)
          val timer = Time.now()
          val () = if get_continuous_physics world andalso dt > 0.0
                   then solve_toi (world, step)
                   else ()
          val toi_time = get_elapsed timer

          (* XXX just debug *)
(*          val () = oapp D.B.get_next
              (fn b =>
               let in
                   dprint (fn () =>
                           "Posttoi sweep: " ^ sweeptos (D.B.get_sweep b) ^ "\n" ^
                           "           xf: " ^ xftos (D.B.get_xf b) ^ "\n")
               end)
              (get_body_list world) *)
          (* XXX end just debug *)


          (* XXX all debug *)
(*          fun onecontact2 c =
        let
            val { point_count, ... } = Contact.get_manifold c
            val world_manifold = Contact.get_world_manifold c
            fun dprint f = print (f ())
        in
            dprint (fn () => " post-toi Contact! ");
            if Contact.is_touching c
            then dprint (fn () => "touching ")
            else ();
            dprint (fn () => itos point_count ^ " points: ");
            for 0 (point_count - 1)
            (fn i =>
             let val pt = Array.sub(#points world_manifold, i)
             in
                 dprint (fn () => vtos pt ^ ", ")
             end);

            dprint (fn () => "\n")
        end
          val () = oapp Contact.get_next onecontact2 (D.W.get_contact_list world)
*)


          val () = if dt > 0.0
                   then set_inv_dt0 (world, inv_dt)
                   else ()

          val () = if get_flag (world, FLAG_CLEAR_FORCES)
                   then clear_forces world
                   else ()

          val () = clear_flag (world, FLAG_LOCKED)

          val step_time = get_elapsed step_timer
          val profile = D.W.get_profile world
      in
          D.W.set_profile (world,
                           {step = step_time,
                            collide = collide_time,
                            solve = solve_time,
                            solve_toi = toi_time })
      end

  end (* World *)

end
