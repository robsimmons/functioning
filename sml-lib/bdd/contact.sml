(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Implementation of contacts.

   Corresponding to parts of dynamics/contacts/b2contact.cpp. *)
functor BDDContact(Arg :
                   sig
                     type fixture_data
                     type body_data
                     type joint_data
                   end) : BDDCONTACT =
struct
  open Arg
  open BDDSettings
  open BDDTypes
  open BDDMath
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  structure D = BDDDynamics
(*
  datatype bodycell = datatype D.bodycell
  datatype body_type = datatype D.body_type
*)
  structure DT = BDDDynamicsTypes(Arg)
  open DT
  type filter = D.filter

  open D.C

  fun get_world_manifold (c : contact) =
      case D.C.get_manifold c of
          NONE => NONE
        | SOME manifold =>
          let
              val fix_a = D.C.get_fixture_a c
              val fix_b = D.C.get_fixture_b c
              val body_a = D.F.get_body fix_a
              val body_b = D.F.get_body fix_b
              val shape_a = D.F.get_shape fix_a
              val shape_b = D.F.get_shape fix_b
          in
             SOME (BDDCollision.create_world_manifold
                       (manifold,
                        D.B.get_xf body_a, BDDShape.get_radius shape_a,
                        D.B.get_xf body_b, BDDShape.get_radius shape_b))
          end


  (* Update the contact manifold and touching status.
     Note: do not assume the fixture AABBs are overlapping or are valid. *)
  (* Port note: Passing world instead of contact listener, since those
     fields are flattened into world. *)
  fun update (c : contact, world : world) =
    let
      val () = dprint (fn () => "-> Update contact.\n")

      val mbe_old_manifold = get_manifold c
      (* Re-enable this contact. *)
      val () = set_flag (c, FLAG_ENABLED)

      val touching = ref false
      val was_touching = get_flag (c, FLAG_TOUCHING)

      val fixture_a = get_fixture_a c
      val fixture_b = get_fixture_b c
      val sensor_a = D.F.get_sensor fixture_a
      val sensor_b = D.F.get_sensor fixture_b
      val sensor = sensor_a orelse sensor_b

      val body_a = D.F.get_body fixture_a
      val body_b = D.F.get_body fixture_b
      val xf_a = D.B.get_xf body_a
      val xf_b = D.B.get_xf body_b
    in
      dprint (fn () => "  xfa " ^ xftos xf_a ^ " xfb " ^ xftos xf_b ^ "\n");
      (* Is this contact a sensor? *)
      if sensor
      then
          let val shape_a = D.F.get_shape fixture_a
              val shape_b = D.F.get_shape fixture_b
              val manifold = get_manifold c
          in
              touching := BDDCollision.test_overlap(shape_a, shape_b, xf_a, xf_b);
              (* Sensors don't generate manifolds. *)
              set_manifold (c, NONE)
          end
      else
          let
              val mbe_manifold = evaluate (c, xf_a, xf_b)
          in
              set_manifold (c, mbe_manifold);
              case mbe_manifold of
                  NONE => touching := false
                | SOME manifold =>
                  (touching := true;
                   (case mbe_old_manifold of
                        NONE => ()
                      | SOME old_manifold =>
                        (* Match old contact ids to new contact ids and copy the
                           stored impulses to warm start the solver. *)
                        BDDCollision.app_one_or_two
                            (fn p =>
                                let
                                    val { normal_impulse, tangent_impulse, id = id2, ... } = p
                                in
                                    BDDCollision.app_one_or_two
                                        (fn mp1 =>
                                            if #id mp1 = id2
                                            then (normal_impulse := !(#normal_impulse mp1);
                                                  tangent_impulse := !(#tangent_impulse mp1))
                                            else ()) (BDDCollision.manifold_points old_manifold)
                                end) (BDDCollision.manifold_points manifold)
                   ));

              if !touching <> was_touching
              then
                  let in
                      D.B.set_awake (body_a, true);
                      D.B.set_awake (body_b, true)
                  end
              else ()
          end;

      if !touching
      then set_flag (c, FLAG_TOUCHING)
      else clear_flag (c, FLAG_TOUCHING);

      (* Call listeners if state has changed. *)
      (case (was_touching, !touching) of
           (false, true) => D.W.get_begin_contact world c
         | (true, false) => D.W.get_end_contact world c
         | _ => ());

      if not sensor andalso !touching
      then D.W.get_pre_solve world (c, mbe_old_manifold)
      else ();

      dprint (fn () => "<- done updating contact\n")
    end

end
