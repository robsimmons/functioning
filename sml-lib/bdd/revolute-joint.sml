structure BDDRevoluteJoint =
struct

open BDDTypes
open BDDMath
open BDDOps
infix 6 :+: :-: %-% %+% +++
infix 7 *: *% +*: +*+ #*% @*:

structure D = BDDDynamics

exception RevoluteJoint of string

(* Out this here for now.
  Leter, I think we're just going to put everything in joint.sml.
*)
datatype limit_state = InactiveLimit
                     | AtLowerLimit
                     | AtUpperLimit
                     | EqualLimits


fun assert (b : bool) =
    if b then () else raise RevoluteJoint "assertion failed"

fun new {local_anchor_a : vec2,
         local_anchor_b : vec2,
         reference_angle : real,
         lower_angle : real,
         upper_angle : real,
         max_motor_torque : real,
         motor_speed : real,
         enable_limit : bool,
         enable_motor : bool
        }
        joint =
    let
        val m_body_a = D.J.get_body_a joint
        val m_body_b = D.J.get_body_b joint
        val m_local_anchor_a = local_anchor_a
        val m_local_anchor_b = local_anchor_b
        val m_local_center_a = ref (vec2 (0.0, 0.0))
        val m_local_center_b = ref (vec2 (0.0, 0.0))
        val m_reference_angle = reference_angle
        val m_impulse = ref (vec3 (0.0, 0.0, 0.0))
        val m_motor_impulse = ref 0.0
        val m_lower_angle = ref lower_angle
        val m_upper_angle = ref upper_angle
        val m_max_motor_torque = max_motor_torque
        val m_motor_speed = ref motor_speed
        val m_enable_limit = ref enable_limit
        val m_enable_motor = ref enable_motor
        val m_limit_state = ref InactiveLimit
        val m_mass = ref (mat33with (0.0, 0.0, 0.0,
                                     0.0, 0.0, 0.0,
                                     0.0, 0.0, 0.0))
        val m_motor_mass = ref 0.0

        val m_rA = ref (vec2 (0.0, 0.0))
        val m_rB = ref (vec2 (0.0, 0.0))

        fun init_velocity_constraints { dt,
                                        inv_dt,
                                        dt_ratio,
                                        velocity_iterations,
                                        position_iterations,
                                        warm_starting
                                      } =
            let
                val bA = m_body_a
                val bB = m_body_b
                val () = if !m_enable_motor orelse !m_enable_limit
		(* You cannot create a rotation limit between bodies that
		   both have fixed rotation. *)
                         then assert (D.B.get_inv_i bA > 0.0 orelse
                                      D.B.get_inv_i bB > 0.0)
                         else ()
                val () = m_local_center_a := sweeplocalcenter (D.B.get_sweep bA)
                val () = m_local_center_b := sweeplocalcenter (D.B.get_sweep bB)
                val aA = sweepa (D.B.get_sweep bA)
                val aB = sweepa (D.B.get_sweep bB)

                val qA = mat22angle aA
                val qB = mat22angle aB
                val () = m_rA := qA +*: (m_local_anchor_a :-: !m_local_center_a)
                val () = m_rB := qB +*: (m_local_anchor_b :-: !m_local_center_b)

	(* J = [-I -r1_skew I r2_skew]
	       [ 0       -1 0       1]
	   r_skew = [-ry; rx] *)

	(* K = [ mA+r1y^2*iA+mB+r2y^2*iB,  -r1y*iA*r1x-r2y*iB*r2x,          -r1y*iA-r2y*iB]
	     [  -r1y*iA*r1x-r2y*iB*r2x, mA+r1x^2*iA+mB+r2x^2*iB,           r1x*iA+r2x*iB]
	     [          -r1y*iA-r2y*iB,           r1x*iA+r2x*iB,                   iA+iB] *)

                val mA = D.B.get_inv_mass bA
                val mB = D.B.get_inv_mass bB
                val iA = D.B.get_inv_i bA
                val iB = D.B.get_inv_i bB

                val (rax, ray) = vec2xy (!m_rA)
                val (rbx, rby) = vec2xy (!m_rB)

                val eyx = ~ray * rax * iA - rby * rbx * iB
                val ezx = ~ray * iA - rby * iB
                val ezy = rax * iA + rbx * iB

                val () = m_mass :=
                    (mat33with (mA + mB + ray * ray * iA + rby * rby * iB,
                                eyx,
                                ezx,
                                eyx,
                                mA + mB + rax * rax * iA + rbx * rbx * iB,
                                ezy,
                                ezx,
                                ezy,
                                iA + iB
                    ))

                val () = m_motor_mass := iA + iB
                val () = if !m_motor_mass > 0.0
                         then m_motor_mass := 1.0 / !m_motor_mass
                         else ()

                val () = if !m_enable_motor = false (* or fixed_rotation? *)
                         then m_motor_impulse := 0.0
                         else ()

                val () = if !m_enable_limit (* and fixed_rotation = false? *)
                         then let val joint_angle = aB - aA - m_reference_angle
                                  val (ix, iy) = (vec3x (!m_impulse), vec3y (!m_impulse))
                                  val () = if abs (!m_upper_angle - !m_lower_angle)
                                              < 2.0 * BDDSettings.angular_slop
                                           then m_limit_state := EqualLimits
                                           else if joint_angle <= !m_lower_angle
                                           then (if !m_limit_state <> AtLowerLimit
                                                 then m_impulse := vec3 (ix, iy, 0.0)
                                                 else ();
                                                 m_limit_state := AtLowerLimit
                                               )
                                           else if joint_angle >= !m_upper_angle
                                           then (if !m_limit_state <> AtUpperLimit
                                                 then m_impulse := vec3 (ix, iy, 0.0)
                                                 else ();
                                                 m_limit_state := AtUpperLimit
                                                )
                                           else (m_limit_state := InactiveLimit;
                                                 m_impulse := vec3 (ix, iy, 0.0))

                              in () end
                         else m_limit_state := InactiveLimit

                val () = if warm_starting
                         then let
                                 (* Scale impulses to support a variable time step. *)
                                  val () = m_impulse := dt_ratio *% !m_impulse
                                  val () = m_motor_impulse := !m_motor_impulse * dt_ratio;
                                  val p = vec2 (vec3x (!m_impulse), vec3y (!m_impulse))
                                  val () = D.B.set_linear_velocity
                                           (bA,
                                            D.B.get_linear_velocity bA :-: mA *: p)
                                  val () = D.B.set_angular_velocity
                                           (bA,
                                            D.B.get_angular_velocity bA -
                                            iA * (cross2vv (!m_rA, p) + !m_motor_impulse
                                                   + (vec3z (!m_impulse))))
                                  val () = D.B.set_linear_velocity
                                           (bB,
                                            D.B.get_linear_velocity bB :+: mB *: p)
                                  val () = D.B.set_angular_velocity
                                           (bB,
                                            D.B.get_angular_velocity bB +
                                            iB * (cross2vv (!m_rB, p) + !m_motor_impulse
                                                   + (vec3z (!m_impulse))))
                              in () end
                         else (m_impulse := vec3 (0.0, 0.0, 0.0);
                               m_motor_impulse := 0.0)

            in ()
            end


        fun solve_velocity_constraints { dt,
                                         inv_dt,
                                         dt_ratio,
                                         velocity_iterations,
                                         position_iterations,
                                         warm_starting
                                       } =
            let
                val bA = m_body_a
                val bB = m_body_b

                val vA = ref (D.B.get_linear_velocity bA)
                val wA = ref (D.B.get_angular_velocity bA)
                val vB = ref (D.B.get_linear_velocity bB)
                val wB = ref (D.B.get_angular_velocity bB)

                val mA = D.B.get_inv_mass bA
                val mB = D.B.get_inv_mass bB
                val iA = D.B.get_inv_i bA
                val iB = D.B.get_inv_i bB

                (*bool fixedRotation = (iA + iB == 0.0f); *)

                (* Solve motor constraint *)
                val () = if (!m_enable_motor andalso !m_limit_state <> EqualLimits)
                         then
                             let val Cdot = !wB - !wA - !m_motor_speed
                                 val impulse = ~(!m_motor_mass) * Cdot
                                 val old_impulse = !m_motor_impulse
                                 val max_impulse = dt * m_max_motor_torque
                                 val () = m_motor_impulse :=
                                          clampr (!m_motor_impulse + impulse,
                                                  ~max_impulse,
                                                  max_impulse)
                                 val impulse = !m_motor_impulse - old_impulse
                                 val () = wA := !wA - iA * impulse
                                 val () = wB := !wB + iB * impulse
                             in () end
                         else ()

                (* Solve limit constraint. *)
                val () = if !m_enable_limit andalso !m_limit_state <> InactiveLimit
                         then
                             let val Cdot1 = !vB :+: cross2sv (!wB, !m_rB)
                                             :-: !vA :-: cross2sv (!wA, !m_rA)
                                 val Cdot2 = !wB - !wA
                                 val Cdot = vec3 (vec2x Cdot1, vec2y Cdot1, Cdot2)
                                 val impulse = ref (~1.0 *% mat33solve33 (!m_mass, Cdot))
                                 fun update_impulses cond =
                                     if cond
                                     then let
                                             (* 2010 Box2D only has -Cdot1 here *)
                                             val rhs =
                                                 (~1.0) *: Cdot1 :+:
                                                        vec3z (!m_impulse) *:
                                                        vec2(vec3x (mat33col3 (!m_mass)),
                                                             vec3y (mat33col3 (!m_mass))
                                                            )
                                             val reduced = mat33solve22 (!m_mass, rhs)
                                             val () = impulse :=
                                                      vec3 (vec2x reduced,
                                                            vec2y reduced,
                                                            ~ (vec3z (!m_impulse)))
                                             val () = m_impulse :=
                                                      vec3 (vec3x (!m_impulse) +
                                                            vec2x reduced,
                                                            vec3y (!m_impulse) +
                                                            vec2y reduced,
                                                            0.0)
                                         in () end
                                     else m_impulse := !m_impulse %+% !impulse
                                 val () =
                                     case !m_limit_state of
                                         EqualLimits =>
                                           m_impulse := !m_impulse %+% !impulse
                                       | AtLowerLimit =>
                                           update_impulses
                                               (vec3z (!m_impulse) + vec3z (!impulse) < 0.0)
                                       | AtUpperLimit =>
                                           update_impulses
                                               (vec3z (!m_impulse) + vec3z (!impulse) > 0.0)
                                       | _ => ()
                                 val P = vec2 (vec3x (!impulse), vec3y (!impulse))
                                 val () = vA := !vA :-: mA *: P
                                 val () = wA := !wA - iA *
                                                      (cross2vv (!m_rA, P) + vec3z (!impulse))
                                 val () = vB := !vB :+: mB *: P
                                 val () = wB := !wB + iB *
                                                      (cross2vv (!m_rB, P) + vec3z (!impulse))
                             in () end
                         else
                             (* Solve point-to-point constraint *)
                             let val Cdot = !vB :+: cross2sv (!wB, !m_rB)
                                            :-: !vA :-: cross2sv (!wA, !m_rA)
                                 val impulse = mat33solve22 (!m_mass, ~1.0 *: Cdot)
                                 val () = m_impulse :=
                                          vec3 (vec3x (!m_impulse) + vec2x impulse,
                                                vec3y (!m_impulse) + vec2y impulse,
                                                vec3z (!m_impulse))
                                 val () = vA := !vA :-: mA *: impulse
                                 val () = wA := !wA - iA *
                                                      (cross2vv (!m_rA, impulse))
                                 val () = vB := !vB :+: mB *: impulse
                                 val () = wB := !wB + iB *
                                                      (cross2vv (!m_rB, impulse))
                             in () end

            in D.B.set_linear_velocity (bA, !vA);
               D.B.set_angular_velocity (bA, !wA);
               D.B.set_linear_velocity (bB, !vB);
               D.B.set_angular_velocity (bB, !wB)
            end

        fun solve_position_constraints baumgarte =
            let
                val bA = m_body_a
                val bB = m_body_b

                val aA = ref (sweepa (D.B.get_sweep bA))
                val cA = ref (sweepc (D.B.get_sweep bA))
                val aB = ref (sweepa (D.B.get_sweep bB))
                val cB = ref (sweepc (D.B.get_sweep bB))

                val iA = D.B.get_inv_i bA
                val iB = D.B.get_inv_i bB

                val angularError = ref 0.0

                (* Solve angular limit constraint. *)
                val () =
                    if !m_enable_limit andalso !m_limit_state <> InactiveLimit
                    then let val angle = !aB - !aA - m_reference_angle
                             val limitImpulse =
                                 case !m_limit_state of
                                     EqualLimits =>
                                     let (* Prevent large angular correction *)
                                         val C = clampr (angle - !m_lower_angle,
                                                         ~BDDSettings.max_angular_correction,
                                                         BDDSettings.max_angular_correction)
                                         val () = angularError := abs C
                                     in ~(!m_motor_mass) * C end
                                   | AtLowerLimit =>
                                     let
                                         val C = angle - !m_lower_angle
                                         val () = angularError := ~C
                                         (* Prevent large angular corrections and allow
                                            some slop. *)
                                         val C1 = clampr (C + BDDSettings.angular_slop,
                                                          ~BDDSettings.max_angular_correction,
                                                          0.0)
                                     in ~(!m_motor_mass) * C1 end
                                   | AtUpperLimit =>
                                     let
                                         val C = angle - !m_upper_angle
                                         val () = angularError := C
                                         (* Prevent large angular corrections and allow
                                            some slop. *)
                                         val C1 = clampr (C - BDDSettings.angular_slop,
                                                          0.0,
                                                          BDDSettings.max_angular_correction)
                                     in ~(!m_motor_mass) * C1 end
                                   | _ => 0.0
                             val () = aA := !aA - iA * limitImpulse
                             val () = aB := !aB + iB * limitImpulse
                         in () end
                    else ()
                (* Solve point-to-point constraint. *)
                val qA = mat22angle (!aA)
                val qB = mat22angle (!aB)
                val rA = qA +*: (m_local_anchor_a :-: !m_local_center_a)
                val rB = qB +*: (m_local_anchor_b :-: !m_local_center_b)
                val C = !cB :+: rB :-: !cA :-: rA
                val positionError = vec2length C
                val mA = D.B.get_inv_mass bA
                val mB = D.B.get_inv_mass bB

                val (rax, ray) = vec2xy rA
                val (rbx, rby) = vec2xy rB

                val K = mat22with
                        (mA + mB + iA * ray * ray + iB * rby * rby,
                         ~iA * rax * ray - iB * rbx * rby,
                         ~iA * rax * ray - iB * rbx * rby,
                         mA + mB + iA * rax * rax + iB * rbx * rbx
                        )
                val impulse = ~1.0 *: mat22solve (K, C)
                val () = cA := !cA :-: mA *: impulse
                val () = aA := !aA - iA * cross2vv (rA, impulse)
                val () = cB := !cB :+: mB *: impulse
                val () = aB := !aB + iB * cross2vv (rB, impulse)

                val sweepA = D.B.get_sweep bA
                val sweepB = D.B.get_sweep bB

                val () = sweep_set_a (sweepA, !aA)
                val () = sweep_set_a (sweepB, !aB)
                val () = sweep_set_c (sweepA, !cA)
                val () = sweep_set_c (sweepB, !cB)

                (* Do we need these? *)
                val () = D.B.synchronize_transform bA
                val () = D.B.synchronize_transform bB
            in positionError <= BDDSettings.linear_slop andalso
               !angularError <= BDDSettings.angular_slop
            end

        fun get_anchor_a () = D.B.get_world_point (m_body_a, m_local_anchor_a)

        fun get_anchor_b () = D.B.get_world_point (m_body_b, m_local_anchor_b)

        fun enable_limit flag =
            (D.B.set_awake (m_body_a, true);
             D.B.set_awake (m_body_b, true);
             m_enable_limit := flag
            )

        fun is_limit_enabled () = !m_enable_limit


        val dispatch =
        {
          init_velocity_constraints = init_velocity_constraints,
          solve_velocity_constraints = solve_velocity_constraints,
          solve_position_constraints = solve_position_constraints,
          get_anchor_a = get_anchor_a,
          get_anchor_b = get_anchor_b
        }

        val methods = BDDDynamicsTypes.Revolute {enable_limit = enable_limit,
                                                 is_limit_enabled = is_limit_enabled}

    in
        (dispatch, methods)
    end

end
