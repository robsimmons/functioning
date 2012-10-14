structure BDDPrismaticJoint =
struct

open BDDTypes
open BDDMath
open BDDOps
infix 6 :+: :-: %-% %+% +++
infix 7 *: *% +*: +*+ #*% @*:

structure D = BDDDynamics

exception PrismaticJoint of string

(* Put this here for now.
  Later, I think we're just going to put everything in joint.sml.
*)
datatype limit_state = InactiveLimit
                     | AtLowerLimit
                     | AtUpperLimit
                     | EqualLimits


fun assert (b : bool) =
    if b then () else raise PrismaticJoint "assertion failed"

fun new {local_anchor_a : BDDMath.vec2,
         local_anchor_b : BDDMath.vec2,
         local_axis_a : BDDMath.vec2,
         reference_angle : real,
         enable_limit : bool,
         lower_translation : real,
         upper_translation : real,
         enable_motor : bool,
         max_motor_force : real,
         motor_speed : real
        }
        joint =
    let
        val bA = D.J.get_body_a joint
        val bB = D.J.get_body_b joint
        val m_localAnchorA = local_anchor_a
        val m_localAnchorB = local_anchor_b
        val m_localXAxisA = vec2normalized local_axis_a
        val m_localYAxisA = cross2sv(1.0, m_localXAxisA)
        val m_referenceAngle = reference_angle
        val m_impulse = ref (vec3 (0.0, 0.0, 0.0))
        val m_motorMass = ref 0.0
        val m_motorImpulse = ref 0.0
        val m_lowerTranslation = lower_translation
        val m_upperTranslation = upper_translation
        val m_maxMotorForce = max_motor_force
        val m_enableLimit = ref enable_limit
        val m_enableMotor = ref enable_motor
        val m_limitState = ref InactiveLimit
        val m_axis = ref (vec2 (0.0, 0.0))
        val m_perp = ref (vec2 (0.0, 0.0))

        val m_localCenterA = ref (vec2 (0.0, 0.0))
        val m_localCenterB = ref (vec2 (0.0, 0.0))
        val m_invMassA = ref 0.0
        val m_invMassB = ref 0.0
        val m_invIA = ref 0.0
        val m_invIB = ref 0.0

        val m_a1 = ref 0.0
        val m_a2 = ref 0.0
        val m_s1 = ref 0.0
        val m_s2 = ref 0.0

        val m_K = ref (mat33with (0.0, 0.0, 0.0,
                                  0.0, 0.0, 0.0,
                                  0.0, 0.0, 0.0))

        fun init_velocity_constraints { dt,
                                        inv_dt,
                                        dt_ratio,
                                        velocity_iterations,
                                        position_iterations,
                                        warm_starting
                                      } =
            let
                val () = m_localCenterA := sweeplocalcenter (D.B.get_sweep bA)
                val () = m_localCenterB := sweeplocalcenter (D.B.get_sweep bB)
                val () = m_invMassA := D.B.get_inv_mass bA
                val () = m_invMassB := D.B.get_inv_mass bB
                val () = m_invIA := D.B.get_inv_i bA
                val () = m_invIB := D.B.get_inv_i bB

                val cA = sweepc (D.B.get_sweep bA)
                val aA = sweepa (D.B.get_sweep bA)
                val vA = ref (D.B.get_linear_velocity bA)
                val wA = ref (D.B.get_angular_velocity bA)

                val cB = sweepc (D.B.get_sweep bB)
                val aB = sweepa (D.B.get_sweep bB)
                val vB = ref (D.B.get_linear_velocity bB)
                val wB = ref (D.B.get_angular_velocity bB)

                val qA = mat22angle aA
                val qB = mat22angle aB

                (* Compute the effective masses. *)
                val rA = qA +*: (m_localAnchorA :-: !m_localCenterA)
                val rB = qB +*: (m_localAnchorB :-: !m_localCenterB)
                val d = (cB :-: cA) :+: rB :-: rA

                val mA = !m_invMassA
                val mB = !m_invMassB
                val iA = !m_invIA
                val iB = !m_invIB

               (* Compute motor Jacobian and effective mass. *)
                val () = m_axis := qA +*: m_localXAxisA
                val () = m_a1 := cross2vv (d :+: rA, !m_axis)
                val () = m_a2 := cross2vv (rB, !m_axis)
                val () = m_motorMass :=
                         mA + mB + iA  * !m_a1 * !m_a1 + iB * !m_a2 * !m_a2
                val () = if !m_motorMass > 0.0
                         then m_motorMass := 1.0 / !m_motorMass
                         else ()

                (* Prismatic Constraint. *)
                val () = m_perp := qA +*: m_localYAxisA
                val () = m_s1 := cross2vv (d :+: rA, !m_perp)
                val () = m_s2 := cross2vv (rB, !m_perp)

                val k11 = mA + mB + iA * !m_s1 * !m_s1 + iB * !m_s2 * !m_s2
                val k12 = iA * !m_s1 + iB * !m_s2
                val k13 = iA * !m_s1 * !m_a1 + iB * !m_s2 * !m_a2
                val k22 = iA + iB
            (*
	     if (k22 == 0.0f)
		    {
		     // For bodies with fixed rotation.
                      k22 = 1.0f;
		    }
             *)
                val k23 = iA * !m_a1 + iB * !m_a2
                val k33 = mA + mB + iA * !m_a1 * !m_a1 + iB * !m_a2 * !m_a2

                val () = m_K := mat33with
                         (k11, k12, k13,
                          k12, k22, k23,
                          k13, k23, k33)

                fun zero_impulse_z () = m_impulse := vec3 (vec3x (!m_impulse),
                                                           vec3y (!m_impulse),
                                                           0.0)

                (* Compute motor and limit terms. *)
                val () = if !m_enableLimit
                         then
                             let val jointTranslation = dot2 (!m_axis, d)
                             in if abs (m_upperTranslation - m_lowerTranslation)
                                    < 2.0 * BDDSettings.linear_slop
                                then m_limitState := EqualLimits
                                else if jointTranslation <= m_lowerTranslation
                                then if !m_limitState <> AtLowerLimit
                                     then (m_limitState := AtLowerLimit;
                                           zero_impulse_z ())
                                     else ()
                                else if jointTranslation >= m_upperTranslation
                                then if !m_limitState <> AtUpperLimit
                                     then (m_limitState := AtUpperLimit;
                                           zero_impulse_z ())
                                     else ()
                                else (m_limitState := InactiveLimit;
                                      zero_impulse_z ()
                                     )
                             end
                         else (m_limitState := InactiveLimit;
                               zero_impulse_z ()
                              )
                val () = if not (!m_enableMotor)
                         then m_motorImpulse := 0.0
                         else ()

                val () = if warm_starting
                         then
                             let (* Account for variable time step. *)
                                 val () = m_impulse := dt_ratio *% !m_impulse
                                 val () = m_motorImpulse := !m_motorImpulse * dt_ratio
                                 val (ix, iy, iz) = (vec3x (!m_impulse),
                                                     vec3y (!m_impulse),
                                                     vec3z (!m_impulse))
                                 val P =
                                     ix *: !m_perp :+:
                                     (!m_motorImpulse + iz) *: !m_axis
                                 val LA =
                                     ix * !m_s1 + iy + (!m_motorImpulse + iz) * !m_a1
                                 val LB =
                                     ix * !m_s2 + iy + (!m_motorImpulse + iz) * !m_a2
                             in vA := !vA :-: mA *: P;
                                wA := !wA - iA * LA;
                                vB := !vB :+: mB *: P;
                                wB := !wB + iB * LB
                             end
                         else (zero_impulse_z ();
                               m_motorImpulse := 0.0)

            in D.B.set_linear_velocity (bA, !vA);
               D.B.set_angular_velocity (bA, !wA);
               D.B.set_linear_velocity (bB, !vB);
               D.B.set_angular_velocity (bB, !wB)
            end


        fun solve_velocity_constraints { dt,
                                         inv_dt,
                                         dt_ratio,
                                         velocity_iterations,
                                         position_iterations,
                                         warm_starting
                                       } =
            let
            in ()
            end

        fun solve_position_constraints baumgarte =
            let
            in true
            end

        fun get_anchor_a () = D.B.get_world_point (bA, m_localAnchorA)

        fun get_anchor_b () = D.B.get_world_point (bB, m_localAnchorB)

        val dispatch =
        {
          init_velocity_constraints = init_velocity_constraints,
          solve_velocity_constraints = solve_velocity_constraints,
          solve_position_constraints = solve_position_constraints,
          get_anchor_a = get_anchor_a,
          get_anchor_b = get_anchor_b
        }

        val methods = BDDDynamicsTypes.Prismatic ()

    in
        (dispatch, methods)
    end

end
