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
        val m_impulse = ref (vec3zero)
        val m_motorMass = 0.0
        val m_motorImpulse = ref 0.0
        val m_lowerTranslation = lower_translation
        val m_upperTranslation = upper_translation
        val m_maxMotorForce = max_motor_force
        val m_enableLimit = ref enable_limit
        val m_enableMotor = ref enable_motor
        val m_limitState = ref InactiveLimit
        val m_axis = vec2 (0.0, 0.0)
        val m_perp = vec2 (0.0, 0.0)

        fun init_velocity_constraints { dt,
                                        inv_dt,
                                        dt_ratio,
                                        velocity_iterations,
                                        position_iterations,
                                        warm_starting
                                      } =
            let
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
