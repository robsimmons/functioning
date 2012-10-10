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
        val m_local_anchor_a = local_anchor_a
        val m_local_anchor_b = local_anchor_b

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

        fun get_anchor_a () = D.B.get_world_point (bA, m_local_anchor_a)

        fun get_anchor_b () = D.B.get_world_point (bB, m_local_anchor_b)

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
