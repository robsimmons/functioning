structure BDDMouseJoint =
struct

open BDDTypes
open BDDMath
open BDDOps
infix 6 :+: :-: %-% %+% +++
infix 7 *: *% +*: +*+ #*% @*:

structure D = BDDDynamics

fun new { target : vec2,
          max_force : real,
          frequency_hz : real,
          damping_ration : real
         }
        joint =
    let
        val body_b = D.J.get_body_b joint

        val m_target = ref target
        val m_max_force = ref max_force
        val local_anchor = (D.B.get_xf body_b) @*: (target)

        fun init_velocity_constraints ts = ()
        fun solve_velocity_constraints ts = ()
        fun solve_position_constraints ts = ()

    in
        { init_velocity_constraints = init_velocity_constraints,
          solve_velocity_constraints = solve_velocity_constraints,
          solve_position_constraints = solve_position_constraints
        }
    end

end
