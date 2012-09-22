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
          damping_ratio : real
         }
        joint =
    let
        val body_b = D.J.get_body_b joint

        val target_ref = ref target
        val max_force_ref = ref max_force
        val frequency_hz_ref = ref frequency_hz
        val damping_ratio_ref = ref damping_ratio
        val local_anchor = (D.B.get_xf body_b) @*: (target)
        val impulse = ref (vec2 (0.0, 0.0))

        val beta = ref 0.0
        val gamma = ref 0.0

        val effective_mass = ref (mat22with (0.0, 0.0, 0.0, 0.0))

        fun init_velocity_constraints { dt,
                                        inv_dt,
                                        dt_ratio,
                                        velocity_iterations,
                                        position_iterations,
                                        warm_starting
                                      } =
            let
                val mass = D.B.get_mass body_b
                (* Frequency *)
                val omega = 2.0 * Math.pi * !frequency_hz_ref
                (* Damping coefficient *)
                val d = 2.0 * mass * !damping_ratio_ref * omega
                (* Spring stiffness *)
                val k = mass * omega * omega

	        (* magic formulas
                   gamma has units of inverse mass.
                   beta has units of inverse time. *)
                (* b2Assert(d + step.dt * k > b2_epsilon); *)
                val () = gamma := dt * k * !gamma
                (* Port note: original test is gamma != 0.0 *)
                val () = if (!gamma > 0.0)
                         then gamma := 1.0 / !gamma
                         else ()
                val () = beta := dt * k * !gamma

                val r = (transformr (D.B.get_xf body_b))
                            +*:
                            (local_anchor :-: sweeplocalcenter (D.B.get_sweep body_b))
(* K = [(1/m1 + 1/m2) * eye(2) - skew(r1) * invI1 * skew(r1) - skew(r2) * invI2 * skew(r2)]
 = [1/m1+1/m2     0    ] + invI1 * [r1.y*r1.y -r1.x*r1.y] + invI2 * [r1.y*r1.y -r1.x*r1.y]
[    0     1/m1+1/m2]           [-r1.x*r1.y r1.x*r1.x]           [-r1.x*r1.y r1.x*r1.x] *)
                val inv_mass = D.B.get_inv_mass body_b
                val inv_i = D.B.get_inv_i body_b

                val K1 = mat22cols
                         (vec2 (inv_mass, 0.0),
                          vec2 (0.0, inv_mass))

                val (rx, ry) = vec2xy r
                val K2 = mat22cols
                         (vec2 (inv_i * ry * ry, ~inv_i * rx * ry),
                          vec2 (~inv_i * rx * ry, inv_i * rx * rx))

                val K3 = mat22cols
                         (vec2 (!gamma, 0.0),
                          vec2 (0.0, !gamma))

                val K = K1 +++ K2 +++ K3

                val () = effective_mass := mat22inverse K

                (* Cheat with some damping *)
                val () = D.B.set_angular_velocity (body_b,
                                                   0.98 * D.B.get_angular_velocity body_b)

                (* Warm starting. *)
                val () = impulse := dt_ratio *: !impulse
                val () = D.B.set_linear_velocity (body_b,
                                                  inv_mass *: !impulse)
                val () = D.B.set_angular_velocity (body_b,
                                                   inv_i * (cross2vv (r, !impulse)))
            in ()
            end


        fun solve_velocity_constraints ts = ()
        fun solve_position_constraints ts = ()

    in
        { init_velocity_constraints = init_velocity_constraints,
          solve_velocity_constraints = solve_velocity_constraints,
          solve_position_constraints = solve_position_constraints
        }
    end

end
