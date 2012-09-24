structure BDDMouseJoint =
struct

open BDDTypes
open BDDMath
open BDDOps
infix 6 :+: :-: %-% %+% +++
infix 7 *: *% +*: +*+ #*% @*:

structure D = BDDDynamics

exception MouseJoint of string

fun new { target : vec2,
          max_force : real,
          frequency_hz : real,
          damping_ratio : real
         }
        joint =
    let
        val body_b = D.J.get_body_b joint

        val m_target = ref target
        val m_max_force = ref max_force
        val m_frequency_hz = ref frequency_hz
        val m_damping_ratio = ref damping_ratio
        val m_local_anchor = mul_ttransformv (D.B.get_xf body_b, target)
        val m_impulse = ref (vec2 (0.0, 0.0))

        val m_beta = ref 0.0
        val m_gamma = ref 0.0

        val m_C = ref (vec2 (0.0, 0.0))
        val m_mass = ref (mat22with (0.0, 0.0, 0.0, 0.0))

        fun init_velocity_constraints { dt,
                                        inv_dt,
                                        dt_ratio,
                                        velocity_iterations,
                                        position_iterations,
                                        warm_starting
                                      } =
            let
                val b = body_b
                val mass = D.B.get_mass b
                (* Frequency *)
                val omega = 2.0 * Math.pi * !m_frequency_hz
                (* Damping coefficient *)
                val d = 2.0 * mass * !m_damping_ratio * omega
                (* Spring stiffness *)
                val k = mass * omega * omega

	        (* magic formulas
                   gamma has units of inverse mass.
                   beta has units of inverse time. *)
                val () = if d + dt * k > BDDSettings.epsilon
                         then ()
                         else raise MouseJoint "Assert Failure"

                val () = m_gamma := dt * (d + dt * k)

                (* Port note: original test is gamma != 0.0 *)
                val () = if (!m_gamma > 0.0)
                         then m_gamma := 1.0 / !m_gamma
                         else ()
                val () = m_beta := dt * k * !m_gamma

                val r = (transformr (D.B.get_xf b))
                            +*:
                            (m_local_anchor :-: sweeplocalcenter (D.B.get_sweep b))
(* K = [(1/m1 + 1/m2) * eye(2) - skew(r1) * invI1 * skew(r1) - skew(r2) * invI2 * skew(r2)]
 = [1/m1+1/m2     0    ] + invI1 * [r1.y*r1.y -r1.x*r1.y] + invI2 * [r1.y*r1.y -r1.x*r1.y]
[    0     1/m1+1/m2]           [-r1.x*r1.y r1.x*r1.x]           [-r1.x*r1.y r1.x*r1.x] *)
                val inv_mass = D.B.get_inv_mass b
                val inv_i = D.B.get_inv_i b

                val K1 = mat22cols
                         (vec2 (inv_mass, 0.0),
                          vec2 (0.0, inv_mass))

                val (rx, ry) = vec2xy r
                val K2 = mat22cols
                         (vec2 (inv_i * ry * ry, ~inv_i * rx * ry),
                          vec2 (~inv_i * rx * ry, inv_i * rx * rx))

                val K3 = mat22cols
                         (vec2 (!m_gamma, 0.0),
                          vec2 (0.0, !m_gamma))

                val K = K1 +++ K2 +++ K3

                val () = m_mass := mat22inverse K

                val () = m_C := (sweepc (D.B.get_sweep b)) :+: r :-: !m_target

                (* Cheat with some damping *)
                val () = D.B.set_angular_velocity (b,
                                                   0.98 * D.B.get_angular_velocity b)

                (* Warm starting. *)
                val () = m_impulse := dt_ratio *: !m_impulse
                val () = D.B.set_linear_velocity
                             (b,
                              D.B.get_linear_velocity b :+: inv_mass *: !m_impulse)
                val () = D.B.set_angular_velocity
                             (b,
                              D.B.get_angular_velocity b +
                              inv_i * (cross2vv (r, !m_impulse)))
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
                val b = body_b
                val r = (transformr (D.B.get_xf b))
                            +*:
                            (m_local_anchor :-: sweeplocalcenter (D.B.get_sweep b))

                (* CDot = v + cross(w, r) *)
                val Cdot = D.B.get_linear_velocity b
                              :+:
                              cross2sv(D.B.get_angular_velocity b, r)
                val impulse = !m_mass +*:
                              (vec2neg (Cdot :+: (!m_beta *: !m_C) :+: !m_gamma *: !m_impulse))

                val old_impulse = !m_impulse
                val () = m_impulse := (!m_impulse :+: impulse)
                val max_impulse = dt * !m_max_force
                val () = if vec2length_squared (!m_impulse) > max_impulse * max_impulse
                         then m_impulse :=
                              (max_impulse / (vec2length (!m_impulse))) *: !m_impulse
                         else ()
                val impulse = !m_impulse :-: old_impulse

                val () = D.B.set_linear_velocity
                         (b, D.B.get_linear_velocity b :+: (D.B.get_inv_mass b) *: impulse)
                val () = D.B.set_angular_velocity
                         (b, D.B.get_angular_velocity b + (D.B.get_inv_i b) *
                                                          (cross2vv (r, impulse)))
            in ()
            end

        fun solve_position_constraints ts = true

        fun get_anchor_a () = !m_target

        fun get_anchor_b () = D.B.get_world_point (body_b, m_local_anchor)

        fun set_target new_target =
            let val () = D.B.set_awake (body_b, true)
            in
                m_target := new_target
            end

        fun get_target () = !m_target

        val methods = BDDDynamicsTypes.MouseMethods {set_target = set_target,
                                                     get_target = get_target}

        val dispatch =
        { specialized_methods = methods,
          init_velocity_constraints = init_velocity_constraints,
          solve_velocity_constraints = solve_velocity_constraints,
          solve_position_constraints = solve_position_constraints,
          get_anchor_a = get_anchor_a,
          get_anchor_b = get_anchor_b
        }

    in
        dispatch
    end

end
