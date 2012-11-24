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
        val m_bodyB = D.J.get_body_b joint
        val m_indexB = ref 0
        val m_target = ref target
        val m_max_force = ref max_force
        val m_frequency_hz = ref frequency_hz
        val m_damping_ratio = ref damping_ratio
        val m_local_anchor = mul_ttransformv (D.B.get_xf m_bodyB, target)
        val m_impulse = ref (vec2 (0.0, 0.0))

        val m_beta = ref 0.0
        val m_gamma = ref 0.0

        val m_localCenterB = ref (vec2 (0.0, 0.0))
        val m_rB = ref (vec2 (0.0, 0.0))
        val m_invMassB = ref 0.0
        val m_invIB = ref 0.0

        val m_C = ref (vec2 (0.0, 0.0))
        val m_mass = ref (mat22with (0.0, 0.0, 0.0, 0.0))

        fun init_velocity_constraints { step : BDDDynamicsTypes.time_step,
                                        positionsc,
                                        positionsa,
                                        velocitiesv,
                                        velocitiesw } =
            let
                val () = m_indexB := D.B.get_island_index m_bodyB
                val () = m_localCenterB := sweeplocalcenter (D.B.get_sweep m_bodyB)
                val () = m_invMassB := D.B.get_inv_mass m_bodyB
                val () = m_invIB := D.B.get_inv_i m_bodyB

                val cB = Array.sub(positionsc, !m_indexB)
                val aB = Array.sub(positionsa, !m_indexB)
                val vB = ref (Array.sub(velocitiesv, !m_indexB))
                val wB = ref (Array.sub(velocitiesw, !m_indexB))

                val qB = mat22angle aB

                val mass = D.B.get_mass m_bodyB

                (* Frequency *)
                val omega = 2.0 * Math.pi * !m_frequency_hz

                (* Damping coefficient *)
                val d = 2.0 * mass * !m_damping_ratio * omega

                (* Spring stiffness *)
                val k = mass * omega * omega

                val {dt, dt_ratio, warm_starting, ...} = step

	        (* magic formulas
                   gamma has units of inverse mass.
                   beta has units of inverse time. *)
                val () = assert (d + dt * k > BDDSettings.epsilon)

                val () = m_gamma := dt * (d + dt * k)

                (* Port note: original test is gamma != 0.0 *)
                val () = if (!m_gamma > 0.0)
                         then m_gamma := 1.0 / !m_gamma
                         else ()
                val () = m_beta := dt * k * !m_gamma

                (* Compute the effective mass matrix. *)
                val () = m_rB := (qB +*: (m_local_anchor :-: (!m_localCenterB)))

(* K = [(1/m1 + 1/m2) * eye(2) - skew(r1) * invI1 * skew(r1) - skew(r2) * invI2 * skew(r2)]
 = [1/m1+1/m2     0    ] + invI1 * [r1.y*r1.y -r1.x*r1.y] + invI2 * [r1.y*r1.y -r1.x*r1.y]
[    0     1/m1+1/m2]           [-r1.x*r1.y r1.x*r1.x]           [-r1.x*r1.y r1.x*r1.x] *)
                val inv_mass = !m_invMassB
                val inv_i = !m_invIB

                val K1 = mat22cols
                         (vec2 (inv_mass, 0.0),
                          vec2 (0.0, inv_mass))

                val (rx, ry) = vec2xy (!m_rB)
                val K2 = mat22cols
                         (vec2 (inv_i * ry * ry, ~inv_i * rx * ry),
                          vec2 (~inv_i * rx * ry, inv_i * rx * rx))

                val K3 = mat22cols
                         (vec2 (!m_gamma, 0.0),
                          vec2 (0.0, !m_gamma))

                val K = K1 +++ K2 +++ K3

                val () = m_mass := mat22inverse K

                val () = m_C := !m_beta *: (cB :+: !m_rB :-: !m_target)

                (* Cheat with some damping *)
                val () = wB := !wB * 0.98

                val () = if warm_starting
                         then (m_impulse := dt_ratio *: !m_impulse;
                               vB := !vB :+: !m_invMassB *: !m_impulse;
                               wB := !wB + !m_invIB * (cross2vv (!m_rB, !m_impulse))
                              )
                         else m_impulse := vec2 (0.0, 0.0)

            in
                Array.update(velocitiesv, !m_indexB, !vB);
                Array.update(velocitiesw, !m_indexB, !wB)
            end


        fun solve_velocity_constraints { step : BDDDynamicsTypes.time_step,
                                         positionsc,
                                         positionsa,
                                         velocitiesv,
                                         velocitiesw } =
            let
                val vB = Array.sub(velocitiesv, !m_indexB)
                val wB = Array.sub(velocitiesw, !m_indexB)

                (* CDot = v + cross(w, r) *)
                val Cdot = vB :+: cross2sv(wB, !m_rB)
                val impulse = !m_mass +*:
                              (vec2neg (Cdot :+: !m_C :+: !m_gamma *: !m_impulse))

                val old_impulse = !m_impulse
                val () = m_impulse := (!m_impulse :+: impulse)
                val max_impulse = (#dt step) * !m_max_force
                val () = if vec2length_squared (!m_impulse) > max_impulse * max_impulse
                         then m_impulse :=
                              (max_impulse / (vec2length (!m_impulse))) *: !m_impulse
                         else ()
                val impulse = !m_impulse :-: old_impulse
            in
                Array.update(velocitiesv, !m_indexB, vB :+: !m_invMassB *: impulse);
                Array.update(velocitiesw, !m_indexB, wB + !m_invIB * cross2vv (!m_rB, impulse))
            end

        fun solve_position_constraints sd = true

        fun get_anchor_a () = !m_target

        fun get_anchor_b () = D.B.get_world_point (m_bodyB, m_local_anchor)

        fun set_target new_target =
            let val () = D.B.set_awake (m_bodyB, true)
            in
                m_target := new_target
            end

        fun get_target () = !m_target


        val dispatch =
        {
          init_velocity_constraints = init_velocity_constraints,
          solve_velocity_constraints = solve_velocity_constraints,
          solve_position_constraints = solve_position_constraints,
          get_anchor_a = get_anchor_a,
          get_anchor_b = get_anchor_b
        }

        val methods = BDDDynamicsTypes.Mouse {set_target = set_target,
                                              get_target = get_target
                                             }

    in
        (dispatch, methods)
    end

end
