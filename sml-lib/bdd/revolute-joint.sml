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

        fun init_velocity_constraints { dt,
                                        inv_dt,
                                        dt_ratio,
                                        velocity_iterations,
                                        position_iterations,
                                        warm_starting
                                      } =
            let
                val b_a = m_body_a
                val b_b = m_body_b
                val () = if !m_enable_motor orelse !m_enable_limit
		(* You cannot create a rotation limit between bodies that
		   both have fixed rotation. *)
                         then assert (D.B.get_inv_i b_a > 0.0 orelse
                                      D.B.get_inv_i b_b > 0.0)
                         else ()
                val local_center_a = sweeplocalcenter (D.B.get_sweep b_a)
                val local_center_b = sweeplocalcenter (D.B.get_sweep b_b)
                val a_a = sweepa (D.B.get_sweep b_a)
                val a_b = sweepa (D.B.get_sweep b_b)

                val q_a = transformr (D.B.get_xf b_a)
                val q_b = transformr (D.B.get_xf b_b)
                val r_a = q_a +*: m_local_anchor_a :-: local_center_a
                val r_b = q_b +*: m_local_anchor_b :-: local_center_b

	(* J = [-I -r1_skew I r2_skew]
	       [ 0       -1 0       1]
	   r_skew = [-ry; rx] *)

	(* K = [ mA+r1y^2*iA+mB+r2y^2*iB,  -r1y*iA*r1x-r2y*iB*r2x,          -r1y*iA-r2y*iB]
	     [  -r1y*iA*r1x-r2y*iB*r2x, mA+r1x^2*iA+mB+r2x^2*iB,           r1x*iA+r2x*iB]
	     [          -r1y*iA-r2y*iB,           r1x*iA+r2x*iB,                   iA+iB] *)

                val m_a = D.B.get_inv_mass b_a
                val m_b = D.B.get_inv_mass b_b
                val i_a = D.B.get_inv_i b_a
                val i_b = D.B.get_inv_i b_b

                val (rax, ray) = vec2xy r_a
                val (rbx, rby) = vec2xy r_b

                val () = m_mass :=
                    (mat33with (m_a + m_b + ray * ray * i_a * rby * rby * i_b,
                                ~ray * rax * i_a - rby * rbx * i_b,
                                ~ray * i_a - rby * i_b,
                                vec3x (mat33col2 (!m_mass)),
                                m_a + m_b + rax * rax * i_a + rbx * rbx * i_b,
                                rax * i_a + rbx * i_b,
                                vec3x (mat33col3 (!m_mass)),
                                vec3y (mat33col3 (!m_mass)),
                                i_a + i_b
                    ))

                val () = m_motor_mass := i_a + i_b
                val () = if !m_motor_mass > 0.0
                         then m_motor_mass := 1.0 / !m_motor_mass
                         else ()

                val () = if !m_enable_motor = false (* or fixed_rotation? *)
                         then m_motor_impulse := 0.0
                         else ()

                val () = if !m_enable_limit (* and fixed_rotation = false? *)
                         then let val joint_angle = a_b - a_a - m_reference_angle
                                  val (ix, iy) = (vec3x (!m_impulse), vec3y (!m_impulse))
                                  val () = if Real.abs (!m_upper_angle - !m_lower_angle)
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
                                  val () = vec3timeseq (!m_impulse, dt_ratio);
                                  val () = m_motor_impulse := !m_motor_impulse * dt_ratio;
                                  val p = vec2 (vec3x (!m_impulse), vec3y (!m_impulse))
                                  val () = D.B.set_linear_velocity
                                           (b_a,
                                            D.B.get_linear_velocity b_a :-: m_a *: p)
                                  val () = D.B.set_angular_velocity
                                           (b_a,
                                            D.B.get_angular_velocity b_a -
                                            i_a * (cross2vv (r_a, p) + !m_motor_impulse
                                                   + (vec3z (!m_impulse))))
                                  val () = D.B.set_linear_velocity
                                           (b_b,
                                            D.B.get_linear_velocity b_b :-: m_b *: p)
                                  val () = D.B.set_angular_velocity
                                           (b_b,
                                            D.B.get_angular_velocity b_b -
                                            i_b * (cross2vv (r_b, p) + !m_motor_impulse
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
            in ()
            end

        fun solve_position_constraints ts = true

        fun get_anchor_a () = D.B.get_world_point (m_body_a, m_local_anchor_a)

        fun get_anchor_b () = D.B.get_world_point (m_body_b, m_local_anchor_b)


        val dispatch =
        {
          init_velocity_constraints = init_velocity_constraints,
          solve_velocity_constraints = solve_velocity_constraints,
          solve_position_constraints = solve_position_constraints,
          get_anchor_a = get_anchor_a,
          get_anchor_b = get_anchor_b
        }

        val methods = BDDDynamicsTypes.Revolute ()

    in
        (dispatch, methods)
    end

end
