structure Cradle =
struct

open Types

fun init world =
    let
        val ground_body = BDD.World.create_body (world,
                                                 {typ = BDD.Body.Static,
                                                  position = BDDMath.vec2 (0.0, ~0.01),
                                                  angle = 0.0,
                                                  linear_velocity = BDDMath.vec2_zero,
                                                  angular_velocity = 0.0,
                                                  linear_damping = 0.0,
                                                  angular_damping = 0.0,
                                                  allow_sleep = true,
                                                  awake = true,
                                                  fixed_rotation = false,
                                                  bullet = false,
                                                  active = true,
                                                  data = (),
                                                  inertia_scale = 1.0
                                                })
        val ground_shape = BDDShape.Polygon (BDDPolygon.box (40.0, 0.01))
        val ground_fixture = BDD.Body.create_fixture_default
                             (ground_body, ground_shape, (), 1.0)

        val ceiling_body = BDD.World.create_body (world,
                                                 {typ = BDD.Body.Static,
                                                  position = BDDMath.vec2 (0.0, 35.0),
                                                  angle = 0.0,
                                                  linear_velocity = BDDMath.vec2_zero,
                                                  angular_velocity = 0.0,
                                                  linear_damping = 0.0,
                                                  angular_damping = 0.0,
                                                  allow_sleep = true,
                                                  awake = true,
                                                  fixed_rotation = false,
                                                  bullet = false,
                                                  active = true,
                                                  data = (),
                                                  inertia_scale = 1.0
                                                })
        val ceiling_shape = BDDShape.Polygon (BDDPolygon.box (40.0, 1.0))
        val ceiling_fixture = BDD.Body.create_fixture_default
                             (ceiling_body, ceiling_shape, (), 1.0)


        fun make_ball (r, x) =
            let
                val ball_body = BDD.World.create_body (world,
                                                       {typ = BDD.Body.Dynamic,
                                                        position = BDDMath.vec2 (x, 10.0),
                                                        angle = 0.0,
                                                        linear_velocity = BDDMath.vec2_zero,
                                                        angular_velocity = 0.0,
                                                        linear_damping = 0.0,
                                                        angular_damping = 0.0,
                                                        allow_sleep = false,
                                                        awake = true,
                                                        fixed_rotation = false,
                                                        bullet = false,
                                                        active = true,
                                                        data = (),
                                                        inertia_scale = 1.0
                                                      })
                val ball_shape = BDDShape.Circle {radius = r,
                                                  p = BDDMath.vec2_zero}
                val ball_fixture = BDD.Body.create_fixture_default
                                       (ball_body, ball_shape, (), 5.0)

                val () = BDD.Fixture.set_restitution (ball_fixture, 1.0)

                val v = BDDMath.vec2 (x, 35.0)
                val j = BDD.World.create_joint
                            (world, {typ = BDD.Joint.RevoluteDef
                                               {local_anchor_a =
                                                BDD.Body.get_local_point (ceiling_body, v),
                                                local_anchor_b = BDDMath.vec2 (0.0, 25.0),
                                                reference_angle = 0.0,
                                                lower_angle = ~0.25 * Math.pi,
                                                upper_angle = 0.5 * Math.pi,
                                                enable_limit = false,
                                                max_motor_torque = 10000.0,
                                                motor_speed = Math.pi,
                                                enable_motor = false
                                               },
                                     user_data = (),
                                     body_a = ceiling_body,
                                     body_b = ball_body,
                                     collide_connected = true
                            })
            in () end

        (* These are touching *)
        val () = make_ball (1.0, ~16.0)
        val () = make_ball (1.0, ~14.0)
        val () = make_ball (1.0, ~12.0)
        val () = make_ball (1.0, ~10.0)
        val () = make_ball (1.0, ~8.0)


        (* These are not touching *)
        val () = make_ball (0.95, 8.0)
        val () = make_ball (0.95, 10.0)
        val () = make_ball (0.95, 12.0)
        val () = make_ball (0.95, 14.0)
        val () = make_ball (0.95, 16.0)

    in ()
    end


 fun handle_event _ _ = ()

 val test = Test {init = init,
                  handle_event = handle_event}

end
