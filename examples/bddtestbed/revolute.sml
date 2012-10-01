structure Revolute =
struct

open Types


val joint : (BDD.joint option) ref = ref NONE

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


        val ball_body = BDD.World.create_body (world,
                                               {typ = BDD.Body.Dynamic,
                                                position = BDDMath.vec2 (0.0, 20.0),
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
        val ball_shape = BDDShape.Circle {radius = 0.5,
                                          p = BDDMath.vec2_zero}
        val ball_fixture = BDD.Body.create_fixture_default
                           (ball_body, ball_shape, (), 5.0)
        val w = 100.0
        val () = BDD.Body.set_angular_velocity (ball_body, w)
        val () = BDD.Body.set_linear_velocity (ball_body, BDDMath.vec2 (~8.0 * w, 0.0))


        val v = BDDMath.vec2 (0.0, 12.0)
        val j = BDD.World.create_joint
                 (world, {typ = BDD.Joint.RevoluteDef
                                    {local_anchor_a = BDD.Body.get_local_point (ground_body, v),
                                     local_anchor_b = BDD.Body.get_local_point (ball_body, v),
                                     reference_angle = BDD.Body.get_angle ground_body -
                                                       BDD.Body.get_angle ball_body,
                                     lower_angle = ~0.25 * Math.pi,
                                     upper_angle = 0.5 * Math.pi,
                                     enable_limit = true,
                                     max_motor_torque = 0.0,
                                     motor_speed = 0.0,
                                     enable_motor = false
                                    },
                          user_data = (),
                          body_a = ground_body,
                          body_b = ball_body,
                          collide_connected = true
                 })
        val () = joint := SOME j
    in ()
    end


 fun handle_event world (SDL.E_KeyDown {sym = SDL.SDLK_l}) =
     (case !joint of
         NONE => ()
       | SOME j =>
         (case BDD.Joint.get_typ j of
              SOME (BDD.Joint.Revolute {enable_limit, is_limit_enabled, ...}) =>
                enable_limit (not (is_limit_enabled ()))
            | _ => ()
         )
     )
   | handle_event _ _ = ()

 val test = Test {init = init,
                  handle_event = handle_event}

end
