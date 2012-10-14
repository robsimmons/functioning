structure Prismatic =
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
                                                position = BDDMath.vec2 (~10.0, 10.0),
                                                angle = Math.pi * 0.5,
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
        val ball_shape = BDDShape.Polygon (BDDPolygon.box (2.0, 0.5))
        val ball_fixture = BDD.Body.create_fixture_default
                           (ball_body, ball_shape, (), 5.0)


        val v = BDDMath.vec2 (0.0, 0.0)
        val axis = BDDMath.vec2normalized (BDDMath.vec2 (2.0, 1.0))
        val j = BDD.World.create_joint
                 (world, {typ = BDD.Joint.PrismaticDef
                                    {local_anchor_a = BDD.Body.get_local_point (ground_body, v),
                                     local_anchor_b = BDD.Body.get_local_point (ball_body, v),
                                     reference_angle = BDD.Body.get_angle ball_body -
                                                       BDD.Body.get_angle ground_body,
                                     local_axis_a = BDD.Body.get_local_point (ground_body, axis),
                                     lower_translation = 0.0,
                                     upper_translation = 20.0,
                                     enable_limit = true,
                                     max_motor_force = 10000.0,
                                     motor_speed = 10.0,
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
              SOME (BDD.Joint.Prismatic {enable_limit, is_limit_enabled, ...}) =>
                enable_limit (not (is_limit_enabled ()))
            | _ => ()
         )
     )
   | handle_event world (SDL.E_KeyDown {sym = SDL.SDLK_m}) =
     (case !joint of
         NONE => ()
       | SOME j =>
         (case BDD.Joint.get_typ j of
              SOME (BDD.Joint.Prismatic {enable_motor, is_motor_enabled, ...}) =>
                enable_motor (not (is_motor_enabled ()))
            | _ => ()
         )
     )
   | handle_event _ _ = ()

 val test = Test {init = init,
                  handle_event = handle_event}

end
