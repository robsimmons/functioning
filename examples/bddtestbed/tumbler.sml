structure Tumbler =
struct

open Types

val e_count = 200
val m_count = ref 0


fun init world =
    let
        val () = m_count := 0
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

        val box_body = BDD.World.create_body (world,
                                              {typ = BDD.Body.Dynamic,
                                               position = BDDMath.vec2 (0.0, 10.0),
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

        val box_shape =
            BDDShape.Polygon (BDDPolygon.rotated_box (0.5, 10.0,
                                                      BDDMath.vec2(10.0, 0.0),
                                                      0.0))
        val box_fixture = BDD.Body.create_fixture_default
                              (box_body, box_shape, (), 1.0)
        val box_shape =
            BDDShape.Polygon (BDDPolygon.rotated_box (0.5, 10.0,
                                                      BDDMath.vec2(~10.0, 0.0),
                                                      0.0))
        val box_fixture = BDD.Body.create_fixture_default
                              (box_body, box_shape, (), 1.0)

        val box_shape =
            BDDShape.Polygon (BDDPolygon.rotated_box (10.0, 0.5,
                                                      BDDMath.vec2(0.0, 10.0),
                                                      0.0))
        val box_fixture = BDD.Body.create_fixture_default
                              (box_body, box_shape, (), 1.0)
        val box_shape =
            BDDShape.Polygon (BDDPolygon.rotated_box (10.0, 0.5,
                                                      BDDMath.vec2(0.0, ~10.0),
                                                      0.0))
        val box_fixture = BDD.Body.create_fixture_default
                              (box_body, box_shape, (), 1.0)

        val j = BDD.World.create_joint
                 (world, {typ = BDD.Joint.RevoluteDef
                                    {local_anchor_a = BDDMath.vec2(0.0, 10.0),
                                     local_anchor_b = BDDMath.vec2(0.0, 0.0),
                                     reference_angle = 0.0,
                                     lower_angle = 0.0,
                                     upper_angle = 0.0,
                                     enable_limit = false,
                                     max_motor_torque = 1.0e8,
                                     motor_speed = 0.05 * Math.pi,
                                     enable_motor = true
                                    },
                          user_data = (),
                          body_a = ground_body,
                          body_b = box_body,
                          collide_connected = true
                 })



    in
        ()
    end

fun tick world =
    if !m_count < e_count
    then
        let
            val body = BDD.World.create_body (world,
                                              {typ = BDD.Body.Dynamic,
                                               position = BDDMath.vec2 (0.0, 10.0),
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

        val shape =
            BDDShape.Polygon (BDDPolygon.box (0.125, 0.125))

        val fixture = BDD.Body.create_fixture_default
                              (body, shape, (), 1.0)
        in
            m_count := (!m_count) + 1
        end
    else ()

fun handle_event world _ = ()

val test = Test {init = init,
                 handle_event = handle_event,
                 tick = tick}

end
