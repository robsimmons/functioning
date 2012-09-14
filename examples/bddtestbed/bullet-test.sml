structure BulletTest =
struct
open Types


datatype teststate = TS of {body: BDD.body,
                            bullet : BDD.body}

val state : (teststate option) ref = ref NONE

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
        val ground_shape = BDDShape.Polygon (BDDPolygon.box (10.0, 0.01))
        val ground_fixture = BDD.Body.create_fixture_default
                             (ground_body, ground_shape, (), 1.0)

        val vert_shape = BDDShape.Polygon
                             (BDDPolygon.rotated_box
                                  (0.2, 1.0, BDDMath.vec2(0.5, 1.0), 0.0))
        val vert_fixture = BDD.Body.create_fixture_default
                             (ground_body, vert_shape, (), 1.0)

        val bar = BDD.World.create_body (world,
                                          {typ = BDD.Body.Dynamic,
                                           position = BDDMath.vec2 (0.0, 4.0),
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
        val bar_shape = BDDShape.Polygon (BDDPolygon.box(2.0, 0.1))
        val bar_fixture = BDD.Body.create_fixture_default
                          (bar, bar_shape, (), 1.0)

        val x = 0.20352793
        val bullet = BDD.World.create_body (world,
                                          {typ = BDD.Body.Dynamic,
                                           position = BDDMath.vec2 (x, 10.0),
                                           angle = 0.0,
                                           linear_velocity = BDDMath.vec2(0.0, ~50.0),
                                           angular_velocity = 0.0,
                                           linear_damping = 0.0,
                                           angular_damping = 0.0,
                                           allow_sleep = true,
                                           awake = true,
                                           fixed_rotation = false,
                                           bullet = true,
                                           active = true,
                                           data = (),
                                           inertia_scale = 1.0
                                         })
        val bullet_shape = BDDShape.Polygon (BDDPolygon.box(0.25, 0.25))
        val bullet_fixture = BDD.Body.create_fixture_default
                             (bullet, bullet_shape, (), 100.0)

    in ()
    end



 fun handle_event _ _ = ()

 val test = Test {init = init,
                  handle_event = handle_event}

end
