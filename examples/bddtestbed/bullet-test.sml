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
                                                  position = BDDMath.vec2 (0.0, 0.0),
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
        val ground_shape = BDDShape.Polygon
                               (BDDPolygon.edge (BDDMath.vec2(~10.0, 0.0),
                                                 BDDMath.vec2(10.0, 0.0)))
        val ground_fixture = BDD.Body.create_fixture_default
                             (ground_body, ground_shape, (), 1.0)

        val vert_shape = BDDShape.Polygon
                             (BDDPolygon.rotated_box
                                  (0.2, 1.0, BDDMath.vec2(1.0, 1.0), 0.0))
        val vert_fixture = BDD.Body.create_fixture_default
                             (ground_body, vert_shape, (), 1.0)

        val vert_shape1 = BDDShape.Polygon
                             (BDDPolygon.rotated_box
                                  (0.2, 1.0, BDDMath.vec2(~1.0, 1.0), 0.0))
        val vert_fixture1 = BDD.Body.create_fixture_default
                             (ground_body, vert_shape1, (), 1.0)

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

        val x = 0.1
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

    in state := SOME (TS {body = bar, bullet = bullet})
    end

 fun launch world =
     let val TS {body, bullet} = valOf (!state)
         val () = BDD.Body.set_transform (body, BDDMath.vec2(0.0, 4.0), 0.0)
         val () = BDD.Body.set_linear_velocity (body, BDDMath.vec2_zero)
         val () = BDD.Body.set_angular_velocity (body, 0.0)

         val x = 0.1
         val () = BDD.Body.set_transform (bullet, BDDMath.vec2(x, 10.0), 0.0)
         val () = BDD.Body.set_linear_velocity (bullet, BDDMath.vec2(0.0, ~50.0))
         val () = BDD.Body.set_angular_velocity (bullet, 0.0)
     in ()
     end

 fun handle_event world (SDL.E_KeyDown {sym = SDL.SDLK_COMMA}) = launch world
   | handle_event world _ = ()


 val test = Test {init = init,
                  handle_event = handle_event,
                  tick = ignore}

end
