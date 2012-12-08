structure VaryingRestitution =
struct

open Types


val restitution = Array.fromList [0.0, 0.1, 0.3, 0.5, 0.75, 0.9, 1.0]

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

        val shape = BDDShape.Circle {radius = 1.0,
                                     p = BDDMath.vec2_zero}
    in
        BDDOps.for 0 6 (fn i =>
            let
                val pos = BDDMath.vec2 (~10.0 + 3.0 * (Real.fromInt i), 20.0)
                val body = BDD.World.create_body (world,
                                                  {typ = BDD.Body.Dynamic,
                                                   position = pos,
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
                val fixture = BDD.Body.create_fixture_default
                                  (body, shape, (), 1.0)
                val () = BDD.Fixture.set_restitution (fixture,
                                                      Array.sub(restitution, i))
            in ()
            end
        )
    end


 fun handle_event _ _ = ()

 val test = Test {init = init,
                  handle_event = handle_event,
                  tick = ignore}

end
