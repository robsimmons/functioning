structure VerticalStack =
struct

open Types

val columnCount = 5
val rowCount = 16

val xs = Array.fromList [0.0, ~10.0, ~5.0, 5.0, 10.0]

fun init world =
    let
        val ground_body = BDD.World.create_body (world,
                                                 {typ = BDD.Body.Static,
                                                  position = BDDMath.vec2_zero,
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
        val ground_shape = BDDShape.Polygon (BDDPolygon.box (40.0, 0.3))
        val ground_fixture = BDD.Body.create_fixture_default
                             (ground_body, ground_shape, (), 1.0)
        val shape = BDDShape.Polygon (BDDPolygon.box (0.5, 0.5))
    in
        Util.for 0 (columnCount - 1) (fn j =>
         Util.for 0 (rowCount - 1) (fn i =>
            let val n = j * rowCount + i
                val pos = BDDMath.vec2 (Array.sub(xs, j),
                                        0.752 + 1.54 * (Real.fromInt i))
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
                val () = BDD.Fixture.set_friction (fixture, 0.3)
            in ()
            end
         )
        )
    end

end
