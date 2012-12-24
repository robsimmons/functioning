structure RayCast =
struct

open Types


val maxBodies = 256

local
    val w = 1.0
    val b = 2.0 / (2.0 + Math.sqrt 2.0)
    val s = Math.sqrt 2.0 * b
in
val m_polygons = Array.fromList
                 [BDDPolygon.polygon [BDDMath.vec2(~0.5, 0.0),
                                      BDDMath.vec2(0.5, 0.0),
                                      BDDMath.vec2(0.0, 1.5)],
                  BDDPolygon.polygon [BDDMath.vec2(~0.1, 0.0),
                                      BDDMath.vec2(0.1, 0.0),
                                      BDDMath.vec2(0.0, 1.5)],
                  BDDPolygon.polygon [BDDMath.vec2(0.5 * s, 0.0),
                                      BDDMath.vec2(0.5 * w, b),
                                      BDDMath.vec2(0.5 * w, b + s),
                                      BDDMath.vec2(0.5 * s, w),
                                      BDDMath.vec2(~0.5 * s, w),
                                      BDDMath.vec2(~0.5 * w, b + s),
                                      BDDMath.vec2(~0.5 * w, b),
                                      BDDMath.vec2(~0.5 * s, 0.0)],
                  BDDPolygon.box(0.5, 0.5)
                 ]
end

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
        val ground_shape = BDDShape.Polygon (BDDPolygon.box (40.0, 0.01))
        val ground_fixture = BDD.Body.create_fixture_default
                             (ground_body, ground_shape, (), 1.0)

        val shape = BDDShape.Circle {radius = 1.0,
                                     p = BDDMath.vec2_zero}
    in ()
    end


 fun handle_event _ _ = ()

 val test = Test {init = init,
                  handle_event = handle_event,
                  tick = ignore,
                  render = ignore}

end
