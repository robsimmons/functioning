structure RayCast =
struct

open Types
open BDDOps

infix 6 :+: :-: %-% %+% +++
infix 7 *: *% +*: +*+ #*% @*:

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

val angle = ref 0.0

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
                                                  data = Nothing,
                                                  inertia_scale = 1.0
                                                })
        val ground_shape = BDDShape.Polygon (BDDPolygon.box (40.0, 0.01))
        val ground_fixture = BDD.Body.create_fixture_default
                             (ground_body, ground_shape, (), 1.0)


    in
        angle := 0.0
    end

 fun tick world =
     angle := (!angle) + 0.25 * Math.pi / 180.0

 fun render world =
     let
         val L = 11.0
         val point1 = BDDMath.vec2(0.0, 10.0)
         val d = BDDMath.vec2(L * Math.cos(!angle), L * Math.sin(!angle))
         val point2 = point1 :+: d

         val m_hit = ref false
         val m_point = ref (BDDMath.vec2(0.0, 0.0))
         val m_normal = ref (BDDMath.vec2(0.0, 0.0))
         fun callback {fixture, point, normal, fraction} =
             let
                 val body = BDD.Fixture.get_body fixture
                 val userData = BDD.Body.get_data body
             in
                 case userData of
                     Filtered => BDD.World.IgnoreAndContinue
                   | _ => (m_hit := true;
                           m_point := point;
                           m_normal := normal;
                           BDD.World.Clip fraction)
             end

         val () = BDD.World.ray_cast (world, callback, point1, point2)
     in
         ()
     end

 fun handle_event _ _ = ()

 val test = Test {init = init,
                  handle_event = handle_event,
                  tick = tick,
                  render = render}

end
