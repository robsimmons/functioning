structure Game :> GAME =
struct
  (* Types *)
  type state = unit 
  type screen = SDL.surface

  (* Constants *)
  val width = 800
  val height = 600
  val pixelsPerMeter = 20
  val meter_height = (Real.fromInt height /
                      Real.fromInt pixelsPerMeter)
  val meter_width = (Real.fromInt width /
                     Real.fromInt pixelsPerMeter)


  structure BW = BDDWorld( 
                 struct type fixture_data = unit
                        type body_data = string
                        type joint_data = unit
                 end
                 )
  
  val gravity = BDDMath.vec2 (0.0, 0.0) 
  val world = BW.World.world (gravity, true)

  val zero = BDDMath.vec2 (0.0, 0.0) 
  val textbody = BW.World.create_body (world,
                                       {typ = BW.Body.Dynamic,
                                        position = zero,
                                        angle = 0.0,
                                        linear_velocity = BDDMath.vec2 (0.0, ~2.0),
                                        angular_velocity = 0.0,
                                        linear_damping = 0.0,
                                        angular_damping = 0.0,
                                        allow_sleep = false,
                                        awake = true,
                                        fixed_rotation = true,
                                        bullet = false,
                                        active = true,
                                        data = "hello world",
                                        inertia_scale = 1.0
                                      })

  val textfix = BW.Body.create_fixture_default
                    (textbody,
                     BDDShape.Polygon (BDDPolygon.box (0.2, 0.2)),
                     (),
                     20.0)
  val () = BW.Fixture.set_restitution (textfix, 1.0)

  val groundbody = BW.World.create_body
                       (world,
                        {typ = BW.Body.Dynamic,
                         position = BDDMath.vec2 (0.0, ~10.0),
                         angle = 0.0,
                         linear_velocity = zero,
                         angular_velocity = 0.0,
                         linear_damping = 0.0,
                         angular_damping = 0.0,
                         allow_sleep = false,
                         awake = true,
                         fixed_rotation = true,
                         bullet = false,
                         active = true,
                         data = "ground",
                         inertia_scale = 1.0
                       })

  val groundfix = BW.Body.create_fixture_default
                      (groundbody,
                       BDDShape.Polygon (BDDPolygon.box (50.0, 0.5)),
                       (),
                       0.2)
  val () = BW.Fixture.set_restitution (groundfix, 1.0)

(*
  The box2d world has as its origin the center of the screen.
*)
  fun worldToScreen (v : BDDMath.vec2) : int * int =
      let open BDDMath
          val (xw, yw) = (vec2x v, vec2y v)
          val x = Real.*(Real.fromInt pixelsPerMeter,
                         xw + (meter_width / 2.0))
          val y = Real.*(Real.fromInt pixelsPerMeter,
                         Real.~(yw) + (meter_height / 2.0))
          val (xi, yi) = (Real.round x, Real.round y)
      in
          (xi, yi)
      end

  val initstate = ()
  
  fun initscreen screen =
  (
    SDL.flip screen
  )

  val lasttime = ref (Time.now ())

  fun dophysics () = 
      let val now = Time.now ()
          val diff = Time.-(now, !lasttime)
          val () = lasttime := now
          val millis = IntInf.toString (Time.toMilliseconds (diff))
          val () = BW.World.step (world, Time.toReal diff,
                                  10, 10)
      in () end
      

  fun drawbodies screen bl = 
      ( case bl of
            SOME b =>
            let 
                val p = BW.Body.get_position b
                val txt = BW.Body.get_data b
                val (x, y) = worldToScreen p
                val () = Font.Normal.draw (screen, x, y, txt);
            in drawbodies screen (BW.Body.get_next b) end
          | NONE => ()
      )

  fun render screen () =
  (
    SDL.clearsurface (screen, SDL.color (0w0,0w0,0w0,0w0));
    dophysics ();
    drawbodies screen (BW.World.get_body_list world);
    SDL.flip screen
  )

  fun keyDown (SDL.SDLK_ESCAPE) _ = NONE (* quit the game *)
    | keyDown _ s = SOME s

  fun keyUp _ s = SOME s

  fun handle_event (SDL.E_KeyDown {sym=k}) s = keyDown k s
    | handle_event (SDL.E_KeyUp {sym=k}) s = keyUp k s
    | handle_event _ s = SOME s


  fun tick s = SOME s
end

structure Main =
struct
  structure S = RunGame (Game)
end
