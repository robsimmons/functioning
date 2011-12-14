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


  structure B = BDDWorld( 
                struct type fixture_data = unit
                       type body_data = string
                       type joint_data = unit
                end
                )
  
  val gravity = BDDMath.vec2 (0.0, 0.0) 
  val world = B.World.world (gravity, true)


  fun create_text_body (text : string)
                       (p : BDDMath.vec2)
                       (v : BDDMath.vec2)
                       (density : real) : unit = 
      let val body = B.World.create_body
                         (world,
                          {typ = B.Body.Dynamic,
                           position = p,
                           angle = 0.0,
                           linear_velocity = v,
                           angular_velocity = 0.0,
                           linear_damping = 0.0,
                           angular_damping = 0.0,
                           allow_sleep = false,
                           awake = true,
                           fixed_rotation = true,
                           bullet = false,
                           active = true,
                           data = text,
                           inertia_scale = 1.0
                         })
          val fixture = B.Body.create_fixture_default
                            (body,
                             BDDShape.Polygon (BDDPolygon.box (1.0, 0.5)),
                             (),
                             density)
          val () = B.Fixture.set_restitution (fixture, 1.0)
      in () end

  val zero = BDDMath.vec2 (0.0, 0.0) 
  val () = create_text_body "hello world" zero
                            (BDDMath.vec2 (1.0, ~2.0))
                            1.0
  val () = create_text_body "ground" 
                            (BDDMath.vec2 (0.0, ~10.0))
                            zero
                            1000.0
  val () = create_text_body "ground" 
                            (BDDMath.vec2 (~5.0, ~10.0))
                            zero
                            1000.0
  val () = create_text_body "ground" 
                            (BDDMath.vec2 (5.0, ~10.0))
                            zero
                            1000.0

  val () = create_text_body "wall" 
                            (BDDMath.vec2 (10.0, 0.0))
                            zero
                            1000.0 
  val () = create_text_body "wall" 
                            (BDDMath.vec2 (~10.0, 0.0))
                            zero
                            1000.0

  val () = create_text_body "sky" 
                            (BDDMath.vec2 (0.0, 10.0))
                            zero
                            1000.0
  val () = create_text_body "sky" 
                            (BDDMath.vec2 (~5.0, 10.0))
                            zero
                            1000.0
  val () = create_text_body "sky" 
                            (BDDMath.vec2 (5.0, 10.0))
                            zero
                            1000.0

(*
  The box2d world has as its origin the center of the screen.
*)
  fun worldToScreen (v : BDDMath.vec2) : int * int =
      let open BDDMath
          val (xw, yw) = (vec2x v, vec2y v)
          open Real
          val x = (fromInt pixelsPerMeter) *
                  (xw + (meter_width / 2.0))
          val y = (fromInt pixelsPerMeter) *
                  (~yw + (meter_height / 2.0))
          val (xi, yi) = (round x, round y)
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
          val () = B.World.step (world, Time.toReal diff,
                                 10, 10)
      in () end
      

  fun drawbodies screen bl = 
      ( case bl of
            SOME b =>
            let 
                val p = B.Body.get_position b
                val txt = B.Body.get_data b
                val (x, y) = worldToScreen p
                val () = Font.Normal.draw (screen, x, y, txt);
            in drawbodies screen (B.Body.get_next b) end
          | NONE => ()
      )

  fun render screen () =
  (
    SDL.clearsurface (screen, SDL.color (0w0,0w0,0w0,0w0));
    dophysics ();
    drawbodies screen (B.World.get_body_list world);
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
