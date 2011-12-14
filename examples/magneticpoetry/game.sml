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


  datatype bodytype = Text of {text : string,
                               width : int,
                               height : int}
                    | VerticalLine of int
                    | HorizontalLine of int

  structure B = BDDWorld( 
                struct type fixture_data = unit
                       type body_data = bodytype
                       type joint_data = unit
                end
                )
  
  val gravity = BDDMath.vec2 (0.0, 0.0) 
  val world = B.World.world (gravity, true)


  fun create_text_body (text : string)
                       (p : BDDMath.vec2)
                       (v : BDDMath.vec2)
                       (mass : real) : unit = 
      let val pixel_width = (Font.Normal.width - Font.Normal.overlap)
                            * String.size text 
          val pixel_height = Font.Normal.height * 3 div 4
          val meter_width = (Real.fromInt pixel_width) /
                            (Real.fromInt pixelsPerMeter)
          val meter_height = (Real.fromInt pixel_height) /
                             (Real.fromInt pixelsPerMeter)
          val body = B.World.create_body
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
                           bullet = true (* mass < 1.0*),
                           active = true,
                           data = Text {text = text,
                                        width = pixel_width,
                                        height = pixel_height},
                           inertia_scale = 1.0
                         })
          val density = mass / meter_width * meter_height
          val fixture = B.Body.create_fixture_default
                            (body,
                             BDDShape.Polygon
                                 (BDDPolygon.box (meter_width / 2.0,
                                                  meter_height / 2.0)),
                             (),
                             density)
          val () = B.Fixture.set_restitution (fixture, 1.0)
          val () = B.Fixture.set_friction (fixture, 0.0)
      in () end

  val zero = BDDMath.vec2 (0.0, 0.0) 

  fun create_wall (p : BDDMath.vec2)
                  (meter_height : real) : unit = 
      let 
          val pixel_height =
                 Real.round (meter_height * (Real.fromInt pixelsPerMeter))
          val body = B.World.create_body
                         (world,
                          {typ = B.Body.Dynamic,
                           position = p,
                           angle = 0.0,
                           linear_velocity = zero,
                           angular_velocity = 0.0,
                           linear_damping = 0.0,
                           angular_damping = 0.0,
                           allow_sleep = false,
                           awake = true,
                           fixed_rotation = true,
                           bullet = true,
                           active = true,
                           data = VerticalLine pixel_height,
                           inertia_scale = 1.0
                         })

          val fixture = B.Body.create_fixture_default
                            (body,
                             BDDShape.Polygon
                                 (BDDPolygon.box (0.2,
                                                  meter_height / 2.0)),
                             (),
                             10000.0)
          val () = B.Fixture.set_restitution (fixture, 1.0)
          val () = B.Fixture.set_friction (fixture, 0.0)
      in () end


  fun create_ceiling (p : BDDMath.vec2)
                     (meter_width : real) : unit = 
      let 
          val pixel_width =
                 Real.round (meter_width * (Real.fromInt pixelsPerMeter))
          val body = B.World.create_body
                         (world,
                          {typ = B.Body.Dynamic,
                           position = p,
                           angle = 0.0,
                           linear_velocity = zero,
                           angular_velocity = 0.0,
                           linear_damping = 0.0,
                           angular_damping = 0.0,
                           allow_sleep = false,
                           awake = true,
                           fixed_rotation = true,
                           bullet = true,
                           active = true,
                           data = HorizontalLine pixel_width,
                           inertia_scale = 1.0
                         })

          val fixture = B.Body.create_fixture_default
                            (body,
                             BDDShape.Polygon
                                 (BDDPolygon.box (meter_width / 2.0,
                                                  0.2)),
                             (),
                             10000.0)
          val () = B.Fixture.set_restitution (fixture, 1.0)
          val () = B.Fixture.set_friction (fixture, 0.0)
      in () end


  val mt =
      MersenneTwister.initstring (Time.toString (Time.now ()))

  fun random_vector max_mag = 
      let open MersenneTwister
          val theta = Math.pi * 2.0 *
               (Real.fromInt (random_nat mt 1000)) / 1000.0
          val mag = max_mag * 
               (Real.fromInt (random_nat mt 1000)) / 1000.0
          val x = mag * (Math.cos theta)
          val y = mag * (Math.sin theta)
      in BDDMath.vec2 (x, y) end

  val () = Util.for 0 10 (fn y =>
               create_text_body "hydrogen" 
                                (random_vector 9.0)
                                (random_vector 5.0)
                                0.1
                         )


  val () = create_text_body "helium"
                            (random_vector 9.0)
                            (random_vector 6.0)
                            0.4

  val () = create_text_body "helium"
                            (random_vector 9.0)
                            (random_vector 6.0)
                            0.4

  val () = create_text_body "krypton"
                            (random_vector 9.0)
                            (random_vector 6.0)
                            8.38

  val () = create_text_body "xenon"
                            (random_vector 9.0)
                            (random_vector 6.0)
                            13.12

  val () = create_text_body "radon"
                            (random_vector 9.0)
                            (random_vector 6.0)
                            22.2


  val () = create_wall (BDDMath.vec2 (~15.0, 0.0)) 24.0

  val () = create_wall (BDDMath.vec2 (15.0, 0.0)) 24.0

  val () = create_ceiling (BDDMath.vec2 (0.0, 12.0)) 30.0
  val () = create_ceiling (BDDMath.vec2 (0.0, ~12.0)) 30.0


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
      

  val white = SDL.color (0w255,0w255,0w255,0w0);

  fun drawbodies screen bl = 
      ( case bl of
            SOME b =>
            let 
                val p = B.Body.get_position b
                val (x, y) = worldToScreen p
                val () =
                    (case B.Body.get_data b of
                         Text {text, width, height} => 
                         let val (x0, y0) = (x - (width div 2),
                                             y - (height div 2)) 
                         in Font.Normal.draw (screen, x0, y0, text) 
                         end 
                       | VerticalLine h =>
                         let val y0 = y - (h div 2)
                             val y1 = y + (h div 2)
                             val () = SDL.drawline (screen, x, y0, x, y1, white)
                         in () end
                       | HorizontalLine w =>
                         let val x0 = x - (w div 2)
                             val x1 = x + (w div 2)
                             val () = SDL.drawline (screen, x0, y, x1, y, white)
                         in () end
                    )
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
