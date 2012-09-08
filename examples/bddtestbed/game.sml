structure Game :> GAME =
struct
  open Types
  open GL
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  type state = game_state
  type screen = SDL.surface

  (* Constant parameters *)
  val width = 640
  val height = 480
  val use_gl = true

  fun draw_solid_polygon vertexList (RGB (r, g, b)) =
      (
       glEnable GL_BLEND;
       glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA;
       glColor4d (0.5 * r) (0.5 * g) (0.5 * b) 0.5;
       glBegin GL_TRIANGLE_FAN;
       List.map (fn v => glVertex2d (BDDMath.vec2x v) (BDDMath.vec2y v)) vertexList;
       glEnd ();
       glDisable GL_BLEND;
       
       glColor4d r g b 1.0;
       glBegin GL_LINE_LOOP;
       List.map (fn v => glVertex2d (BDDMath.vec2x v) (BDDMath.vec2y v)) vertexList;
       glEnd()
      )

  fun draw_solid_circle center radius axis (RGB (r, g, b)) = 
      let val (centerx, centery) = BDDMath.vec2xy center
          val k_segments = 16
          val k_increment = 2.0 * Math.pi / (Real.fromInt k_segments)
          val (px, py) = BDDMath.vec2xy (center :+: (radius *: axis))
          fun draw_vertex ii =
              let val theta = k_increment * (Real.fromInt ii)
                  val v = center :+: (radius *: (BDDMath.vec2 (Math.cos theta,
                                                               Math.sin theta)))
                  val (x, y) = BDDMath.vec2xy v
              in glVertex2d x y
              end
      in
       glEnable GL_BLEND;
       glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA;
       glColor4d (0.5 * r) (0.5 * g) (0.5 * b) 0.5;
       glBegin GL_TRIANGLE_FAN;
       List.tabulate (k_segments, draw_vertex);
       glEnd ();
       glDisable GL_BLEND;
       
       glColor4d r g b 1.0;
       glBegin GL_LINE_LOOP;
       List.tabulate (k_segments, draw_vertex);
       glEnd();

       (* draw radius *)
       glBegin GL_LINES;
       glVertex2d centerx centery;
       glVertex2d px py;
       glEnd ()
      end


  fun body_color b =
      if not (BDD.Body.get_active b)
      then RGB (0.5, 0.5, 0.3)
      else case BDD.Body.get_type b of
               BDD.Body.Static => RGB (0.5, 0.9, 0.5)
             | BDD.Body.Kinematic => RGB (0.5, 0.5, 0.9)
             | BDD.Body.Dynamic =>
               if not (BDD.Body.get_awake b)
               then RGB (0.6, 0.6, 0.6)
               else RGB (0.9, 0.7, 0.7)


  val initstate =
      let val world = BDD.World.world (BDDMath.vec2 (0.0, ~10.0), true)
          val () = VerticalStack.init world
      in
          GS {world = world}
      end

  fun initscreen screen =
      (

       glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA;
       glEnable GL_BLEND;

       glEnable GL_TEXTURE_2D;

       glClearColor 0.0 0.0 0.0 1.0;
       glClearDepth 1.0;
       glViewport 0 0 width height;
       glClear GL_COLOR_BUFFER_BIT;
       glMatrixMode GL_PROJECTION;
       glLoadIdentity();

       let val viewCenter = BDDMath.vec2 (0.0, 20.0)
           val ratio = (Real.fromInt width) / (Real.fromInt height)
           val extents = BDDMath.vec2 (ratio * 25.0, 25.0)
           val lower = viewCenter :-: extents
           val upper = viewCenter :+: extents
           val (lx, ly) = BDDMath.vec2xy lower
           val (ux, uy) = BDDMath.vec2xy upper
       in
           glOrtho lx ux ly uy 5.0 ~5.0
       end;

       glMatrixMode GL_MODELVIEW;

       glLoadIdentity();
       ()
      )

  fun drawfixture color tf fix =
      case BDD.Fixture.shape fix of
          BDDShape.Polygon p =>
          let val n = BDDPolygon.get_vertex_count p
              val vl = List.tabulate (n, fn ii => tf @*: (BDDPolygon.get_vertex(p, ii)))
          in draw_solid_polygon vl color
          end 
        | BDDShape.Circle {radius, p} =>
          let val center = tf @*: p
              val axis = (BDDMath.transformr tf) +*: (BDDMath.vec2 (1.0, 0.0))
          in draw_solid_circle center radius axis color
          end

  fun drawbody b =
      let val pos = BDD.Body.get_position b
          val theta = BDD.Body.get_angle b
          val fl = BDD.Body.get_fixtures b
          val color = body_color b
          val tf = BDD.Body.get_transform b
      in
          oapp BDD.Fixture.get_next (drawfixture color tf) fl
      end


  fun render screen (GS {world, ...}) = 
  let in
   glClear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);
   glLoadIdentity();

   oapp BDD.Body.get_next drawbody (BDD.World.get_body_list world);

   glFlush();
   SDL.glflip();
   ()
  end

  fun keyDown (SDL.SDLK_ESCAPE) _ = NONE (* quit the game *)
    | keyDown (SDL.SDLK_COMMA) (s as GS {world, ...}) =
      (VerticalStack.bullet world; SOME s)
    | keyDown key s = SOME s

  fun keyUp upkey s = SOME s

  fun handle_event (SDL.E_KeyDown {sym = k}) s = keyDown k s
    | handle_event (SDL.E_KeyUp {sym = k}) s = keyUp k s
    | handle_event SDL.E_Quit s = NONE
    | handle_event _ s = SOME s

  val ticks_per_second = 60.0

  fun dophysics world = 
      let val timestep = 1.0 / ticks_per_second
          val () = BDD.World.step (world, timestep, 10, 10)
      in () end

  fun tick (s as GS {world, ...}) =
    let val () = dophysics world
    in
        SOME s
    end
end

structure Main =
struct
  structure S = RunGame (Game)
end
