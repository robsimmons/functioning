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
      GS {world = BDD.World.world (BDDMath.vec2 (0.0, ~10.0), true)}

  fun initscreen screen =
      (

       glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA;
       glEnable GL_BLEND;

       glEnable(GL_TEXTURE_2D);

       glClearColor 0.0 0.0 0.0 1.0;
       glClearDepth 1.0;
       glViewport 0 0 width height;
       glClear GL_COLOR_BUFFER_BIT;
       glMatrixMode(GL_PROJECTION);
       glLoadIdentity();
       glOrtho ~10.0 10.0 ~10.0 10.0 5.0 ~5.0;
       glMatrixMode(GL_MODELVIEW);

       glLoadIdentity();
       ()
      )

  fun drawfixture color pos theta fix = ()

  fun drawbody b =
      let val pos = BDD.Body.get_position b
          val theta = BDD.Body.get_angle b
          val fl = BDD.Body.get_fixtures b
          val color = body_color b
      in
          oapp BDD.Fixture.get_next (drawfixture color pos theta) fl
      end


  fun render screen (GS {world, ...}) = 
  let in
   glClear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);
   glLoadIdentity();

   (* draw a square *)
   glBegin(GL_QUADS);
   glColor3f 0.9 1.0 0.0;
   glVertex3f (~ 4.0) 4.0 0.0;
   glVertex3f 4.0 4.0 0.0;
   glColor3f 0.0 0.8 0.9;
   glVertex3f 4.0 (~ 4.0) 0.0;
   glVertex3f (~4.0) (~ 4.0) 0.0;
   glEnd();

   oapp BDD.Body.get_next drawbody (BDD.World.get_body_list world);

   glFlush();
   SDL.glflip();
   ()
  end

  fun keyDown (SDL.SDLK_ESCAPE) _ = NONE (* quit the game *)
    | keyDown key s = SOME s

  fun keyUp upkey s = SOME s

  fun handle_event (SDL.E_KeyDown {sym = k}) s = keyDown k s
    | handle_event (SDL.E_KeyUp {sym = k}) s = keyUp k s
    | handle_event SDL.E_Quit s = NONE
    | handle_event _ s = SOME s

  val ticks_per_second = 60.0

  fun tick (s as GS {...}) =
    let
    in
        SOME s
    end
end

structure Main =
struct
  structure S = RunGame (Game)
end
