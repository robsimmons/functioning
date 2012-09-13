structure Game :> GAME =
struct
  open GL

  datatype spec = RGB of GLreal * GLreal * GLreal;


  (* Types *)
  datatype loc = L of {xpos : GLreal, ypos : GLreal}
  type state =
    { robotloc : loc,        (* Location of player character *)
      key : SDL.sdlk option (* Last key depressed *)
    }
  type screen = SDL.surface

  (* Constant parameters *)
  val width = 500
  val height = 500
  val use_gl = true
  val dpos_robot = 0.02

  (* Initialization *)
  val init_robotloc = L {xpos = 0.0, ypos = 0.0}
  val initstate =
      {
        robotloc = init_robotloc,
        key = NONE
      }

  fun glGenSingleTexture () =
      let val arr = Array.array (1, 0)
          val () = glGenTextures 1 arr
      in Array.sub (arr, 0)
      end

  val robot = Graphics.requireimage "media/graphics/robot.png"
  val noise = Graphics.requireimage "media/graphics/noise.png"

  fun make_message text =
      let val pixel_width = (Font.Normal.width - Font.Normal.overlap)
                            * String.size text
          val pixel_height = Font.Normal.height
          val surf = SDL.makesurface (pixel_width, pixel_height)
          val () = SDL.clearsurface (surf, SDL.color(0w255, 0w255, 0w255, 0w255))
          val () = Font.Normal.draw (surf, 0, 0, text)
      in surf
      end

  val message = make_message "font uses glDrawPixels; robot uses texture mapping"

  fun load_texture surface =
      let
          val w = SDL.surface_width surface
          val h = SDL.surface_height surface
          val texture = glGenSingleTexture ()
          val mode = case (SDL.get_bytes_per_pixel surface,
                           SDL.is_rgb surface) of
                         (4, true) => GL_RGBA
                       | (4, false) => GL_BGRA
                       | (_, true) => GL_RGB
                       | (_, false) => GL_BGR
      in
          glBindTexture GL_TEXTURE_2D texture;
          glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST;
          glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST;
          glTexImage2D GL_TEXTURE_2D 0 4 w h 0 mode GL_UNSIGNED_BYTE (SDL.getpixels surface);
          texture
      end

  fun blit surface =
      let
          val w = SDL.surface_width surface
          val h = SDL.surface_height surface
          val mode = case (SDL.get_bytes_per_pixel surface,
                           SDL.is_rgb surface) of
                         (4, true) => GL_RGBA
                       | (4, false) => GL_BGRA
                       | (_, true) => GL_RGB
                       | (_, false) => GL_BGR
      in
          glDrawPixels w h mode GL_UNSIGNED_BYTE (SDL.getpixels surface)
      end


  val noise_texture = ref 0;
  val robot_texture = ref 0;

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
       glOrtho ~5.0 5.0 ~5.0 5.0 5.0 ~5.0;
       glMatrixMode(GL_MODELVIEW);

       glLoadIdentity();
       noise_texture := load_texture noise;
       robot_texture := load_texture robot;
       ()
      )


  fun move_right (L {xpos=x, ypos=y}) = L {xpos=x+dpos_robot, ypos=y}
  fun move_left  (L {xpos=x, ypos=y}) = L {xpos=x-dpos_robot, ypos=y}
  fun move_up    (L {xpos=x, ypos=y}) = L {xpos=x, ypos=y+dpos_robot}
  fun move_down  (L {xpos=x, ypos=y}) = L {xpos=x, ypos=y-dpos_robot}

  fun render screen {robotloc = L {xpos=sx, ypos=sy}, key = key} =
  let in
   glClear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);
   glLoadIdentity();

   glDisable GL_TEXTURE_2D;

   (* draw a square *)
   glBegin(GL_QUADS);
   glColor3f 0.9 1.0 0.0;
   glVertex3f (~ 4.0) 4.0 0.0;
   glVertex3f 4.0 4.0 0.0;
   glColor3f 0.0 0.8 0.9;
   glVertex3f 4.0 (~ 4.0) 0.0;
   glVertex3f (~4.0) (~ 4.0) 0.0;
   glEnd();

   (* draw message *)
   glColor3f 1.0 1.0 1.0;
   glRasterPos2d ~4.5 4.5;
   glPixelZoom 1.0 ~1.0;
   blit message;

   glEnable GL_TEXTURE_2D;
   glColor3f 1.0 1.0 1.0;


   (* draw the robot *)
   glBindTexture GL_TEXTURE_2D (!robot_texture);
   glBegin(GL_QUADS);
   glTexCoord2i 0 1;
   glVertex3f sx sy 0.0;
   glTexCoord2i 1 1;
   glVertex3f (sx + 2.0) sy 0.0;
   glTexCoord2i 1 0;
   glVertex3f (sx + 2.0) (sy + 4.0) 0.0;
   glTexCoord2i 0 0;
   glVertex3f sx (sy + 4.0) 0.0;
   glEnd();

   (* draw a noisy rectangle *)
   glBindTexture GL_TEXTURE_2D (!noise_texture);
   glBegin(GL_QUADS);
   glTexCoord2i 0 0;
   glVertex3f (~2.0) 0.0 0.0;
   glTexCoord2i 0 2;
   glVertex3f (~4.0) (~2.0) 0.0;
   glTexCoord2i 4 2;
   glVertex3f 0.0 (~5.0) 0.0;
   glTexCoord2i 4 0;
   glVertex3f (2.0) (~3.0) 0.0;
   glEnd();


   glFlush();

   SDL.glflip();
   ()
  end

  fun keyDown (SDL.SDLK_ESCAPE) _ = NONE (* quit the game *)
    | keyDown key {robotloc=r, key=oldkey} =
          SOME {robotloc=r, key=SOME key}

  fun keyUp upkey {robotloc=r, key=SOME downkey} =
    let
      val k = if upkey = downkey then NONE else SOME downkey
    in
      SOME {robotloc=r, key=k}
    end
    | keyUp _ s = SOME s

  fun handle_event (SDL.E_KeyDown {sym = k}) s = keyDown k s
    | handle_event (SDL.E_KeyUp {sym = k}) s = keyUp k s
    | handle_event SDL.E_Quit s = NONE
    | handle_event _ s = SOME s

  val ticks_per_second = 60.0

  fun tick {robotloc = r, key = k} =
    let
      val r = case k of
                  SOME SDL.SDLK_RIGHT => move_right r
                | SOME SDL.SDLK_LEFT  => move_left r
                | SOME SDL.SDLK_UP => move_up r
                | SOME SDL.SDLK_DOWN => move_down r
                | _ => r
    in
        SOME {robotloc = r, key = k}
    end
end

structure Main =
struct
  structure S = RunGame (Game)
end
