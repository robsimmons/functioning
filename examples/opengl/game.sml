structure Game :> GAME =
struct
  open GL

  datatype spec = RGB of GLreal * GLreal * GLreal;

fun DrawPrim (_,[]) = glFlush ()
  | DrawPrim (obj,l) =
    let
        fun draw_vertices [] = ()
          | draw_vertices ((x,y,z)::t) =
                    ((glVertex3f x y z); draw_vertices t)
          
        fun draw_all [] = ()
          | draw_all ((RGB(r,g,b), v)::t) =
            ((glColor3f r g b) ; draw_vertices(v);
             draw_all t)
    in
        (glBegin(obj);
         draw_all l;
         glEnd();
         glFlush())
    end

  (* Types *)
  datatype loc = L of {xpos : GLreal, ypos : GLreal}
  type state =
    { starloc : loc,        (* Location of player character *) 
      robotlocs : loc list, (* Locations of enemies and/or friends *)
      key : SDL.sdlk option (* Last key depressed *)
    }
  type screen = SDL.surface

  (* Constant parameters *)
  val width = 500
  val height = 500
  val use_gl = true
  val dpos = 0.02
  val dpos_star = 0.02
  
  (* Initialization *)
  val init_starloc = L {xpos = 0.0, ypos = 0.0}
  val init_robotlocs = [L {xpos=5.0, ypos=5.0}, L {xpos=100.0, ypos=500.0}]
  val initstate =
      { 
        starloc = init_starloc,
        robotlocs = init_robotlocs,
        key = NONE
      }

  val time = ref 0  (* Imperatively updated loop counter *)

  fun initscreen screen =
  (
   glClearColor 0.0 0.0 0.0 1.0;
   glClearDepth 1.0;
 
   glViewport 0 0 width height;
 
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
 
   glOrtho ~5.0 5.0 ~5.0 5.0 5.0 ~5.0;
 
   glMatrixMode(GL_MODELVIEW);

   glEnable(GL_TEXTURE_2D);
 
    glLoadIdentity();

   SDL.glflip();
   ()
  )

  fun move_right (L {xpos=x, ypos=y}) = L {xpos=x+dpos_star, ypos=y}
  fun move_left  (L {xpos=x, ypos=y}) = L {xpos=x-dpos_star, ypos=y}
  fun move_up    (L {xpos=x, ypos=y}) = L {xpos=x, ypos=y+dpos_star}
  fun move_down  (L {xpos=x, ypos=y}) = L {xpos=x, ypos=y-dpos_star}

  fun render screen {starloc = L {xpos=sx, ypos=sy}, robotlocs = rs, key = key} =
  let in
      glClear(GL_COLOR_BUFFER_BIT + GL_DEPTH_BUFFER_BIT);
   glLoadIdentity();
    glBegin(GL_QUADS);
        glColor3f 1.0 0.0 0.0;
        glVertex3f 0.0 0.0 0.0;
        glColor3f 1.0 1.0 0.0; 
        glVertex3f 10.0 0.0 0.0;
        glColor3f 1.0 0.0 1.0;
       glVertex3f 10.0 20.0 0.0;
        glColor3f 1.0 1.0 1.0;
      glVertex3f 0.0 10.0 0.0;
    glEnd();

   DrawPrim (GL_QUADS,
             [
              (RGB(0.9, 1.0, 0.0),
               [(sx - 1.0, sy + 1.0, 1.0), (sx + 1.0, sy + 1.0,1.0)]),
              (RGB(0.0,0.7,0.7),
               [(sx + 1.0, sy - 1.0,1.0),( sx - 1.0, sy - 1.0,1.0)])
              ]);
   


   SDL.glflip();
      ()

  end

  fun keyDown (SDL.SDLK_ESCAPE) _ = NONE (* quit the game *)
    | keyDown key {starloc=s, robotlocs=r, key=oldkey} =
          SOME {starloc=s, robotlocs=r, key=SOME key}

  fun keyUp upkey {starloc=s, robotlocs=r, key=SOME downkey} =
    let
      val k = if upkey = downkey then NONE else SOME downkey
    in
      SOME {starloc=s, robotlocs=r, key=k}
    end
    | keyUp _ s = SOME s

  fun handle_event (SDL.E_KeyDown {sym = k}) s = keyDown k s
    | handle_event (SDL.E_KeyUp {sym = k}) s = keyUp k s
    | handle_event SDL.E_Quit s = NONE
    | handle_event _ s = SOME s

  val ticks_per_second = 60.0

  fun tick {starloc = s, robotlocs = rs, key = k} =
    let
      val () = time := !time + 1
      val step = 100

      fun updateRobotLoc (L {xpos=x, ypos=y}) =
        if !time mod step = 0 then
          L {xpos=(x+dpos), ypos=(y+dpos)}
        else
          L {xpos=x, ypos=y}

      val s = case k of
                  SOME SDL.SDLK_RIGHT => move_right s
                | SOME SDL.SDLK_LEFT  => move_left s
                | SOME SDL.SDLK_UP => move_up s
                | SOME SDL.SDLK_DOWN => move_down s
                | _ => s
    in
        SOME {starloc = s, robotlocs = map updateRobotLoc rs, key = k}
    end
end

structure Main =
struct
  structure S = RunGame (Game)
end
