structure Game :> GAME =
struct

  type state = {xpos : int, ypos : int} (* position of controllable glyph *)
  type screen = SDL.surface

  val width = 800
  val height = 600

  val initstate = {xpos = width div 2, ypos = height div 2}
  
  fun initscreen screen =
  (
    SDL.blitall (star, screen, #xpos initstate, #ypos initstate); 
    SDL.flip screen
  )

  fun render screen {xpos=x, ypos=y} =
  (
    SDL.clearsurface (screen, SDL.color (0w0,0w0,0w0,0w0));
    SDL.blitall (star, screen, x, y);
    SDL.flip screen
  )

  val dpos = 5

  fun keyDown (SDL.SDLK_ESCAPE) _ = NONE (* quit the game *)
    | keyDown (SDL.SDLK_RIGHT) {xpos=x, ypos=y} = SOME {xpos=x+dpos, ypos=y}
    | keyDown (SDL.SDLK_LEFT)  {xpos=x, ypos=y} = SOME {xpos=x-dpos, ypos=y}
    | keyDown (SDL.SDLK_UP)    {xpos=x, ypos=y} = SOME {xpos=x, ypos=y-dpos}
    | keyDown (SDL.SDLK_DOWN)  {xpos=x, ypos=y} = SOME {xpos=x, ypos=y+dpos}
    | keyDown _ s = SOME s

  fun keyUp _ s = SOME s

  fun handle_event (SDL.E_KeyDown {sym=k}) s = keyDown k s
    | handle_event (SDL.E_KeyUp {sym=k}) s = keyUp k s
    | handle_event _ s = SOME s

  (* fun tick {xpos=x, ypos=y} = SOME {xpos=x+1, ypos=y} *)
  fun tick s = SOME s
end

