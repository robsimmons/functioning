(* TODO: make this do something when collision occurs *)

structure Game :> GAME =
struct
  val width = 800
  val height = 600

  val star = Graphics.redstar
  val robot = Graphics.robot
  
  type loc = {xpos : int, ypos : int} (* position of controllable glyph *)

  type state = {starloc : loc, robotlocs : loc list}
  type screen = SDL.surface

  val init_starloc = {xpos = width div 2, ypos = height div 2}
  val init_robotlocs = [{xpos=5, ypos=5}, {xpos=100, ypos=500}]
  val initstate = {starloc = init_starloc, robotlocs = init_robotlocs}

  val time = ref 0

  fun blit     pic loc screen = SDL.blitall (pic, screen, #xpos loc, #ypos loc)
  fun blitmany pic locs screen = app (fn l => blit pic l screen) locs

  fun initscreen screen =
  (
    blit star init_starloc screen;
    blitmany robot init_robotlocs screen;

    SDL.flip screen
  )

  fun render screen {starloc = sloc, robotlocs = rlocs} =
  (
    SDL.clearsurface (screen, SDL.color (0w0,0w0,0w0,0w0));
    blit star sloc screen;
    blitmany robot rlocs screen;
    SDL.flip screen
  )

  val dpos = 5

  (* move_right : loc -> loc *)
  fun move_right {xpos=x, ypos=y} = {xpos=x+dpos, ypos=y}
  fun move_left  {xpos=x, ypos=y} = {xpos=x-dpos, ypos=y}
  fun move_up    {xpos=x, ypos=y} = {xpos=x, ypos=y-dpos}
  fun move_down  {xpos=x, ypos=y} = {xpos=x, ypos=y+dpos}

  fun keyDown (SDL.SDLK_ESCAPE) _ = NONE (* quit the game *)
    | keyDown (SDL.SDLK_RIGHT) {starloc=s, robotlocs=r} = 
        SOME {starloc=move_right s, robotlocs=r}
    | keyDown (SDL.SDLK_LEFT)  {starloc=s, robotlocs=r} = 
        SOME {starloc=move_left s, robotlocs=r}
    | keyDown (SDL.SDLK_UP)    {starloc=s, robotlocs=r} = 
        SOME {starloc=move_up s, robotlocs=r}
    | keyDown (SDL.SDLK_DOWN)  {starloc=s, robotlocs=r} = 
        SOME {starloc=move_down s, robotlocs=r}
    | keyDown _ s = SOME s

  fun keyUp _ s = SOME s

  (* fun tick {xpos=x, ypos=y} = SOME {xpos=x+1, ypos=y} *)
  fun tick {starloc = s, robotlocs = rs} =
    let
      val () = time := !time + 1
      val step = 100
      fun updateRobotLoc {xpos=x, ypos=y} =
        if !time mod step = 0 then
          {xpos=(x+dpos) mod width, ypos=(y+dpos) mod height}
        else
          {xpos=x, ypos=y}
    in
        SOME {starloc = s, robotlocs = map updateRobotLoc rs}
    end
end

