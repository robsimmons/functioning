(* TODO: 
* - make this do something when collision occurs 
* *)

structure Game :> GAME =
struct
  (* Types *)
  type loc = {xpos : int, ypos : int}
  type state =
    { starloc : loc,        (* Location of player character *) 
      robotlocs : loc list, (* Locations of enemies and/or friends *)
      key : SDL.sdlk option (* Last key depressed *)
    }
  type screen = SDL.surface

  (* Constant parameters *)
  val width = 800
  val height = 600
  val dpos = 5
  val dpos_star = 1
  val star = Graphics.redstar
  val robot = Graphics.robot
  
  (* Initialization *)
  val init_starloc = {xpos = width div 2, ypos = height div 2}
  val init_robotlocs = [{xpos=5, ypos=5}, {xpos=100, ypos=500}]
  val initstate =
      { 
        starloc = init_starloc,
        robotlocs = init_robotlocs,
        key = NONE
      }
  val time = ref 0  (* Imperatively updated loop counter *)

  fun blit     pic loc screen = SDL.blitall (pic, screen, #xpos loc, #ypos loc)
  fun blitmany pic locs screen = app (fn l => blit pic l screen) locs

  fun initscreen screen =
  (
    blit star init_starloc screen;
    blitmany robot init_robotlocs screen;

    SDL.flip screen
  )

  fun move_right {xpos=x, ypos=y} = {xpos=x+dpos_star, ypos=y}
  fun move_left  {xpos=x, ypos=y} = {xpos=x-dpos_star, ypos=y}
  fun move_up    {xpos=x, ypos=y} = {xpos=x, ypos=y-dpos_star}
  fun move_down  {xpos=x, ypos=y} = {xpos=x, ypos=y+dpos_star}

  fun render screen {starloc = s, robotlocs = rs, key = key} =
  let in
    SDL.clearsurface (screen, SDL.color (0w0,0w0,0w0,0w0));
    blitmany robot rs screen;
    blit star s screen;
    SDL.flip screen
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

      fun updateRobotLoc {xpos=x, ypos=y} =
        if !time mod step = 0 then
          {xpos=(x+dpos) mod width, ypos=(y+dpos) mod height}
        else
          {xpos=x, ypos=y}

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

