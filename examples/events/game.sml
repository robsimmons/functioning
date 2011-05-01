structure Game :> GAME =
struct
  (* Types *)
  type loc = {xpos : int, ypos : int}
  type state =
    {
      things : loc list,
      key : SDL.sdlk option (* Last key depressed *)
    }
  type screen = SDL.surface

  (* Constant parameters *)
  val width = 800
  val height = 600
  val star = Graphics.redstar
  val robot = Graphics.robot
  
  (* Initialization *)
  val initstate =
      { 
        things = [],
        key = NONE
      }

  val time = ref 0

  fun blit     pic {xpos=x,ypos=y} screen =
      SDL.blitall (pic, screen, x, y)
  fun blitmany pic locs screen = app (fn l => blit pic l screen) locs

  fun initscreen screen = SDL.flip screen

  fun render screen {things = locs, key = _} =
  let in
    SDL.clearsurface (screen, SDL.color (0w0,0w0,0w0,0w0));
    blitmany star locs screen;
    SDL.flip screen
  end

  fun keyDown (SDL.SDLK_ESCAPE) _ = NONE (* quit the game *)
    | keyDown (SDL.SDLK_m) {things=locs, key=k} =
        SOME 
        {things=({xpos = !time mod width, ypos = !time mod height})::locs,
         key=SOME SDL.SDLK_m}
    | keyDown key s = SOME s

  fun keyUp upkey {things=locs, key=SOME downkey} =
    let
      val k = if upkey = downkey then NONE else SOME downkey
    in
      SOME {things=locs, key=k}
    end
    | keyUp _ s = SOME s

  fun handle_event (SDL.E_KeyDown {sym = k}) s = keyDown k s
    | handle_event (SDL.E_KeyUp {sym = k}) s = keyUp k s
    | handle_event SDL.E_Quit s = NONE
    | handle_event _ s = SOME s

  fun tick {key = k, things = locs} =
    let
      val () = time := !time + 1
      val step = 100
    in
        SOME {key = k, things = locs}
    end
end

