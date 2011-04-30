signature GAME =
sig
  type state
  type screen = SDL.surface
  val initstate : state
  val initscreen : screen -> unit
  val width : int 
  val height : int
  val render : screen -> state -> unit
  val keyDown : SDL.sdlk -> state -> state option
  val keyUp : SDL.sdlk -> state -> state option
  val tick : state -> state option
end
