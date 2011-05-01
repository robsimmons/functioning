signature GAME =
sig
  type state
  type screen = SDL.surface
  val initstate : state
  val initscreen : screen -> unit
  val width : int 
  val height : int
  val render : screen -> state -> unit
  val handle_event : SDL.event -> state -> state option
  val tick : state -> state option
end
