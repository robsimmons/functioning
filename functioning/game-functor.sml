functor RunGame (Game : GAME) =
struct
  val screen = SDL.makescreen (Game.width, Game.height)

  fun loop s =
    case Game.tick s of
         NONE => ()
       | SOME s =>
           (Game.render screen s;
            case SDL.pollevent () of
                 NONE => (SDL.delay 0; loop s)
               | SOME (SDL.E_KeyDown {sym = k}) => 
                   Option.app loop (Game.keyDown k s)
               | SOME (SDL.E_KeyUp {sym = k}) =>
                   Option.app loop (Game.keyUp k s)
               | _ => loop s)

  val () = Game.initscreen screen               
  val () = loop Game.initstate
end


structure Main =
struct
  structure S = RunGame (Game)
end
