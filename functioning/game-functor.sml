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
               | SOME e => Option.app loop (Game.handle_event e s))

  val () = Game.initscreen screen               
  val () = loop Game.initstate
end

