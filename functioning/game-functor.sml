functor RunGame (Game : GAME) =
struct
  val screen = SDL.makescreen (Game.width, Game.height)

  val last_simulated_time = ref (Time.zeroTime)


  fun option_iterate f s n =
      if n <= 0 then SOME s
      else (case f s of
                NONE => NONE
              | SOME s' => option_iterate f s' (n - 1)
           )


  fun loop s =
      let local open Time
          in
            val new_observed_time = now ()
            val sim_seconds = toReal (new_observed_time - (!last_simulated_time))
            val num_ticks = Int.max(0,
                                    Real.round(sim_seconds * Game.ticks_per_second))
            val () = last_simulated_time :=
                     (!last_simulated_time) + 
                      fromReal(Real.fromInt(num_ticks) / Game.ticks_per_second)
          end
      in
          case option_iterate Game.tick s num_ticks of
              NONE => ()
            | SOME s =>
              (Game.render screen s;
               case SDL.pollevent () of
                   NONE => (SDL.delay 0; loop s)
                 | SOME e => Option.app loop (Game.handle_event e s))
      end

  val () = Game.initscreen screen

  val () = last_simulated_time := (Time.now())
     
  val () = loop Game.initstate
end


