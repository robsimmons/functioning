structure Types =
struct

datatype spec = RGB of real * real * real;

structure BDD = BDDWorld( 
                struct type fixture_data = unit
                       type body_data = unit
                       type joint_data = unit
                end
                )


datatype constants = CONST of {width : int,
                               height : int,
                               left : real,
                               right : real,
                               bottom : real,
                               top : real,
                               gravity : BDDMath.vec2}

datatype game_state = GS of {world : BDD.world
                            }

datatype test = Test of
         {init : BDD.world -> unit,
          handle_event :  BDD.world -> SDL.event -> unit
         }


end
