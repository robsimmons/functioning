structure Types =
struct

datatype spec = RGB of GL.GLdouble * GL.GLdouble * GL.GLdouble;

structure BDD = BDDWorld(
                struct type fixture_data = unit
                       type body_data = unit
                       type joint_data = unit
                end
                )

datatype view = View of
         {center : BDDMath.vec2,
          zoom : real,
          needs_resize : bool
         }

datatype test = Test of
         {init : BDD.world -> unit,
          handle_event : BDD.world -> SDL.event -> unit,
          tick : BDD.world -> unit
         }

type settings =
         { draw_contacts : bool ref,
           paused : bool ref,
           profile : bool ref
         }

type mouse_joint = {get_target : unit -> BDDMath.vec2,
                    set_target : BDDMath.vec2 -> unit
                   }

datatype game_state = GS of {world : BDD.world,
                             mouse_joint : (mouse_joint * BDD.joint) option,
                             test : test,
                             view : view,
                             settings : settings
                            }

end
