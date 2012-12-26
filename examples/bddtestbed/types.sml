structure Types =
struct

datatype spec = RGB of GL.GLdouble * GL.GLdouble * GL.GLdouble;

datatype body_data = Nothing | Filtered

structure BDD = BDDWorld(
                struct type fixture_data = unit
                       type body_data = body_data
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
          tick : BDD.world -> unit,
          render : BDD.world -> unit
         }

type profile_data = { step_count : int,
                      total : BDDDynamicsTypes.profile,
                      max : BDDDynamicsTypes.profile
                    }

fun new_profile_data () =
    let fun new_profile () = {step = Time.zeroTime,
                              collide = Time.zeroTime,
                              solve = Time.zeroTime,
                              solve_toi = Time.zeroTime}
    in
        {step_count = 0, total = new_profile(), max = new_profile()}
    end

type settings =
         { draw_contacts : bool ref,
           paused : bool ref,
           profile : profile_data option ref,
           test_num : int ref,
           view : view ref
         }

type mouse_joint = {get_target : unit -> BDDMath.vec2,
                    set_target : BDDMath.vec2 -> unit
                   }

datatype game_state = GS of {world : BDD.world,
                             mouse_joint : (mouse_joint * BDD.joint) option,
                             test : test,
                             settings : settings
                            }

end
