(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* XXXX documentation is out of date.
   The implementation of the dynamics engine has several complex,
   mutually-referential data structures. This defines the raw storage
   for these (body, fixture, world, contact, and joint) in a transparent
   way, as well as accessors for them. These are then used to implement
   the algorithms and abstract types in BDDWorld. This way we don't need
   the entire implementation in one file. Clients should not bother with
   this file. You can't even use it to get at the internals of dynamics
   types, because in the client interface those types are abstract.

   Corresponding to parts of dynamics/contacts/b2contact.h, dynamics/b2body.h,
   dynamics/b2fixture.h, dynamics/joints/b2joint.h, etc. *)
structure BDDDynamicsTypes =
struct

  exception BDDDynamics of string

  (* 16 category bits, then 16 mask bits; group index *)
  type filter = Word32.word * int
  datatype body_type =
      Static
    | Kinematic
    | Dynamic

  datatype limit_state =
      Inactive
    | AtLower
    | AtUpper
    | Equal

  (* Action that the raycast callback can take upon encountering a fixture.
     Port note: These were all encoded as special float values in Box2D. *)
  datatype raycast_action =
      IgnoreAndContinue
    | Terminate
      (* Clip to the fraction of the ray; must be in (0, 1). *)
    | Clip of real
    | Don'tClip

  type contact_impulse = { normal_impulses : real array,
                           tangent_impulses : real array }

  (* Corresponding to Dynamics/b2TimeStep.h *)
  type time_step = { (* time step *)
                     dt : real,
                     (* inverse time step (0 if dt == 0) *)
                     inv_dt : real,
                     (* dt * inv_dt0 *)
                     dt_ratio : real,
                     velocity_iterations : int,
                     position_iterations : int,
                     warm_starting : bool }

  datatype joint_def =
      RevoluteDef of {local_anchor_a : BDDMath.vec2,
                      local_anchor_b : BDDMath.vec2,
                      reference_angle : real,
                      lower_angle : real,
                      upper_angle : real,
                      max_motor_torque : real,
                      motor_speed : real,
                      enable_limit : bool,
                      enable_motor : bool}
    | PrismaticDef of {local_anchor_a : BDDMath.vec2,
                       local_anchor_b : BDDMath.vec2,
                       local_axis_a : BDDMath.vec2,
                       reference_angle : real,
                       enable_limit : bool,
                       lower_translation : real,
                       upper_translation : real,
                       enable_motor : bool,
                       max_motor_force : real,
                       motor_speed : real}
    | DistanceDef
    | PulleyDef
    | MouseDef of { target : BDDMath.vec2,
                    max_force : real,
                    frequency_hz : real,
                    damping_ratio : real
                  }
    | GearDef
    | LineDef
    | WeldDef
    | FrictionDef

  type solver_data = { step : time_step,
                       positionsc : BDDMath.vec2mut Array.array,
                       positionsa : real ref Array.array,
                       velocitiesv : BDDMath.vec2mut Array.array,
                       velocitiesw : real ref Array.array
                     }

  type joint_dispatch = { init_velocity_constraints : solver_data -> unit,
                          solve_velocity_constraints : solver_data -> unit,
                          solve_position_constraints : solver_data -> bool,
                          get_anchor_a : unit -> BDDMath.vec2,
                          get_anchor_b : unit -> BDDMath.vec2
                        }

  datatype joint_type =
           Mouse of {get_target : unit -> BDDMath.vec2,
                      set_target : BDDMath.vec2 -> unit
                    }
         | Revolute of {enable_limit : bool -> unit,
                        is_limit_enabled : unit -> bool,
                        enable_motor : bool -> unit,
                        is_motor_enabled : unit -> bool
                       }
         | Prismatic of {enable_limit : bool -> unit,
                        is_limit_enabled : unit -> bool,
                        enable_motor : bool -> unit,
                        is_motor_enabled : unit -> bool
                       }
         | Unknown of unit

  type profile = { step : Time.time,
                   collide : Time.time,
                   solve : Time.time,
                   solve_toi : Time.time
                 }

end
