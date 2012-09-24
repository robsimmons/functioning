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

  (* TODO don't need this here. This represents jointdefs. *)
  type mouse_joint_def =
       {
        target : BDDMath.vec2,
        max_force : real,
        frequency_hz : real,
        damping_ratio : real
       }

  datatype joint_def =
      RevoluteDef
    | PrismaticDef
    | DistanceDef
    | PulleyDef
    | MouseDef of mouse_joint_def
    | GearDef
    | LineDef
    | WeldDef
    | FrictionDef

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


  type joint_dispatch = { init_velocity_constraints : time_step -> unit,
                          solve_velocity_constraints : time_step -> unit,
                          solve_position_constraints : real -> bool,
                          get_anchor_a : unit -> BDDMath.vec2,
                          get_anchor_b : unit -> BDDMath.vec2
                        }

  type mouse_joint = {get_target : unit -> BDDMath.vec2,
                      set_target : BDDMath.vec2 -> unit,
                      base : joint_dispatch}

  datatype joint_type =
           Mouse of mouse_joint
         | Unknown of unit


end
