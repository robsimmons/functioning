(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Internal module for solving contacts, which is used by the island
   solver (also internal!). Clients should ignore this module.

   Corresponding to dynamics/contacts/b2contactsolver.h *)
signature BDDCONTACT_SOLVER =
sig

  exception BDDContactSolver of string

  (* Parameterized by user data, since it uses the internal
     polymorpic types. *)
  type ('b, 'f, 'j) contact_solver

  (* A solver whose velocity constraints have not yet been initialized. *)
  type ('b, 'f, 'j) pre_contact_solver

  val pre_contact_solver : BDDDynamicsTypes.solver_data *
                           ('b, 'f, 'j) BDDDynamics.contact Vector.vector
                           ->
                           ('b, 'f, 'j) pre_contact_solver


  val solve_toi_position_constraints : ('b, 'f, 'j) pre_contact_solver * int * int -> bool

  (* Port note: pushed all of the velocity contraint intitialization to
     here. The original initialized the position-independent portions
     in the constructor. *)
  val initialize_velocity_constraints : ('b, 'f, 'j) pre_contact_solver ->
                                        ('b, 'f, 'j) contact_solver

  val warm_start : ('b, 'f, 'j) contact_solver -> unit

  val solve_velocity_constraints : ('b, 'f, 'j) contact_solver -> unit
  val store_impulses : ('b, 'f, 'j) contact_solver -> unit

  val solve_position_constraints : ('b, 'f, 'j) contact_solver -> bool


  (* Apply the function to every contact, paired with all of its
     impulses. *)
  (* Port note: In Box2D, the Report function loops over
     all of the contact constraints of the solver. This
     function is used to implement the loop without
     exposing the internal types. *)
  val app_contacts :
      ('b, 'f, 'j) contact_solver *
      (('b, 'f, 'j) BDDDynamics.contact *
       { normal_impulses : real array,
         tangent_impulses : real array } -> unit) -> unit

end
