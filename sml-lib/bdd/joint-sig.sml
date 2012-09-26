signature BDDJOINT =
sig

  (* Supplied by functor argument *)
  type fixture_data
  type body_data
  type joint_data

  type body
  type fixture
  type joint
  type world
  type contact
  type filter
  type contactedge
  type jointedge

  type mouse_joint_def = BDDDynamicsTypes.mouse_joint_def
  type mouse_joint = BDDDynamicsTypes.mouse_joint

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
    | PrismaticDef
    | DistanceDef
    | PulleyDef
    | MouseDef of mouse_joint_def
    | GearDef
    | LineDef
    | WeldDef
    | FrictionDef


  datatype joint_type =
           Mouse of mouse_joint
         | Revolute of unit
         | Unknown of unit

  val get_next : joint -> joint option
  val get_body_a : joint -> body
  val get_body_b : joint -> body

  val get_typ : joint -> joint_type option

  val get_anchor_a : joint -> BDDMath.vec2
  val get_anchor_b : joint -> BDDMath.vec2

end
