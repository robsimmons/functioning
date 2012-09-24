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

  datatype joint_def =
      RevoluteDef
    | PrismaticDef
    | DistanceDef
    | PulleyDef
    | MouseDef of BDDDynamicsTypes.mouse_joint_def
    | GearDef
    | LineDef
    | WeldDef
    | FrictionDef


  datatype joint_type =
           Mouse of BDDDynamicsTypes.mouse_joint
         | Unknown of unit

  val get_next : joint -> joint option
  val get_body_a : joint -> body
  val get_body_b : joint -> body

  val get_typ : joint -> joint_type option

  val get_anchor_a : joint -> BDDMath.vec2
  val get_anchor_b : joint -> BDDMath.vec2

end
