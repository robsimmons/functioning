(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Collision detection.
   Corresponds to collision/b2collision.h, but the type
   declarations were moved to types.sml *)

signature BDDCOLLISION =
sig

  (* Evaluate the manifold with supplied transforms. This assumes
     modest motion from the original state. This does not change the
     point count, impulses, etc. The radii must come from the shapes
     that generated the manifold. *)
  val create_world_manifold : BDDTypes.manifold *
                              BDDMath.transform * real *
                              BDDMath.transform * real ->
                              BDDTypes.world_manifold


  (* Compute the point states given two manifolds, based on whether they
     share mutual points. The states pertain to the transition from
     manifold1 to manifold2. So state1 is either persist or remove
     while state2 is either add or persist. *)
  val get_point_states : BDDTypes.manifold * BDDTypes.manifold ->
                         BDDTypes.point_state BDDTypes.one_or_two *
                         BDDTypes.point_state BDDTypes.one_or_two


  (* True if the bounds are sorted. *)
  val aabb_valid : BDDTypes.aabb -> bool
  (* Get the center of the AABB. *)
  val aabb_center : BDDTypes.aabb -> BDDMath.vec2
  (* Get the extents of the AABB (half-widths). *)
  val aabb_extents : BDDTypes.aabb -> BDDMath.vec2

  (* combine a b
     Combine the two AABBs.
     Port note: In the original, this was an imperative operation;
     we treat AABBs functionally. *)
  val aabb_combine : BDDTypes.aabb * BDDTypes.aabb -> BDDTypes.aabb

  (* contains container test
     Does the container contain the test aabb? *)
  val aabb_contains : BDDTypes.aabb * BDDTypes.aabb -> bool

  val aabb_overlap : BDDTypes.aabb * BDDTypes.aabb -> bool

  val aabb_ray_cast : BDDTypes.aabb * BDDTypes.ray_cast_input ->
                      BDDTypes.ray_cast_output option

  (* Compute the collision manifold between two circles. *)
  val collide_circles : BDDCircle.circle * BDDMath.transform *
                        BDDCircle.circle * BDDMath.transform ->
                        BDDTypes.manifold option

  (* Pack contact_id from components (all ints must be in [0, 255]). *)
  val contact_id : { index_a : int, index_b : int,
                     type_a : int, type_b : int } -> BDDTypes.contact_id

  val contact_id_index_a : BDDTypes.contact_id -> int
  val contact_id_index_b : BDDTypes.contact_id -> int
  val contact_id_type_a : BDDTypes.contact_id -> int
  val contact_id_type_b : BDDTypes.contact_id -> int


  (* clip_segment_to_line (v1, v2, normal, offset, vertex_index_a)
     Clipping for contact manifolds. Returns either no points (entire segment
     is clipped out) or two points (possibly modified). *)
  val clip_segment_to_line : BDDTypes.clip_vertex * BDDTypes.clip_vertex *
                             BDDMath.vec2 * real * int ->
                             (BDDTypes.clip_vertex * BDDTypes.clip_vertex) option

  (* Compute the collision manifold between a polygon and a circle. *)
  val collide_polygon_and_circle : BDDPolygon.polygon * BDDMath.transform *
                                   BDDCircle.circle * BDDMath.transform ->
                                   BDDTypes.manifold option

  (* Compute the collision manifold between two polygons. *)
  val collide_polygons : BDDPolygon.polygon * BDDMath.transform *
                         BDDPolygon.polygon * BDDMath.transform ->
                         BDDTypes.manifold option

  (* Determine if two generic shapes overlap. *)
  val test_overlap : BDDShape.shape * BDDShape.shape *
                     BDDMath.transform * BDDMath.transform ->
                     bool
end
