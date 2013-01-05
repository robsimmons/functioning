(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Collision detection.
   Corresponds to the implementation components of
   collision/b2collision.{h,cpp} as well as b2collidecircle.cpp and
   b2collidepolygon.cpp *)

structure BDDCollision :> BDDCOLLISION =
struct
  open BDDSettings
  open BDDTypes
  open BDDMath
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*: &*:

  exception BDDCollision of string

  fun map_points f (OnePoint mp) = f mp
    | map_points f (TwoPoints (mp1, mp2)) = (f mp1; f mp2)

  fun map_one_or_two f (OnePoint p) = OnePoint (f p)
    | map_one_or_two f (TwoPoints (p1, p2)) = TwoPoints (f p1, f p2)

  fun exists_one_or_two f (OnePoint p) = f p
    | exists_one_or_two f (TwoPoints (p1, p2)) = f p1 orelse f p2

  fun exists_manifold f E_None = false
    | exists_manifold f (E_Circles {point, ...}) = f point
    | exists_manifold f (E_FaceA {points, ...}) = exists_one_or_two f points
    | exists_manifold f (E_FaceB {points, ...}) = exists_one_or_two f points

  fun manifold_points (E_Circles {point, ...} ) = OnePoint point
    | manifold_points (E_FaceA {points, ...} ) = points
    | manifold_points (E_FaceB {points, ...} ) = points

  fun create_world_manifold (manifold : manifold,
                             xfa : transform, radiusa : real,
                             xfb : transform, radiusb : real) : world_manifold =
      case manifold of
         E_Circles { point, local_point } =>
            let
                val pointa = multransformv(xfa, local_point)
                val pointb = multransformv
                    (xfb, #local_point point)
                val normal =
                    if distance_squared (pointa, pointb) >
                       epsilon * epsilon
                    then vec2normalized (pointb :-: pointa)
                    else vec2 (1.0, 0.0)

                val ca = pointa :+: radiusa *: normal
                val cb = pointb :-: radiusb *: normal
            in
                { normal = normal,
                  points = OnePoint (0.5 *: (ca :+: cb)) }
            end

      | E_FaceA {points, local_normal, local_point} =>
            let
                val normal = transformr xfa @*: local_normal
                val plane_point = xfa &*: local_point
                fun one_point p =
                    let val clip_point = xfb &*: #local_point p
                        val ca = clip_point :+:
                                 (radiusa - dot2(clip_point :-: plane_point,
                                                 normal)) *:
                                 normal
                        val cb = clip_point :-: radiusb *: normal
                    in
                        0.5 *: (ca :+: cb)
                    end
            in
                { normal = normal,
                  points = map_one_or_two one_point points }
            end

      | E_FaceB {points, local_normal, local_point} =>
            let val normal = transformr xfb @*: local_normal
                val plane_point = xfb &*: local_point
                fun one_point p =
                    let val clip_point = xfa &*: #local_point p
                        val cb = clip_point :+:
                                  (radiusb - dot2(clip_point :-: plane_point,
                                                  normal)) *:
                                  normal
                        val ca = clip_point :-: radiusa *: normal
                    in
                        0.5 *: (ca :+: cb)
                    end
            in
                (* Ensure normal points from A to B. *)
                { normal = vec2neg normal,
                  points = map_one_or_two one_point points }
            end


  fun get_point_states (manifold1 : manifold, manifold2 : manifold) =
      let
          fun in_manifold mf pt = exists_manifold (fn pt1 => #id pt = #id pt1) mf
      in
          (* Detect persists and removes. *)
          (map_one_or_two (fn pt => if in_manifold manifold2 pt
                                    then PersistState
                                    else RemoveState) (manifold_points manifold1),
          (* Detect persists and adds. *)
           map_one_or_two (fn pt => if in_manifold manifold1 pt
                                    then PersistState
                                    else AddState) (manifold_points manifold2))
      end

  val vertex_feature = 0
  val face_feature = 1

  fun contact_id { index_a : int, index_b : int,
                   type_a : int, type_b : int } =
      let val ia = Word32.fromInt index_a
          val ib = Word32.fromInt index_b
          val ta = Word32.fromInt type_a
          val tb = Word32.fromInt type_b
          val orb = Word32.orb
          infix orb
      in
          Word32.<<(ia, 0w24) orb
          Word32.<<(ib, 0w16) orb
          Word32.<<(ta, 0w8) orb
          tb
      end


  fun contact_id_index_a w =
      Word32.toInt(Word32.andb(Word32.>>(w, 0w24), 0w255))
  fun contact_id_index_b w =
      Word32.toInt(Word32.andb(Word32.>>(w, 0w16), 0w255))
  fun contact_id_type_a w =
      Word32.toInt(Word32.andb(Word32.>>(w, 0w8), 0w255))
  fun contact_id_type_b w =
      Word32.toInt(Word32.andb(w, 0w255))

  fun aabb_valid { lowerbound, upperbound } =
      let val d : vec2 = upperbound :-: lowerbound
      in vec2x d > 0.0 andalso vec2y d > 0.0
          andalso vec2is_valid lowerbound
          andalso vec2is_valid upperbound
      end

  fun aabb_center { lowerbound, upperbound } =
      0.5 *: (lowerbound :+: upperbound)

  fun aabb_extents { lowerbound, upperbound } =
      0.5 *: (upperbound :-: lowerbound)

  fun aabb_combine ({ lowerbound = l1, upperbound = u1 },
                    { lowerbound = l2, upperbound = u2 }) =
      { lowerbound = vec2min(l1, l2),
        upperbound = vec2max(u1, u2) }

  fun aabb_contains ({ lowerbound, upperbound }, { lowerbound = l,
                                                   upperbound = u }) =
      vec2x lowerbound <= vec2x l andalso
      vec2y lowerbound <= vec2y l andalso
      vec2x u <= vec2x upperbound andalso
      vec2y u <= vec2y upperbound

  fun aabb_ray_cast ({ lowerbound, upperbound },
                     { p1 : BDDMath.vec2, p2 : BDDMath.vec2,
                       max_fraction : real })
      : BDDTypes.ray_cast_output option =
      let
          val tmin = ref (~max_float)
          val tmax = ref max_float

          val p = p1
          val d = p2 :-: p1
          val absd : vec2 = vec2abs d

          (* In original, uninitialized *)
          val normal = ref (vec2 (0.0, 0.0))
          fun setnormalx x = normal := vec2 (x, 0.0)
          fun setnormaly y = normal := vec2 (0.0, y)

          (* In original, a loop for i = 0 and 1 *)
          fun loop proj (setnormal : real -> unit) =
              if proj absd < epsilon
              then (* parallel *)
                  not (proj p < proj lowerbound orelse
                       proj lowerbound < proj p)
              else
                  let val inv_d = 1.0 / proj d
                      val t1 = (proj lowerbound - proj p) * inv_d
                      val t2 = (proj upperbound - proj p) * inv_d
                      (* sign of the normal vector *)
                      val (t1, t2, s) =
                          if t1 > t2
                          then (t2, t1, 1.0)
                          else (t1, t2, ~1.0)
                  in
                      (* push the min up. *)
                      if (t1 > !tmin)
                      then (setnormal s; tmin := t1)
                      else ();

                      (* pull the max down. *)
                      tmax := Real.min(!tmax, t2);

                      not (!tmin > !tmax)
                  end

      in
          if loop vec2x setnormalx andalso
             loop vec2y setnormaly andalso
             (* Does the ray start inside the box?
                Does the ray intersect beyond the max fraction? *)
             not (!tmin < 0.0 orelse max_fraction < !tmin)
          then SOME { fraction = !tmin, normal = !normal }
          else NONE
      end

  fun aabb_overlap ({ lowerbound = al, upperbound = au },
                    { lowerbound = bl, upperbound = bu }) : bool =
      let val d1 = bl :-: au
          val d2 = al :-: bu
      in
          not (vec2x d1 > 0.0 orelse vec2y d1 > 0.0 orelse
               vec2x d2 > 0.0 orelse vec2y d2 > 0.0)
      end


  fun collide_circles (circlea : BDDCircle.circle,
                       xfa : transform,
                       circleb : BDDCircle.circle,
                       xfb : transform) : manifold option =
      let
          val pa : vec2 = xfa &*: #p circlea
          val pb : vec2 = xfb &*: #p circleb
          val d  : vec2 = pb :-: pa

          val dist_sqr = dot2(d, d)
          val ra = #radius circlea
          val rb = #radius circleb
          val radius = ra + rb
      in
          (* PERF would it work to return NONE? The client
             code can't be accessing these fields; they're
             not even initialized in the original code. *)
          if dist_sqr > radius * radius
          then NONE
          else
              SOME
              (E_Circles
                   { point = { local_point = #p circleb,
                               id = 0w0,
                               (* uninitialized in original *)
                               normal_impulse = 0.0,
                               tangent_impulse = 0.0 },
                     local_point = #p circlea} )
      end

  (* Sutherland-Hodgman clipping.

     Port note: This code is a little tricky in the original.
     I made them more exlicit here and also clearer that this
     can only return 0 or 2 points.
     *)
  fun clip_segment_to_line (v0, v1, normal : vec2, offset : real, vertex_index_a : int)
      : (clip_vertex * clip_vertex) option =
      let
          (* Calculate distance of end points to the line. *)
          val distance0 = dot2 (normal, #v v0) - offset
          val distance1 = dot2 (normal, #v v1) - offset
      in
          (* If the two points are on different sides of the
             plane, then we have to clip one of them. *)
          if distance0 * distance1 < 0.0
          then
              (* Find intersection point of edge and plane. *)
              let
                  val interp : real = distance0 / (distance0 - distance1)
                  val clippedv = #v v0 :+: interp *: (#v v1 :-: #v v0)
                  val id = contact_id {index_a = vertex_index_a,
                                       index_b = contact_id_index_b (#id v0),
                                       type_a = vertex_feature,
                                       type_b = face_feature}
              in
                  if distance0 > 0.0
                  then SOME(v1, { v = clippedv, id = id })
                  else SOME(v0, { v = clippedv, id = id })

              end
          else
            (* Otherwise, we either take both or none. *)
            if distance0 <= 0.0 andalso distance1 <= 0.0
            then SOME(v0, v1)
            else NONE
      end

  (* Determine if two generic shapes overlap. *)
  fun test_overlap (shapea : BDDShape.shape, shapeb : BDDShape.shape,
                    xfa : BDDMath.transform, xfb : BDDMath.transform) : bool =
      let
          val input = { proxya = BDDDistance.shape_proxy shapea,
                        proxyb = BDDDistance.shape_proxy shapeb,
                        transforma = xfa,
                        transformb = xfb,
                        use_radii = true }
          val cache = BDDDistance.initial_cache ()
          val { distance, ... } = BDDDistance.distance (input, cache)
      in
          distance < 10.0 * epsilon
      end


  (* Compute the collision manifold between a polygon and a circle. *)
  exception NoCollision
  fun collide_polygon_and_circle ({ centroid = _, vertices, normals } : BDDPolygon.polygon,
                                  xfa : BDDMath.transform,
                                  { p = cirp, radius = cirr } : BDDCircle.circle,
                                  xfb : BDDMath.transform) : BDDTypes.manifold option =
      let
        (* Compute circle position in the frame of the polygon. *)
        val c : vec2 = xfb &*: cirp
        val c_local : vec2 = mul_ttransformv (xfa, c)

        (* Find the min separating edge. *)
        val normal_index = ref 0
        val separation : real ref = ref (~max_float)
        val radius : real = polygon_radius + cirr
        val vertex_count = Array.length vertices

        val () =
            for 0 (vertex_count - 1)
            (fn i =>
             let
                 val s : real = dot2(Array.sub(normals, i),
                                     c_local :-: Array.sub(vertices, i))
             in
                 if s > radius
                 (* early out *)
                 then raise NoCollision
                 else if s > !separation
                      then (separation := s;
                            normal_index := i)
                      else ()
             end)

        (* Vertices that subtend the incident face. *)
        val vert_index1 = !normal_index
        val vert_index2 = if vert_index1 + 1 < vertex_count
                          then vert_index1 + 1
                          else 0
        val v1 = Array.sub(vertices, vert_index1)
        val v2 = Array.sub(vertices, vert_index2)

        val separation = !separation
      in
        (* If the center is inside the polygon ... *)
        if separation < epsilon
        then SOME
              (E_FaceA { local_normal = Array.sub(normals, !normal_index),
                         local_point = 0.5 *: (v1 :+: v2),
                         points = OnePoint { local_point = cirp,
                                             id = 0w0,
                                             (* PERF uninitialized in Box2D *)
                                             normal_impulse = 0.0,
                                             tangent_impulse = 0.0 }})
        else
        let
            (* Compute barycentric coordinates. *)
            val u1 : real = dot2(c_local :-: v1, v2 :-: v1)
            val u2 : real = dot2(c_local :-: v2, v1 :-: v2)
        in
            if u1 <= 0.0
            then (if distance_squared(c_local, v1) > radius * radius
                  then raise NoCollision
                  else SOME
                           (E_FaceA
                                { local_normal = vec2normalized (c_local :-: v1),
                                  local_point = v1,
                                  points = OnePoint { local_point = cirp,
                                                      id = 0w0,
                                                      (* PERF uninitialized in Box2D *)
                                                      normal_impulse = 0.0,
                                                      tangent_impulse = 0.0 }}))
            else if u2 <= 0.0
            then (if distance_squared(c_local, v2) > radius * radius
                  then raise NoCollision
                  else SOME
                           (E_FaceA
                                { local_normal = vec2normalized(c_local :-: v2),
                                  local_point = v2,
                                  points = OnePoint { local_point = cirp,
                                                      id = 0w0,
                                                      (* PERF uninitialized in Box2D *)
                                                      normal_impulse = 0.0,
                                                      tangent_impulse = 0.0 }}))
            else let
                     val face_center : vec2 = 0.5 *: (v1 :+: v2)
                     val separation : real = dot2 (c_local :-: face_center,
                                                   Array.sub(normals, vert_index1))
                 in if separation > radius
                    then raise NoCollision
                    else SOME
                         (E_FaceA { local_normal = Array.sub(normals, vert_index1),
                                    local_point = face_center,
                                    points = OnePoint { local_point = cirp,
                                                        id = 0w0,
                                                        (* PERF uninitialized in Box2D *)
                                                        normal_impulse = 0.0,
                                                        tangent_impulse = 0.0 }})
                 end
        end
      end handle NoCollision => NONE

  (* Find the separation between poly1 and poly2 for a give edge normal on poly1. *)
  fun edge_separation (poly1 : BDDPolygon.polygon,
                       xf1 : transform,
                       edge1 : int,
                       poly2 : BDDPolygon.polygon,
                       xf2 : transform) : real =
      let
          val vertices1 = #vertices poly1
          val normals1 = #normals poly1

          val vertices2 = #vertices poly2

          (* PERF assert *)
          val () = if 0 <= edge1 andalso edge1 < Array.length vertices1
                   then ()
                   else raise BDDCollision "bad edge index"

          (* Convert normal from poly1's frame into poly2's frame. *)
          val normal1_world = transformr xf1 @*: Array.sub(normals1, edge1)
          val normal1 = mul_trotv (transformr xf2, normal1_world)

          (* Find support vertex on poly2 for -normal. *)
          val index = ref 0
          val min_dot = ref max_float

          val () = for 0 (Array.length vertices2 - 1)
              (fn i =>
               let val dot = dot2 (Array.sub(vertices2, i), normal1)
               in if dot < !min_dot
                  then (min_dot := dot;
                        index := i)
                  else ()
               end)

          val v1 = xf1 &*: Array.sub(vertices1, edge1)
          val v2 = xf2 &*: Array.sub(vertices2, !index)
      in
          dot2 (v2 :-: v1, normal1_world)
      end

  (* Find the max separation between poly1 and poly2 using edge normals from poly1. *)
  fun find_max_separation (poly1 : BDDPolygon.polygon,
                           xf1 : BDDMath.transform,
                           poly2 : BDDPolygon.polygon,
                           xf2 : BDDMath.transform) : int * real =
      let
          val normals1 = #normals poly1
          val count1 = Array.length normals1

          (* Vector pointing from the centroid of poly1 to the centroid of poly2. *)
          val d : vec2 = xf2 &*: #centroid poly2 :-: xf1 &*: #centroid poly1
          val d_local1 = mul_trotv (transformr xf1, d)

          (* Find edge normal on poly1 that has the largest projection onto d. *)
          val edge = ref 0
          val max_dot = ref (~max_float)
          val () = for 0 (count1 - 1)
              (fn i =>
               let val dot = dot2(Array.sub(normals1, i), d_local1)
               in if dot > !max_dot
                  then (max_dot := dot;
                        edge := i)
                  else ()
               end)

          val edge = !edge

          (* Get the separation for the edge normal. *)
          val s = edge_separation (poly1, xf1, edge, poly2, xf2)

          (* Check the separation for the previous edge normal. *)
          val prev_edge = if edge - 1 >= 0 then edge - 1 else count1 - 1
          val s_prev : real = edge_separation (poly1, xf1, prev_edge, poly2, xf2)

          (* Check the separation for the next edge normal. *)
          val next_edge = if edge + 1 < count1 then edge + 1 else 0
          val s_next : real = edge_separation(poly1, xf1, next_edge, poly2, xf2)

          (* Perform a local search for the best edge normal. *)
          fun search (best_edge, best_separation, forward) =
              let
                  val edge = if forward
                             then (if best_edge + 1 < count1
                                   then best_edge + 1 else 0)
                             else (if best_edge - 1 >= 0
                                   then best_edge - 1 else count1 - 1)
                  val s = edge_separation(poly1, xf1, edge, poly2, xf2)
              in
                  if s > best_separation
                  then search (edge, s, forward)
                  else (best_edge, best_separation)
              end

      in
          (* Find the best edge and the search direction, and search
             if we need to. *)
          if s_prev > s andalso s_prev > s_next
          then search (prev_edge, s_prev, false)
          else if s_next > s
               then search (next_edge, s_next, true)
               else (edge, s)
      end

  fun find_incident_edge (poly1 : BDDPolygon.polygon,
                          xf1 : transform,
                          edge1 : int,
                          poly2 : BDDPolygon.polygon,
                          xf2 : transform) : clip_vertex * clip_vertex =
      let 
          val normals1 = #normals poly1

          val vertices2 = #vertices poly2
          val normals2 = #normals poly2

          (* PERF assert *)
          val () = if 0 <= edge1 andalso edge1 < Array.length normals1
                   then ()
                   else raise BDDCollision "edge out of bounds"

          (* Get the normal of the reference edge in poly2's frame. *)
          val normal1 : vec2 = mul_trotv (transformr xf2,
                                          transformr xf1 @*: Array.sub(normals1, edge1))

          (* Find the incident edge on poly2. *)
          val index = ref 0
          val min_dot = ref max_float
          val () = for 0 (Array.length vertices2 - 1)
              (fn i =>
               let val dot = dot2 (normal1, Array.sub(normals2, i))
               in if dot < !min_dot
                  then (min_dot := dot;
                        index := i)
                  else ()
               end)

          (* Build the clip vertices for the incident edge. *)
          val i1 = !index
          val i2 = if i1 + 1 < Array.length vertices2
                   then i1 + 1
                   else 0
      in
          ({ v = xf2 &*: Array.sub(vertices2, i1),
             id = contact_id { index_a = edge1,
                               index_b = i1,
                               type_a = face_feature,
                               type_b = vertex_feature } },
           { v = xf2 &*: Array.sub(vertices2, i2),
             id = contact_id { index_a = edge1,
                               index_b = i2,
                               type_a = face_feature,
                               type_b = vertex_feature } })
      end

  (* PERF used for control flow. shouldn't carry diagnostic string data *)
  exception NoCollision of string
  (* Compute the collision manifold between two polygons. *)
  fun collide_polygons (polya : BDDPolygon.polygon,
                        xfa : BDDMath.transform,
                        polyb : BDDPolygon.polygon,
                        xfb : BDDMath.transform) : BDDTypes.manifold option =
    let
      (* Find edge normal of max separation on A - return if separating axis is found
         Find edge normal of max separation on B - return if separation axis is found
         Choose reference edge as min(minA, minB)
         Find incident edge
         Clip

         The normal points from 1 to 2 *)
      val total_radius = polygon_radius + polygon_radius

      val (edge_a, separation_a) = find_max_separation(polya, xfa, polyb, xfb)
      val () = if separation_a > total_radius
               then raise NoCollision "A"
               else ()
      val (edge_b, separation_b) = find_max_separation(polyb, xfb, polya, xfa)
      val () = if separation_b > total_radius
               then raise NoCollision "B"
               else ()

      val RELATIVE_TOL = 0.98
      val ABSOLUTE_TOL = 0.001

      (* 1: reference polygon, 2: incident polygon *)
      val (poly1, poly2, xf1, xf2, edge1, flip, is_face_b) =
          if separation_b > RELATIVE_TOL * separation_a + ABSOLUTE_TOL
          then (polyb, polya, xfb, xfa, edge_b, true, true)
          else (polya, polyb, xfa, xfb, edge_a, false, false)

      (* Port note: Original passes around array of two. This is better
         for ML, for sure: *)
      val (incident_edge1, incident_edge2) =
          find_incident_edge (poly1, xf1, edge1, poly2, xf2)
      val vertices1 = #vertices poly1
      val count1 = Array.length vertices1

      val iv1 = edge1
      val iv2 = if edge1 + 1 < count1 then edge1 + 1 else 0
      val v11 = Array.sub(vertices1, iv1)
      val v12 = Array.sub(vertices1, iv2)
      val local_tangent : vec2 = vec2normalized (v12 :-: v11)

      val local_normal : vec2 = cross2vs (local_tangent, 1.0)
      val plane_point = 0.5 *: (v11 :+: v12)
      val tangent : vec2 = mulrotv (transformr xf1, local_tangent)
      val normal : vec2 = cross2vs (tangent, 1.0)

      (* Port note: shadowing instead of assignment in original *)
      val v11 = xf1 &*: v11
      val v12 = xf1 &*: v12

      (* Face offset. *)
      val front_offset : real = dot2(normal, v11)

      (* Side offsets, extended by polytope skin thickness. *)
      val side_offset1 = ~(dot2(tangent, v11)) + total_radius
      val side_offset2 = dot2(tangent, v12) + total_radius

      (* Clip incident edge against extruded edge1 side edges. *)
      (* Clip to box side 1. *)
      val (cp1, cp2) =
          case clip_segment_to_line (incident_edge1,
                                     incident_edge2,
                                     vec2neg tangent,
                                     side_offset1,
                                     iv1) of
              NONE => raise NoCollision "side1"
            | SOME p => p
      (* Clip to box side 2. *)
      val (cp1, cp2) =
          case clip_segment_to_line (cp1, cp2,
                                     tangent,
                                     side_offset2,
                                     iv2) of
              NONE => raise NoCollision "side2"
            | SOME p => p

      (* Now cp1, cp2 are the clipped points. *)

      val points =
          List.mapPartial
          (fn cp =>
           let val separation : real = dot2(normal, #v cp) - front_offset
           in if separation <= total_radius
              then
                  let
                      val old_id = #id cp
                      val id = if flip
                               then contact_id {
                                    index_a = contact_id_index_b old_id,
                                    index_b = contact_id_index_a old_id,
                                    type_a = contact_id_type_b old_id,
                                    type_b = contact_id_type_a old_id
                                    }
                               else old_id
                  in
                      SOME { local_point = mul_ttransformv (xf2, #v cp),
                             id = id,
                             (* PERF uninitialized in Box2D *)
                             normal_impulse = 0.0,
                             tangent_impulse = 0.0 }
                  end
              else NONE
           end) [cp1, cp2]

      val points = case points of nil => raise NoCollision "no points"
                                | [cp1] => OnePoint cp1
                                | [cp1, cp2] => TwoPoints (cp1, cp2)
                                | _ => raise Fail "impossible"
      val result = {points = points, local_normal = local_normal, local_point = plane_point}
    in
        if is_face_b
        then SOME (E_FaceB result)
        else SOME (E_FaceA result)

    end handle NoCollision _ => NONE

end
