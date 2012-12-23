(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* A dynamic AABB tree for the broad-phase collision detection.
   Corresponding to collision/b2dynamictree.cpp. *)
structure BDDDynamicTree :> BDDDYNAMIC_TREE =
struct
  open BDDSettings
  open BDDTypes
  open BDDMath
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*:

  exception BDDDynamicTree of string

  (* Port note: Corresponds to DynamicTreeNode in b2dynamictree.h.
     I decided to use more idiomatic SML instead of the hand-written
     allocator in Box2D, since it's not clear it'd be faster than
     a good generational gc.

     PERF: As I worked through it I realized there are some representation
     invariants that could be enforced with types, which would make the
     code clearer and would remove some of the awkward re-testing !! stuff.
     It deserves another pass. *)

  and datatype 'a tree_node =
      Node of { (* Fattened aabb *)
                aabb : aabb,
                (* Port note: Box2D has a possibility for
                   a 'next' pointer here, but it's just
                   so that the structure can be stored 
                   in freelists for its custom allocator. *)
                parent : 'a tree_node option ref,
                left : 'a tree_node ref,
                right : 'a tree_node ref }
    | Leaf of  {
      (* Fattened aabb *)
      aabb : aabb,
      stamp : int,
      data : 'a,
      parent : 'a tree_node option ref
      }

  type 'a aabb_proxy = 'a tree_node ref

  (* Port note: The representation is that leaf nodes are the
     "real" nodes (and have user data) whereas internal nodes just
     union up leaves to arrange them hierarchically, and are expendable.
     It is probably worth having both Interior and Leaf arms rather
     than Node and Empty. *)
  (* Represent the whole thing as a ref so that we don't confuse the
     updateable root pointer with the identity of the node contained
     there. *)
  type 'a dynamic_tree =
      { node_count : int,
        path : Word32.word,
        root : 'a tree_node option } ref

  (* PERF just for debugging. *)
  fun checkstructure s (r as ref (Leaf _ )) = ()
    | checkstructure s (r as ref (Node { left, right, ... })) =
      let
          fun checkeq NONE r =
              raise BDDDynamicTree
                        ("checkstructure " ^ s ^ ": node's " ^
                         which ^ " child's parent is NONE")
            | checkeq (SOME p) r =
              if p = r
              then ()
              else raise BDDDynamicTree
                  ("checkstructure " ^ s ^ ": node's " ^
                   which ^ " child's parent isn't node")
          fun checkpar which (Leaf { parent, ... }) = checkeq parent r
            | checkpar which (Node { parent, ... }) = checkeq parent r

      in
          checkpar "left" (!left);
          checkpar "right" (!right);
          checkstructure s left;
          checkstructure s right
      end

  fun checktreestructure s (ref { node_count : int, path : Word32.word,
                                  root : 'a aabb_proxy option }) =
      case root of
          NONE => ()
        | SOME pxy => checkstructure s pxy

  fun checktreestructure _ _ = ()

  fun debugprint pa (tree as ref { node_count, path, root }) =
      let
          fun indent 0 = ()
            | indent n = (dprint (fn () => " "); indent (n - 1))
          fun pr (depth, Empty) =
              let in
                  indent depth;
                  dprint (fn () => "Empty\n")
              end
            | pr (depth, Leaf { data = dat, aabb, ... }) =
              let in
                  indent depth;
                  dprint (fn () => "Leaf: " ^ aabbtos aabb ^ "\n");
                  indent depth;
                  dprint (fn () => " dat: " ^ pa dat ^ "\n");
              end
            | pr (depth, Node { left, right, aabb, ... }) =
              let in
                  indent depth;
                  dprint (fn () => "Node: " ^ aabbtos aabb ^ "\n");
                  pr (depth + 2, !left);
                  pr (depth + 2, !right)
              end
      in
          dprint (fn () => "DT: " ^ Int.toString node_count ^ " " ^
                 Word32.toString path ^ ":\n");
          pr (0, !root);
          checktreestructure "dp" tree
      end

  fun cmp_proxy (ref (Leaf { stamp = a, ... }),
                 ref (Leaf { stamp = b, ... })) = Int.compare (a, b)
    | cmp_proxy _ = raise BDDDynamicTree "can only compare leaves."

  fun eq_proxy p = EQUAL = cmp_proxy p

  local
      val next_stamp_ = ref 0
  in
      fun next_stamp () = (next_stamp_ := !next_stamp_ + 1;
                           !next_stamp_)
  end
  (* Port note: We need to update fields of each object to mimic the
     imperative style of Box2D. We treat the object itself as a ref,
     rather than the updatable fields (the latter would be more
     idiomatic in SML). This allows us to quickly compare the objects
     for equality, but means that we need setter functions for the
     fields that we modify. *)
  fun set_node_count (r as ref { node_count = _, root, path }, node_count) =
      r := { node_count = node_count, root = root, path = path }

  fun set_root (r as ref { node_count, root = _, path }, root) =
      r := { node_count = node_count, root = root, path = path }

  fun set_path (r as ref { node_count, root, path = _ }, path) =
      r := { node_count = node_count, root = root, path = path }

  fun set_parent (r as ref (Node { aabb, parent = _, left, right}),
                  parent) =
      r := Node { aabb = aabb, parent = parent,
                  left = left, right = right }
    | set_parent (r as ref (Leaf {aabb, data, stamp, parent = _}),
                  parent) =
      r := Leaf { aabb = aabb, data = data, stamp = stamp, parent = parent }

  fun set_left (r as ref (Node { aabb, parent, left = _, right }),
                left) =
      r := Node { aabb = aabb, parent = parent,
                  left = left, right = right }
    | set_left _ = raise BDDDynamicTree "expected node; got Leaf"

  fun set_right (r as ref (Node { aabb, parent, left, right = _ }),
                 right) =
      r := Node { aabb = aabb, parent = parent,
                  left = left, right = right }
    | set_right _ = raise BDDDynamicTree "expected node; got Leaf"

  fun user_data (ref (Leaf {data, ... })) = data
    | user_data (ref (Node _ )) =
      raise BDDDynamicTree "data only present for leaves"

  fun fat_aabb (ref (Leaf {aabb, ...})) = aabb
    | fat_aabb (ref (Node {aabb, ...})) = aabb

  fun compute_height (ref { root, ... } : 'a dynamic_tree) =
      let fun ch (Leaf _) = 0
            | ch (Node { left, right, ... }) =
          1 + Int.max(ch (!left), ch (!right))
      in case !root of
             NONE => 0
           | SOME (ref tn) => ch tn
      end

  fun dynamic_tree () : 'a dynamic_tree =
      ref { node_count = 0, root = NONE, path = 0w0 }

  (* Derive the AABB for an interior node, based on its children
     (which must have accurate AABBs. Set it and return it. *)
  fun set_derived_aabb (r as ref (Node { aabb = _, parent,
                                         left, right })) =
      let val new_aabb =
          BDDCollision.aabb_combine (#aabb (!!left), #aabb (!!right))
      in r := Node { aabb = new_aabb, parent = parent,
                     left = left, right = right };
          new_aabb
      end
    | set_derived_aabb _ = raise BDDDynamicTree "expected Node; got Leaf"

  (* Climb the tree starting at the given node, expanding the derived
     AABBs if necessary.
     Port note: In the original, usually inlined as a do..while loop.
     PERF: Easy to merge this and the above to remove some repeated
     tests (though they are probably optimized out). *)
  fun adjust_aabbs ancestor =
      case !ancestor of
        | Node { parent, aabb = old_aabb, ... } =>
           let val new_aabb = set_derived_aabb ancestor
           in if BDDCollision.aabb_contains (old_aabb, new_aabb)
              then ()
              else adjust_aabbs parent
           end
        | Leaf _ = raise BDDDynamicTree "expected Node; got Leaf"

  fun insert_leaf (tree as ref { root, ... } : 'a dynamic_tree,
                   leaf as Leaf { aabb, data = _, parent = _,
                                  stamp = _ })) =
      (case !root of
           NONE =>
               let in
                   (* PERF should always be the case already? *)
                   set_parent (leaf, ref NONE);
                   set_root (tree, leaf)
               end
         | SOME root =>
            (* Find the best sibling for this leaf. *)
            let
                val center : vec2 = BDDCollision.aabb_center aabb
                fun find (sibling as Leaf _) = sibling
                  | find (sibling as Node {aabb, parent, left, right}) =
                    let
                      val ldelta : vec2 =
                        vec2abs (BDDCollision.aabb_center (#aabb (!!left)) :-: center)
                      val rdelta : vec2 =
                        vec2abs (BDDCollision.aabb_center (#aabb (!!right)) :-: center)
                      val lnorm = vec2x ldelta + vec2y ldelta
                      val rnorm = vec2x rdelta + vec2y rdelta
                    in
                      if lnorm < rnorm
                      then find left
                      else find right
                    end
                val sibling = find root
                val parent = #parent (!!sibling)
                val new = ref (Node { parent = parent,
                                      aabb = BDDCollision.aabb_combine 
                                          (#aabb (!!leaf),
                                           #aabb (!!sibling)),
                                      (* Port note: Same in both branches. *)
                                      left = sibling,
                                      right = leaf })
            in
                set_parent (sibling, new);
                set_parent (leaf, new);

                case !parent of
                  Empty => set_root (tree, new)
                | _ => 
                  let
                  in
                      if #left (!!parent) = sibling
                      then set_left (parent, new)
                      else set_right (parent, new);

                      (* Port note: This expansion routine is not exactly 
                         the same as the one in the code, but I believe
                         it has equivalent effect. *)
                      adjust_aabbs parent
                  end
            end)
    | insert_leaf _ = raise BDDDynamicTree "can't insert interior node or empty"

  (* Assumes the proxy is a leaf. *)
  fun remove_leaf (tree : 'a dynamic_tree, 
                   proxy : 'a aabb_proxy) =
    let val { parent, ... } = !!proxy
    in
      checktreestructure "remove_leaf before" tree;
      (* If it's the root, we just make the tree empty. 
         Port note: Throughout this code, Box2D uses equality
         on proxy IDs (integers); I ref equality. *)
      (case !parent of
          Empty => set_root (tree, ref Empty)
        | Node { left, right, parent = grandparent, ... } =>
            let
                (* Get the other child of our parent. *)
                val sibling = if left = proxy 
                              then right
                              else left
            in
              case !grandparent of
                  (* Note: discards parent. *)
                  Empty => (set_parent (sibling, ref Empty);
                            set_root (tree, sibling))
                | Node { left = g_left, ... } => 
                      let 
                      in
                          (* Destroy parent and connect grandparent
                             to sibling. *)
                          if g_left = parent
                          then set_left (grandparent, sibling)
                          else set_right (grandparent, sibling);
                          set_parent (sibling, grandparent);
                          (* Adjust ancestor bounds. *)
                          adjust_aabbs grandparent
                      end
            end);
       checktreestructure "remove_leaf after" tree
    end

  fun rebalance (tree : 'a dynamic_tree, iters : int) =
    case !(#root (!tree)) of
      Empty => ()
    | _ =>
      let
          (* Port note: Rebalancing consists of finding leaves and reinserting
             them. The member variable path is some kinda magic that seems
             to be intended to make us choose different nodes each time; we
             read bits from least to most significant so that we switch
             between the two children of the root on every step, etc. *)
          val path = ref (#path (!tree))
      in
          checktreestructure "rebalance before" tree;
          for 1 iters
          (fn _ =>
           let fun loop (node, bit) =
               if is_leaf node
               then (path := !path + 0w1; 
                     remove_leaf (tree, node); 
                     insert_leaf (tree, node))
               else
                let
                    val node =
                        case Word32.andb(0w1, Word32.>>(!path, bit)) of
                            0w0 => #left (!!node)
                          | _ => #right (!!node)
                    val bit = Word.andb(bit + 0w1, 0w31)
                in
                    path := !path + 0w1;
                    loop (node, bit)
                end
           in
               loop (#root (!tree), 0w0)
           end);
           set_path (tree, !path);
           checktreestructure "rebalance after" tree
      end

  fun aabb_proxy (tree : 'a dynamic_tree, aabb : aabb, a : 'a) : 'a aabb_proxy =
      let
          fun pxy v = 
              Real.fmt (StringCvt.FIX (SOME 2)) (vec2x v) ^ " " ^
              Real.fmt (StringCvt.FIX (SOME 2)) (vec2y v)
          val () = dprint (fn () => "  inc aabb: " ^
                          pxy (#lowerbound aabb) ^ " to " ^
                          pxy (#upperbound aabb) ^ "\n")


          (* Fatten the aabb. *)
          val r : vec2 = vec2(aabb_extension, aabb_extension)
          val fat : aabb = { lowerbound = #lowerbound aabb :-: r,
                             upperbound = #upperbound aabb :+: r }

          val leaf = { aabb = fat, data = a, parent = ref Empty,
                       stamp = next_stamp () }

          fun rebalance_loop try_count =
              if try_count >= 10 orelse compute_height tree <= 64
              then node
              else (rebalance (tree, #node_count (!tree) div 16);
                    rebalance_loop (try_count + 1))
      in
          set_node_count (tree, #node_count (!tree) + 1);
          insert_leaf (tree, leaf);
          (* Rebalance if necessary. *)
          rebalance_loop 0
      end

  fun remove_proxy (tree : 'a dynamic_tree, proxy : 'a aabb_proxy) =
      if is_leaf proxy
      then remove_leaf (tree, proxy)
      else raise BDDDynamicTree "can only remove leaves"

  fun move_proxy (tree : 'a dynamic_tree,
                  proxy as ref (Node { aabb = proxy_aabb, 
                                       data, stamp, ... }) : 'a aabb_proxy,
                  aabb : aabb,
                  displacement : vec2) : bool =
      if BDDCollision.aabb_contains (proxy_aabb, aabb)
      then false
      else
        let
            val () = if is_leaf proxy
                     then ()
                     else raise BDDDynamicTree "move_proxy on non-leaf"
            val () = remove_leaf (tree, proxy)

            (* Predict AABB displacement. *)
            val d : vec2 = aabb_multiplier *: displacement
            val r : vec2 = vec2(aabb_extension, aabb_extension)

            val (blx, bux) = if vec2x d < 0.0
                             then (vec2x d, 0.0)
                             else (0.0, vec2x d)
            val (bly, buy) = if vec2y d < 0.0
                             then (vec2y d, 0.0)
                             else (0.0, vec2y d)

            (* Extend AABB *)
            val b : aabb = { lowerbound = 
                               #lowerbound aabb :-: r :+: vec2(blx, bly),
                             upperbound = 
                               #upperbound aabb :+: r :+: vec2(bux, buy) }

            fun pxy v = 
                Real.fmt (StringCvt.FIX (SOME 2)) (vec2x v) ^ " " ^
                Real.fmt (StringCvt.FIX (SOME 2)) (vec2y v)
            val () = dprint (fn () => "  moved_aabb: " ^
                             pxy (#lowerbound b) ^ " to " ^
                             pxy (#upperbound b) ^ "\n")

        in
            proxy := Node { aabb = b, data = data, parent = ref Empty,
                            left = ref Empty, right = ref Empty, stamp = stamp };
            insert_leaf (tree, proxy);
            true
        end
    | move_proxy _ = raise BDDDynamicTree "move_proxy on Empty"

  (* Port note: Box2D somewhat strangely uses an explicit stack here
     (might be so that it can abort when the callback returns false
     without using setjmp), which has a maximum depth 128. There's not
     really any reason that the tree can't have an all-left path of
     length greater than 64 (which creates 128 outstanding nodes), at
     which point this function would stop looking at children.
     Rebalancing should prevent that most of the time, but it's better
     to be correct. Implemented instead using an unlimited ML stack
     and exceptions for early exits. *)
  exception Done
  fun query (tree : 'a dynamic_tree,
             f : 'a aabb_proxy -> bool,
             aabb : aabb) : unit =
    let fun q node =
        case !node of
            Empty => ()
          | Node { left, right, aabb = node_aabb, ... } =>
            if BDDCollision.aabb_overlap (node_aabb, aabb)
            then if is_leaf node
                 then if f node
                      then ()
                      else raise Done
                 else (q left; q right)
            else ()
    in
        q (#root (!tree))
    end handle Done => ()

  fun ray_cast (tree : 'a dynamic_tree,
                f : BDDTypes.ray_cast_input * 'a aabb_proxy -> real,
                { p1 : BDDMath.vec2, p2 : BDDMath.vec2,
                  max_fraction : real }) : unit =
    let
      val r0 : vec2 = p2 :-: p1
      val () = if vec2length_squared r0 > 0.0
               then ()
               else raise BDDDynamicTree "ray must have length"
      val r = vec2normalized r0

      (* v is perpendicular to the segment. *)
      val v : vec2 = cross2sv(1.0, r)
      val abs_v : vec2 = vec2abs v


      (* These two are updated in the loop. *)
      val max_fraction = ref max_fraction
      (* Build a bounding box for the segment. *)
      fun make_segment () =
          let val t : vec2 = p1 :+: !max_fraction *: (p2 :-: p1)
          in
              { lowerbound = vec2min(p1, t),
                upperbound = vec2max(p1, t) }
          end
      val segment_aabb : aabb ref = ref (make_segment ())

      (* Port note: Again with the explicit stack. Prevents descent deeper
         than 64 in the worst case. Just use the ML stack and
         exceptions for correctness and simplicity. *)
      fun loop node =
        case !node of
            Empty => ()
          | Node { aabb = node_aabb, ... } =>
            if not (BDDCollision.aabb_overlap (node_aabb, !segment_aabb))
            then ()
            else
             let
               (* Separating axis for segment (Gino, p80).
                  |dot(v, p1 - c)| > dot(|v|, h) *)
               val c : vec2 = BDDCollision.aabb_center node_aabb
               val h : vec2 = BDDCollision.aabb_extents node_aabb
               val separation : real = Real.abs(dot2(v, p1 :-: c)) - dot2(abs_v, h)
             in
               if separation > 0.0
               then ()
               else
                   if is_leaf node
                   then
                       let
                           val sub_input = { p1 = p1, p2 = p2, 
                                             max_fraction = !max_fraction }
                           val value = f (sub_input, node)
                       in
                           (* Just used as a sentinel for the client to
                              request that the ray cast should stop *)
                           if Real.== (value, 0.0)
                           then raise Done
                           else if value > 0.0
                                then
                                    let in
                                        (* update segment bounding box. *)
                                        max_fraction := value;
                                        segment_aabb := make_segment()
                                    end
                                else ()
                       end
                   else (loop (#left (!!node)); loop (#right (!!node)))
             end

    in
        loop (#root (!tree))
    end handle Done => ()

end
