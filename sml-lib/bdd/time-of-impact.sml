(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Time of impact calculation.
   Corresponding to collision/b2timeofimpact.cpp. *)
structure BDDTimeOfImpact :> BDDTIME_OF_IMPACT =
struct
  open BDDSettings
  open BDDTypes
  open BDDMath
  open BDDOps
  infix 6 :+: :-: %-% %+% +++
  infix 7 *: *% +*: +*+ #*% @*: &*:

  (* Port note: Box2D contains some overall max iteration counts, which
     seem to be for diagnostics and tuning. I left them out. *)

  exception BDDTimeOfImpact of string

  val MAX_ITERATIONS = 20

  datatype state =
      SFailed
    | SOverlapped
    | STouching
    | SSeparated

  fun stos SFailed = "failed"
    | stos SOverlapped = "overlapped"
    | stos STouching = "touching"
    | stos SSeparated = "separated"

  datatype separation_type = TPoints | TFaceA | TFaceB
  type separation_function = 
      { proxya : distance_proxy,
        proxyb : distance_proxy,
        sweepa : sweep,
        sweepb : sweep,
        typ : separation_type,
        local_point : vec2,
        axis : vec2 }

  fun typtos TPoints = "points"
    | typtos TFaceA = "facea"
    | typtos TFaceB = "faceb"

  (* Port note: Original separates construction and initialization. 
     Initialization returns the magnitude of the axis, but it is unused. 
     Removed here. *)
  fun separation_function (cache, proxya, sweepa, proxyb, sweepb, t1) :
      separation_function =
    let
        val count = !(#count cache)
        (* PERF assertion *)
        val () = 
            if count > 0 andalso !(#count cache) < 3
            then ()
            else raise BDDTimeOfImpact "assertion failure"

        val xfa : transform = sweep_transform (sweepa, t1)
        val xfb : transform = sweep_transform (sweepb, t1)
    in
        if count = 1
        then 
            let
                val local_point_a = #vertex proxya (Array.sub(#indexa cache, 0))
                val local_point_b = #vertex proxyb (Array.sub(#indexb cache, 0))
                val point_a = xfa &*: local_point_a
                val point_b = xfb &*: local_point_b
                val axis = vec2normalized (point_b :-: point_a)
            in
                { typ = TPoints,
                  proxya = proxya,
                  proxyb = proxyb,
                  sweepa = sweepa,
                  sweepb = sweepb,
                  (* PERF uninitialized in original; consider datatype taking 
                     args *)
                  local_point = vec2 (0.0, 0.0),
                  axis = axis }
            end
        else if Array.sub(#indexa cache, 0) = Array.sub(#indexa cache, 1)
        then
            let
                (* Two points on B and one on A. *)
                val local_point_b1 : vec2 = #vertex proxyb (Array.sub(#indexb cache, 0))
                val local_point_b2 : vec2 = #vertex proxyb (Array.sub(#indexb cache, 1))

                val axis = vec2normalized
                               (cross2vs(local_point_b2 :-: local_point_b1, 1.0))
                val normal : vec2 = mulrotv (transformr xfb, axis)

                val local_point = 0.5 *: (local_point_b1 :+: local_point_b2)
                val point_b : vec2 = xfb &*: local_point

                val local_point_a = #vertex proxya (Array.sub(#indexa cache, 0))
                val point_a = xfa &*: local_point_a

                val axis = if dot2(point_a :-: point_b, normal) < 0.0
                           then vec2neg axis
                           else axis
            in
                { typ = TFaceB,
                  proxya = proxya,
                  proxyb = proxyb,
                  sweepa = sweepa,
                  sweepb = sweepb,
                  local_point = local_point,
                  axis = axis }
            end
         else
             let
                 (* Two points on A and one or two points on B. *)
                 val local_point_a1 : vec2 = #vertex proxya (Array.sub(#indexa cache, 0))
                 val local_point_a2 : vec2 = #vertex proxya (Array.sub(#indexa cache, 1))
                    
                 val axis = vec2normalized
                                (cross2vs(local_point_a2 :-: local_point_a1, 1.0))
                 val normal : vec2 = mulrotv (transformr xfa, axis)

                 val local_point = 0.5 *: (local_point_a1 :+: local_point_a2)
                 val point_a : vec2 = xfa &*: local_point

                 val local_point_b : vec2 = #vertex proxyb (Array.sub(#indexb cache, 0))
                 val point_b : vec2 = xfb &*: local_point_b

                 val axis = if dot2(point_b :-: point_a, normal) < 0.0
                            then vec2neg axis
                            else axis
             in
                 { typ = TFaceA,
                   proxya = proxya,
                   proxyb = proxyb,
                   sweepa = sweepa,
                   sweepb = sweepb,
                   local_point = local_point,
                   axis = axis }
             end
    end

  fun find_min_separation ({ proxya : distance_proxy,
                             proxyb : distance_proxy,
                             sweepa : sweep,
                             sweepb : sweep,
                             typ : separation_type,
                             local_point : vec2,
                             axis : vec2 } : separation_function, 
                           t : real) : real * int * int =
    let
        val xfa : transform = sweep_transform (sweepa, t)
        val xfb : transform = sweep_transform (sweepb, t)
    in
        case typ of
            TPoints =>
              let
                  val axis_a : vec2 = mul_trotv (transformr xfa, axis)
                  val axis_b : vec2 = mul_trotv (transformr xfb, vec2neg axis)

                  val indexa = #support proxya axis_a
                  val indexb = #support proxyb axis_b

                  val local_point_a : vec2 = #vertex proxya indexa
                  val local_point_b : vec2 = #vertex proxyb indexb

                  val point_a : vec2 = xfa &*: local_point_a
                  val point_b : vec2 = xfb &*: local_point_b

                  val separation : real = dot2(point_b :-: point_a, axis)
              in
                  (separation, indexa, indexb)
              end

         | TFaceA => 
              let
                  val normal : vec2 = mulrotv (transformr xfa, axis)
                  val point_a : vec2 = xfa &*: local_point
                  val axis_b : vec2 = mul_trotv (transformr xfb, vec2neg normal)

                  val indexa = ~1
                  val indexb = #support proxyb axis_b
                      
                  val local_point_b = #vertex proxyb indexb
                  val point_b = xfb &*: local_point_b

                  val separation = dot2(point_b :-: point_a, normal)
              in
                  (separation, indexa, indexb)
              end

         | TFaceB => 
              let
                  val normal = mulrotv (transformr xfb, axis)
                  val point_b = xfb &*: local_point

                  val axis_a : vec2 = mul_trotv(transformr xfa, vec2neg normal)
                  val indexb = ~1
                  val indexa = #support proxya axis_a

                  val local_point_a = #vertex proxya indexa
                  val point_a = xfa &*: local_point_a

                  val separation = dot2 (point_a :-: point_b, normal)
              in
                  (separation, indexa, indexb)
              end
    end

  fun evaluate ({ proxya : distance_proxy,
                  proxyb : distance_proxy,
                  sweepa : sweep,
                  sweepb : sweep,
                  typ : separation_type,
                  local_point : vec2,
                  axis : vec2 } : separation_function, 
                indexa : int, indexb : int, t : real) : real =
    let
        val xfa : transform = sweep_transform (sweepa, t)
        val xfb : transform = sweep_transform (sweepb, t)
    in
        dprint (fn () => "    ev: xfa " ^ xftos xfa ^ " xfb " ^ xftos xfb ^ 
               " t " ^ rtos t ^ " typ " ^ typtos typ ^ "\n");
        case typ of
            TPoints =>
              let
                  (* unused -twm
                  val axis_a : vec2 = mul_t22mv (transformr xfa, axis)
                  val axis_b : vec2 = mul_t22mv (transformr xfb, vec2neg axis)
                  *)

                  val local_point_a : vec2 = #vertex proxya indexa
                  val local_point_b : vec2 = #vertex proxyb indexb

                  val point_a = xfa &*: local_point_a
                  val point_b = xfb &*: local_point_b
              in
                  dot2(point_b :-: point_a, axis)
              end

          | TFaceA => 
              let
                  val normal : vec2 = mulrotv (transformr xfa, axis)
                  val point_a : vec2 = xfa &*: local_point

                  (* unused -twm
                  val axis_b : vec2 = mul_t22mv (transformr xfb, vec2neg normal)
                  *)

                  val local_point_b = #vertex proxyb indexb
                  val point_b = xfb &*: local_point_b
              in
                  dot2 (point_b :-: point_a, normal)
              end

          | TFaceB => 
              let 
                  val normal : vec2 = mulrotv (transformr xfb, axis)
                  val point_b = xfb &*: local_point

                  (* unused -twm
                  val axis_a : vec2 = mul_t22mv (transformr xfa, vec2neg normal)
                  *)
                  val local_point_a : vec2 = #vertex proxya indexa
                  val point_a : vec2 = xfa &*: local_point_a

                  val sep = dot2(point_a :-: point_b, normal)
              in
                  dprint (fn () =>
                          "    pa " ^ vtos point_a ^ " pb " ^ vtos point_b ^ " n " ^
                          vtos normal ^ " sep " ^ rtos sep ^ "\n");
                  sep
              end
    end

  (* CCD via the local separating axis method. This seeks progression
     by computing the largest time at which separation is maintained. *)
  fun time_of_impact { proxya : distance_proxy,
                       proxyb : distance_proxy,
                       sweepa : sweep,
                       sweepb : sweep,
                       (* Defines sweep interval [0, tmax] *)
                       tmax : real } : state * real =
    let
      (* ++b2_toiCalls *)

      (* Port note: Box2D initializes the output to tmax and unknown,
         but these assignments appear to be dead? *)

      (* Large rotations can make the root finder fail, so we normalize the
         sweep angles. *)
      val () = dprint (fn () => "TOI sweepa: " ^ sweeptos sweepa ^ "\n")
      val () = dprint (fn () => "TOI sweepb: " ^ sweeptos sweepb ^ "\n")
      val () = sweep_normalize sweepa
      val () = sweep_normalize sweepb

      val total_radius = #radius proxya + #radius proxyb
      val target = Real.max(linear_slop, total_radius - 3.0 * linear_slop)
      val tolerance = 0.25 * linear_slop

      val t1 = ref 0.0

      (* Prepare input for distance query. *)
      (* nb. uninitialized in Box2D. *)
      val cache = BDDDistance.initial_cache ()

      (* Port note: simulates the partially-initialized struct in Box2D. *)
      fun distance_input (xfa, xfb) = { proxya = proxya,
                                        proxyb = proxyb,
                                        use_radii = false,
                                        transforma = xfa,
                                        transformb = xfb }

      (* The outer loop progressively attempts to compute new separating axes.
         This loop terminates when an axis is repeated (no progress is made). *)
      fun outer_loop iter =
        let
            val () = dprint (fn () => "outer loop #" ^ itos iter ^ "\n")
            val () = dprint (fn () => "sa: " ^ sweeptos sweepa ^ " time " ^ rtos (!t1) ^ "\n")
            val xfa : transform = sweep_transform (sweepa, !t1)
            val xfb : transform = sweep_transform (sweepb, !t1)
            val () = dprint (fn () => "xfa: " ^ xftos xfa ^ "\n")

            (* FIXME HERE tom: I think that xfa and xfb have diverged, specifically
               the rotations. *)

            (* Get the distance between shapes. We can also use the results
               to get a separating axis. *)
            val { distance, ... } = 
                BDDDistance.distance (distance_input (xfa, xfb), cache)
        in
            dprint (fn () => "  toi distance: " ^ rtos distance ^ " targ " ^ rtos target ^ " tol " ^
                    rtos tolerance ^ "\n");
            (* If the shapes are overlapped, we give up on continuous collision. *)
            if distance < 0.0
            then (SOverlapped, 0.0) (* Failure! *)
            else if distance < target + tolerance
            then (dprint (fn () => "  toi initial touching\n");
                  (STouching, !t1) (* Victory! *))
            else
            let
                (* Initialize the separating axis. *)
                val fcn : separation_function = 
                    separation_function (cache, proxya, sweepa, proxyb, sweepb, !t1)

                (* Port note: Removed commented-out debugging code. *)

                (* Compute the TOI on the separating axis. We do this by successively
                   resolving the deepest point. This loop is bounded
                   by the number of vertices. *)

                (* Port note: 'done' variable implemented by returning SOME
                   from the loops when they're done. *)
                val t2 = ref tmax
                fun inner_loop push_back_iter =
                  let
                      (* Find the deepest point at t2. Store the witness point indices. *)
                      val (s2 : real, indexa : int, indexb : int) = 
                          find_min_separation (fcn, !t2)
                  in
                      dprint (fn () => "  inner_loop #" ^ itos push_back_iter ^ 
                              ": t2 " ^ rtos (!t2) ^ " s2 " ^ rtos s2 ^
                              " a " ^ itos indexa ^ " b " ^ itos indexb ^ "\n");

                      (* Is the final configuration separated? *)
                      if s2 > target + tolerance
                      then SOME (SSeparated, tmax) (* Victory! *)
                      (* Has the separation reached tolerance? *)
                      else if s2 > target - tolerance
                      (* XXX could maybe just make t1 and t2 into args
                         and make this a direct call to the outer loop *)
                      then (t1 := !t2; NONE) (* Advance the sweeps *)
                      else
                      let
                          (* Compute the initial separation of the witness points. *)
                          val s1 : real = evaluate (fcn, indexa, indexb, !t1)
                          val () = dprint (fn () => "    evaluated: " ^ rtos s1 ^ "\n");
                      in
                          (* Check for initial overlap. This might happen if the root finder
                             runs out of iterations. *)
                          if s1 < target - tolerance
                          then SOME (SFailed, !t1)
                          (* Check for touching *)
                          else if s1 <= target + tolerance
                          (* Victory! t1 should hold the TOI (could be 0.0). *)
                          then (dprint (fn () => "  toi touching at " ^ rtos (!t1) ^ "\n");
                                SOME (STouching, !t1))
                          else
                          let
                              (* Compute 1D root of: f(x) - target = 0 *)
                              fun root_loop (s1, s2, a1, a2, root_iters) =
                                  if root_iters < 50
                                  then 
                                  let
                                      (* Use a mix of the secant rule and bisection. *)
                                      val t : real =
                                          case root_iters mod 2 of
                                              (* Secant rule to improve convergence. *)
                                              1 => a1 + (target - s1) * (a2 - a1) / (s2 - s1)
                                              (* Bisection to guarantee progress *)
                                            | _ => 0.5 * (a1 + a2)
                                      val () = dprint (fn () => "      rl #" ^ itos root_iters ^
                                                      " s1 " ^ rtos s1 ^ " s2 " ^
                                                      rtos s2 ^ " a1 " ^ rtos a1 ^ 
                                                      " a2 " ^ rtos a2 ^ " t " ^
                                                      rtos t ^ "\n");
                                      val s : real = evaluate (fcn, indexa, indexb, t)
                                  in
                                      if Real.abs (s - target) < tolerance
                                      (* t2 holds a tentative value for t1 *)
                                      then t2 := t
                                      (* Ensure we continue to bracket the root. *)
                                      else if s > target
                                           then root_loop (s, s2, t, a2, root_iters + 1)
                                           else root_loop (s1, s, a1, t, root_iters + 1)
                                  end
                                  else ()
                          in
                              (* Modifies t2.
                                 PERF: What's the point of doing root_loop if
                                 push_back_iter is already max_polygon_vertices?
                                 the only result is modification of t2, which is
                                 dead in that case. *)
                              root_loop (s1, s2, !t1, !t2, 0);
                              if push_back_iter = max_polygon_vertices
                              then NONE
                              else inner_loop (push_back_iter + 1)
                          end
                      end
                  end
            in
                case inner_loop 0 of
                    NONE =>
                        if iter + 1 = MAX_ITERATIONS
                        (* Root finder got stuck. Semi-victory. *)
                        then (SFailed, !t1)
                        else outer_loop (iter + 1)
                  | SOME ret => ret
            end
        end

      val (s, r) = outer_loop 0
    in
      dprint (fn () => "  toi returns: " ^ stos s ^ ": " ^ rtos r ^ "\n");
      (s, r)
    end
end
