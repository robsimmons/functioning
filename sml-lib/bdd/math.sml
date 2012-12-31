(* Copyright 2010 Tom Murphy VII and Erin Catto. See COPYING for details. *)

(* Math types and utilities used throughout BDD.
   Corresponding to common/b2math.h. *)
structure BDDMath :> BDDMATH =
struct

  exception BDDMath of string

  (* XXX *)
  fun rtos r = Real.fmt (StringCvt.FIX (SOME 4)) r

  (* This function is used to ensure that a floating point number is
     not a NaN or infinity. *)
  val is_valid = Real.isFinite (* checks both NaN and infs. *)

  (* This is a approximate yet fast inverse square-root. *)
  fun inv_sqrt x = 1.0 / Math.sqrt x
  (* PERF: Use magic 0x5f3759df method.
{
        union
        {
                float32 x;
                int32 i;
        } convert;

        convert.x = x;
        float32 xhalf = 0.5f * x;
        convert.i = 0x5f3759df - (convert.i >> 1);
        x = convert.x;
        x = x * (1.5f - xhalf * x * x);
        return x;
}
*)

  val sqrt = Math.sqrt
  val atan2 = Math.atan2
  val abs = Real.abs

  (* Doesn't seem to work. Triggers assertions. *)
  (*
  local
      val B = 1.273239545 (* 4/pi *)
      val C = ~0.405284734569 (* -4/(pi^2) *)
      val P = 0.225 (* empirical; weighted fraction
                       between the parabola and
                       its square *)
  in
      fun fastsin (x : real) =
        let 
            val y = B * x + C * x * abs x
            (* Adds precision *)
            val y = P * (y * abs y - y) + y
        in
            y
        end
  end
*)

  (* A 2D column vector. *)
  type vec2 = { x : real, y : real }

  fun vec2 (x, y) = { x = x, y = y }
  fun vec2x ({x, y = _} : vec2) = x
  fun vec2y ({x = _, y} : vec2) = y
  fun vec2xy {x, y} = (x, y)
  fun vec2neg ({x, y} : vec2) = { x = (0.0 - x), y = (0.0 - y) }
  fun vec2idx ({x, y = _} : vec2) 0 = x
    | vec2idx {x = _, y} 1 = y
    | vec2idx _ _ = raise Subscript
  fun vec2length {x, y} = sqrt(x * x + y * y)

  fun vec2length_squared ({x, y} : vec2) = x * x + y * y

  fun vec2normalized (v as {x, y}) : vec2 =
      let val length = vec2length v
      in if length < BDDSettings.epsilon
         then v
         else let val inv = 1.0 / length
              in vec2 (x * inv, y * inv)
              end
      end

  fun vec2is_valid {x, y} =
      is_valid x andalso is_valid y

  type vec3 = { x : real, y : real, z : real }
  fun vec3 (x, y, z) = { x = x, y = y, z = z }
  fun vec3x ({x, y = _, z = _} : vec3) = x
  fun vec3y ({x = _, y, z = _} : vec3) = y
  fun vec3z ({x = _, y = _, z} : vec3) = z
  fun vec3xyz {x, y, z} = (x, y, z)
  fun vec3neg ({x, y, z} : vec3) = { x = (0.0 - x),
                                     y = (0.0 - y),
                                     z = (0.0 - z) }
  fun vec3idx ({x, y = _, z = _} : vec3) 0 = x
    | vec3idx {x = _, y, z = _} 1 = y
    | vec3idx {x = _, y = _, z} 2 = z
    | vec3idx _ _ = raise Subscript

  (* 2x2 matrix; column-major order. *)
  type mat22 = { col1 : vec2, col2 : vec2 }

  fun mat22cols (col1, col2) = { col1 = col1,
                                 col2 = col2 }

  fun mat22with (a11, a12,
                 a21, a22) =
      { col1 = vec2(a11,
                    a21),
                            col2 = vec2(a12,
                                        a22) }

  fun mat22col1 { col1, col2 = _ } = col1
  fun mat22col2 { col1 = _, col2 } = col2

  (* Construct this matrix using an angle. This matrix becomes
     an orthonormal rotation matrix. *)
  fun mat22angle angle =
      (* PERF compute sin and cos together *)
      let val c = Math.cos angle
          val s = Math.sin angle
      in
          (* FUN BUG "right": if this is (c, s, ~s, c), then polygons stand
             themselves up instead of falling over (angular impulses
             are reversed). *)
          mat22with (c, ~s,
                     s, c)
      end

  fun mat22getangle ({ col1, col2 = _ } : mat22) =
      atan2 (vec2y col1, vec2x col1)

  fun mat22inverse ({ col1, col2 } : mat22) =
      let
          val a = vec2x col1
          val b = vec2x col2
          val c = vec2y col1
          val d = vec2y col2
          val det = a * d - b * c
          val det = if Real.!= (det, 0.0)
                    then 1.0 / det
                    else det
      in
          mat22with(det * d, ~det * b,
                    ~det * c, det * a)
      end

  (* Solve A * x = b, where b is a column vector. This is more efficient
     than computing the inverse in one-shot cases. *)
  fun mat22solve ({ col1, col2 }, b : vec2) : vec2 =
      let
          val a11 = vec2x col1   val a12 = vec2x col2
          val a21 = vec2y col1   val a22 = vec2y col2

          val det = a11 * a22 - a12 * a21
          val det = if Real.!= (det, 0.0)
                    then 1.0 / det
                    else det
      in
          vec2(det * (a22 * vec2x b - a12 * vec2y b),
               det * (a11 * vec2y b - a21 * vec2x b))
      end


  type mat33 = { col1 : vec3, col2 : vec3, col3 : vec3 }

  fun mat33cols (col1, col2, col3) = { col1 = col1, col2 = col2, col3 = col3 }

  fun mat33with  (a11, a12, a13,
                  a21, a22, a23,
                  a31, a32, a33) =
      { col1 = vec3(a11,
                    a21,
                    a31),
                            col2 = vec3(a12,
                                        a22,
                                        a32),
                                              col3 = vec3(a13,
                                                          a23,
                                                          a33)}

  fun mat33col1 { col1, col2 = _, col3 = _ } = col1
  fun mat33col2 { col1 = _, col2, col3 = _ } = col2
  fun mat33col3 { col1 = _, col2 = _, col3 } = col3

  fun dot2(a : vec2, b : vec2) : real =
      vec2x a * vec2x b + vec2y a * vec2y b
  fun dot3(a : vec3, b : vec3) : real =
      vec3x a * vec3x b + vec3y a * vec3y b + vec3z a * vec3z b

  fun cross2vv(a : vec2, b : vec2) : real = 
      vec2x a * vec2y b - vec2y a * vec2x b
  fun cross2vs(a : vec2, s : real) : vec2 =
      vec2(s * vec2y a, ~s * vec2x a)
  fun cross2sv(s : real, a : vec2) : vec2 =
      vec2(~s * vec2y a, s * vec2x a)

  fun cross3vv(a : vec3, b : vec3) : vec3 =
      vec3 (vec3y a * vec3z b - vec3z a * vec3y b, 
            vec3z a * vec3x b - vec3x a * vec3z b, 
            vec3x a * vec3y b - vec3y a * vec3x b)

  fun mat33solve33 ({ col1, col2, col3 }, b : vec3) : vec3 =
      let
          val cross23 = cross3vv (col2, col3)
          val det = dot3 (col1, cross23)
          val det = if Real.!= (det, 0.0)
                    then 1.0 / det
                    else det
      in
          vec3(det * dot3(b, cross23),
               det * dot3(col1, cross3vv(b, col3)),
               det * dot3(col1, cross3vv(col2, b)))
      end

  fun mat33solve22 ({ col1, col2, col3 = _ } : mat33, b : vec2) : vec2 =
      let
          val a11 = vec3x col1     val a12 = vec3x col2
          val a21 = vec3y col1     val a22 = vec3y col2
       
          val det = a11 * a22 - a12 * a21
          val det = if Real.!= (det, 0.0)
                    then 1.0 / det
                    else det
      in
          vec2(det * (a22 * vec2x b - a12 * vec2y b),
               det * (a11 * vec2y b - a21 * vec2x b))
      end

  type rotation = { c : real, s : real }

  fun rotation angle = { c = Math.cos angle, s = Math.sin angle }
  val rotation_identity = { c = 1.0, s = 0.0 }
  fun rotation_getangle { c, s } = atan2 (s, c)
  fun rotation_getxaxis { c, s } = vec2 (c, s)
  fun rotation_getyaxis { c : real, s : real } = vec2 (~s, c)

  type transform = { position : vec2, r : rotation }
  fun transform (pp, rr) = { position = pp, r = rr }
  fun transform_pos_angle (pp, angle : real) =
      { position = pp,
        r = rotation angle }
  fun transformposition { position, r = _ } = position
  fun transformr ({ position = _, r } : transform) = r

  fun transform_getangle { position = _, r } = rotation_getangle r

  val vec2_zero : vec2 = vec2(0.0, 0.0)
  val mat22_identity : mat22 = mat22with(1.0, 0.0,
                                         0.0, 1.0)
  val transform_identity = { position = vec2 (0.0, 0.0) : vec2,
                             r = rotation_identity }

  (* Functional math on vectors, matrices, etc. *)

  fun vec2sub (a : vec2, b : vec2) : vec2 =
      vec2 (vec2x a - vec2x b, vec2y a - vec2y b)

  fun vec3sub (a : vec3, b : vec3) : vec3 =
      vec3 (vec3x a - vec3x b,
            vec3y a - vec3y b,
            vec3z a - vec3z b)

  fun vec2add (a : vec2, b : vec2) : vec2 =
      vec2 (vec2x a + vec2x b, vec2y a + vec2y b)

  fun vec3add (a : vec3, b : vec3) : vec3 =
      vec3 (vec3x a + vec3x b,
            vec3y a + vec3y b,
            vec3z a + vec3z b)

  fun mat22add (a : mat22, b : mat22) : mat22 =
      mat22cols (vec2add (#col1 a, #col1 b), vec2add (#col2 a, #col2 b)) 

  fun vec2stimes (s : real, v : vec2) : vec2 =
      vec2 (s * vec2x v, s * vec2y v)
  fun vec3stimes (s : real, v : vec3) : vec3 =
      vec3 (s * vec3x v, s * vec3y v, s * vec3z v)

  fun mul22v (a : mat22, v : vec2) : vec2 =
      vec2 (vec2x (#col1 a) * vec2x v +
            vec2x (#col2 a) * vec2y v,
            vec2y (#col1 a) * vec2x v +
            vec2y (#col2 a) * vec2y v)

  fun mul22m (a : mat22, b : mat22) : mat22 =
      mat22cols (mul22v(a, #col1 b), mul22v(a, #col2 b))


  fun mul33v (a : mat33, v : vec3) : vec3 =
      vec3add(vec3add (vec3stimes (vec3x v, #col1 a),
                       vec3stimes (vec3y v, #col2 a)),
              vec3stimes (vec3z v, #col3 a))

  fun mulrot (q : rotation, r : rotation) =
      { c = #c q * #c r - #s q * #s r ,
        s = #s q * #c r + #c q * #c r }

  fun mulrotv (q : rotation, v : vec2) =
      vec2 (#c q * vec2x v - #s q * vec2y v,
            #s q * vec2x v + #c q * vec2y v)

  fun multransformv (t : transform, v : vec2) : vec2 =
      let
          val r = transformr t
          val x = vec2x (transformposition t) +
                  (#c r * vec2x v - #s r * vec2y v)
          val y = vec2y (transformposition t) +
                  (#s r * vec2x v + #c r * vec2y v)
      in
        vec2 (x, y)
      end

  fun vec2eq (a, b) = Real.== (vec2x a, vec2x b) andalso
      Real.== (vec2y a, vec2y b)

  fun vec2abs v = vec2 (abs (vec2x v), abs (vec2y v))
  fun mat22abs (a : mat22) = mat22cols (vec2abs (#col1 a), vec2abs (#col2 a))

  fun distance(a : vec2, b : vec2) =
      vec2length (vec2sub (a, b))

  fun distance_squared (a, b) =
      let val c = vec2sub (a, b)
      in dot2 (c, c)
      end

  fun vec2min (a : vec2, b : vec2) =
      vec2 (Real.min (vec2x a, vec2x b),
            Real.min (vec2y a, vec2y b))

  fun vec2max (a : vec2, b : vec2) =
      vec2 (Real.max (vec2x a, vec2x b),
            Real.max (vec2y a, vec2y b))

  fun clampr (a, low, high) =
      Real.max(low, Real.min(a, high))

  fun vec2clamp (a : vec2, low : vec2, high : vec2) : vec2 =
      vec2max (low, vec2min(a, high))

  (* These multiply the transpose of the matrix! *)
  fun mul_t22mv (a : mat22, v : vec2) : vec2 =
      vec2(dot2(v, #col1 a), dot2(v, #col2 a))
  fun mul_t22mm (a : mat22, b : mat22) : mat22 =
      let val c1 = vec2(dot2(#col1 a, #col1 b), dot2(#col2 a, #col1 b))
          val c2 = vec2(dot2(#col1 a, #col2 b), dot2(#col2 a, #col2 b))
      in
          mat22cols (c1, c2)
      end

  fun mul_trotv (q : rotation, v : vec2) =
      vec2 (#c q * vec2x v + #s q * vec2y v,
            ~(#s q) * vec2x v + #c q * vec2y v)

  fun mul_ttransformv (t : transform, v : vec2) : vec2 =
      mul_trotv (transformr t, vec2sub (v, transformposition t))

  (* Cool trick: By downshifting and oring, ensure that the bits are all ones
     starting with the first one in the input. Then the result plus 1 is the
     next largest power of two. *)
  fun next_power_of_two (w : Word32.word) =
      let
          val w = Word32.orb(w, Word32.>>(w, 0w1))
          val w = Word32.orb(w, Word32.>>(w, 0w2))
          val w = Word32.orb(w, Word32.>>(w, 0w4))
          val w = Word32.orb(w, Word32.>>(w, 0w8))
          val w = Word32.orb(w, Word32.>>(w, 0w16))
      in
          w + 0w1
      end

  fun is_power_of_two (w : Word32.word) =
      w <> 0w0 andalso Word32.andb(w, w - 0w1) = 0w0


  type sweep = {
                 (* local center of mass position *)
                 local_center : vec2 ref,
                 (* center world positions *)
                 c0 : vec2 ref,
                 c : vec2 ref,
                 (* world angles *)
                 a0 : real ref,
                 a : real ref,

                 (* Fraction of the current time step in the range [0, 1] *)
                 (* c0 and a0 are the positions at alpha0. *)
                 alpha0 : real ref
               }
  fun sweep { local_center, c0, c, a0, a } : sweep =
      let in
          (* XXX This is not an error. Just trying to track down a difference
             between bdd and box2d. *)

      if a > BDDSettings.epsilon + 2.0 * BDDSettings.pi
      then raise BDDMath ("Angle overflow in sweep ctor: " ^ rtos a)
      else ();

      { local_center = ref local_center,
        c0 = ref c0,
        c = ref c, a0 = ref a0, a = ref a, alpha0 = ref 0.0 }
      end
  fun sweepcopy { local_center, c0, c, a0, a, alpha0 } : sweep =
      { local_center = ref (!local_center),
        c0 = ref (!c0),
        c = ref (!c),
        a0 = ref (!a0),
        a = ref (!a),
        alpha0 = ref (!alpha0) }

  fun sweepa ({ a, ... } : sweep) = !a
  fun sweepc ({ c, ... } : sweep) = !c
  fun sweeplocalcenter ({ local_center, ... } : sweep) = !local_center

  fun sweepa0 ({ a0, ... } : sweep) = !a0
  fun sweepc0 ({ c0, ... } : sweep) = !c0
  fun sweepalpha0 ({ alpha0, ...} : sweep) = !alpha0

  val MAX_ANGLE = BDDSettings.epsilon + 2.0 * BDDSettings.pi

  fun sweep_set_a ({a, ... } : sweep, aa) =
      let in
          (* XXX This is not an error. Just trying to track down a difference
             between bdd and box2d. *)
(*
          if aa > (* BDDSettings.epsilon + 2.0 * BDDSettings.pi *) MAX_ANGLE
          then raise BDDMath ("Angle overflow in sweep_set_a: " ^ rtos aa)
          else ();
*)
          a := aa
      end
  fun sweep_set_c ({c, ... } : sweep, cc) = c := cc

  fun sweep_set_a0 ({a0, ... } : sweep, aa0) = a0 := aa0
  fun sweep_set_c0 ({c0, ... } : sweep, cc0) = c0 := cc0
  fun sweep_set_alpha0 ({alpha0, ...} : sweep, aalpha0) = alpha0 := aalpha0

  fun sweep_set_localcenter ({ local_center, ... } : sweep, lc) =
      local_center := lc

  fun sweep_transform ({ local_center, c0, c, a0, a, alpha0 },
                         alpha : real) =
      let val angle : real = (1.0 - alpha) * !a0 + alpha * !a
          val q = rotation angle
          val p = vec2add(vec2stimes (1.0 - alpha, !c0),
                          vec2stimes (alpha, !c))
          val p' = vec2sub (p,
                            mulrotv(q,
                                    !local_center))
      in
          transform (p', q)
      end

  fun sweep_advance({c0, c, a0, a, alpha0, ...} : sweep, alpha : real) =
      let
          val () = if not (!alpha0 < 1.0)
                   then raise BDDMath "assert"
                   else ()
          val beta = (alpha - !alpha0) / (1.0 - !alpha0)
      in
          c0 := vec2add(vec2stimes(1.0 - beta, !c0),
                        vec2stimes(beta, !c));
          a0 := (1.0 - beta) * !a0 + beta * !a;
          alpha0 := alpha
      end

  (* Normalize the sweep's angle (in radians) to be between -pi and pi *)
  (* XXX twm: This doesn't keep it between ~pi and pi. Maybe ~2pi and 2pi? *)
  fun sweep_normalize ({ a0, a, ... } : sweep) =
      let
          val twopi = 2.0 * BDDSettings.pi
          val d = twopi * real (Real.floor(!a0 / twopi))
(*
          val () = dprint (fn () => "sweep_normalize: " ^ rtos (!a0) ^
                           " a0 / 2pi " ^ rtos (!a0 / twopi) ^
                           " floor " ^ Int.toString (Real.floor(!a0 / twopi)) ^
                           " d " ^ rtos d ^ 
                           " res: " ^ rtos (!a0 - d) ^ 
                           " " ^ rtos (!a - d) ^ "\n")
*)
      in
          a0 := !a0 - d;
          a := !a - d;
          (* PERF bdd-specific assert *)
          if !a0 > BDDSettings.epsilon + (2.0 * BDDSettings.pi) orelse 
             !a0 < (BDDSettings.pi * ~2.0) - BDDSettings.epsilon
          then raise BDDMath ("Angle overflow in sweepnormalize: " ^ rtos (!a0))
          else ()
      end

end
