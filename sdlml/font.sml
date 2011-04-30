
functor FontFn(F : FONTARG) :> FONT =
struct

    open F
    val fontsurf = F.surf

    structure Attr =
    struct
        (* PERF could cache current *)
        datatype attr = 
            C of int * attr
          | A of int * attr
          | NIL
            
        val pushcolor = C
        val pushalpha = A
        val empty = NIL

        fun color (C (x, _)) = x
          | color NIL = 0
          | color (A (_, l)) = color l
        fun alpha (A (a, _)) = a
          | alpha (C (_, l)) = alpha l
          | alpha NIL = 0

        fun pop NIL = NIL
          | pop (C (_, l)) = l
          | pop (A (_, l)) = l
    end

    val chars = Array.array(255, 0)
    val () =
        (* invert the map *)
        Util.for 0 (size F.charmap - 1)
        (fn i =>
         Array.update(chars, ord (String.sub(F.charmap, i)), i))

    exception Font of string

    fun sizex_plain s = size s * (width - overlap)
    fun draw_plain (surf, x, y, s) =
        Util.for 0 (size s - 1)
        (fn i =>
         SDL.blit(fontsurf, 
                  Array.sub(chars, ord (String.sub(s, i))) * width, 0, 
                  width, height,
                  surf, x + i * (width - overlap), y))

    fun draw (surf, x, y, s) = 
        let
            fun d(attr, x, i) =
                let
                    fun dr (c, i) =
                     let in
                         SDL.blit(fontsurf, (* XXX use alpha to select surf *)
                                  Array.sub(chars, ord c) * width,
                                  Attr.color attr * F.height,
                                  width, height,
                                  surf,
                                  x, y);
                         d(attr, x + (width - overlap), i + 1)
                     end
                in
(*
                    print ("size " ^ Int.toString(size s) ^ " i " ^
                           Int.toString i ^ "\n");
*)
                   if i = size s then ()
                   else
                   case (String.sub (s, i), Int.compare(i, size s - 1)) of
                        (_, GREATER) => () (* done *)
                      | (#"^", LESS) => (* escape char, but only when not last *)
                            (case String.sub(s, i + 1) of
                                 #"<" => d(Attr.pop attr, x, i + 2)
                               | #"^" => dr (#"^", i + 1)
                               | c =>
                                 if ord c >= ord #"0" andalso ord c <= ord #"9"
                                 then d(Attr.pushcolor
                                        ((ord c - ord #"0")
                                         mod styles, attr),
                                        x, i + 2)
                                 else if ord c >= ord #"a" andalso 
                                         ord c <= ord #"z"
                                      then d(Attr.pushalpha
                                             ((ord c - ord #"a")
                                              mod dims, attr),
                                             x, i + 2)
                                      else (* ?? skip *)
                                           d(attr, x, i + 2))
                      | (c, _) => dr (c, i)
                end
        in
            d (Attr.empty, x, 0)
        end

    fun sizex s =
        let
            (* PERF no need to keep attribute stack with the attributes that
               we currently support, but in future it might actually affect
               the width. *)
            fun d (attr, w, i) =
                let fun dr (_, i) = d (attr, w + (width - overlap), i + 1)
                in
                   if i = size s then w
                   else
                   case (String.sub (s, i), Int.compare(i, size s - 1)) of
                        (_, GREATER) => w
                      | (#"^", LESS) => (* escape char, but only when not last *)
                            (case String.sub(s, i + 1) of
                                 #"<" => d (Attr.pop attr, w, i + 2)
                               | #"^" => dr (#"^", i + 1)
                               | c =>
                                 if ord c >= ord #"0" andalso ord c <= ord #"9"
                                 then d (Attr.pushcolor
                                         ((ord c - ord #"0")
                                          mod styles, attr),
                                         w, i + 2)
                                 else if ord c >= ord #"a" andalso 
                                         ord c <= ord #"z"
                                      then d (Attr.pushalpha
                                              ((ord c - ord #"a")
                                               mod dims, attr),
                                              w, i + 2)
                                      else (* ?? skip *)
                                           d (attr, w, i + 2))
                      | (c, _) => dr (c, i)
                end
        in
            d (Attr.empty, 0, 0)
        end

    fun drawcenter _ = raise Font "unimplemented"
    fun drawlines _ = raise Font "unimplemented"
    fun length _ = raise Font "unimplemented"
    fun lines _ = raise Font "unimplemented"
    fun substr _ = raise Font "unimplemented"
    fun prefix _ = raise Font "unimplemented"
    fun suffix _ = raise Font "unimplemented"
    fun truncate _ = raise Font "unimplemented"
    fun pad _ = raise Font "unimplemented"
end
