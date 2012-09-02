structure Graphics =
struct
  (* requireimage : string -> surface option *)
  fun requireimage s = 
      case SDL.Image.load s of
            NONE => (print ("couldn't open " ^ s ^ "\n");
                      raise Fail "image not found")
          | SOME p => p

end
