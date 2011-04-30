structure Graphics =
struct
  (* requireimage : string -> surface option *)
  fun requireimage s = 
      case SDL.Image.load s of
            NONE => (print ("couldn't open " ^ s ^ "\n");
                      raise Fail "image not found")
          | SOME p => p

  val redstar = requireimage "media/graphics/redstar.png"
  val robot = requireimage "media/graphics/robot.png"
  (* add more images here *)
end
