structure Font = struct

  val charmap =
      " ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789" ^
      "`-=[]\\;',./~!@#$%^&*()_+{}|:\"<>?"

  val surf = Graphics.requireimage "media/graphics/font.png"

  val () = SDL.setalpha surf false false 0w0

  structure Normal = FontFn
                         (val surf = surf
                          val charmap = charmap
                          val width = 9
                          val height = 16
                          val styles = 6
                          val overlap = 1
                          val dims = 3)

  val surfhuge = Graphics.requireimage "media/graphics/fonthuge.png"

  val () = SDL.setalpha surfhuge false false 0w0

  structure Huge = FontFn
                       (val surf = surfhuge
                        val charmap = charmap
                        val width = 27
                        val height = 48
                        val styles = 7
                        val overlap = 3
                        val dims = 3)

end
