
signature FONTARG =
sig
    (* the surface to use; reference is kept and never freed *)
    val surf : SDL.surface
    (* number of styles (appearing vertically in the surface) *)
    val styles : int
    (* containing the chars in the order that they appear in the font *)
    val charmap : string
    (* pixel width of one character *)
    val width : int
    (* height of one character *)
    val height : int
    (* pixel amount to overlap characters horizontally *)
    val overlap : int
    (* number of alpha-dimmed versions to create *)
    val dims : int
end

signature FONT =
sig
    exception Font of string

    val width : int
    val height : int
    val styles : int
    val overlap : int
        
    (* number of drawn characters, ignoring control codes.
       length(s) * (width-overlap) gives
       the screen width. *)
    val length : string -> int
    val substr : string * int * int -> string

    (* len must be <= font::length(s) *)
    val prefix : string * int -> string
    val suffix : string * int -> string

    (* similarly, pad a string out to n displayable
       characters, doing the right thing for
       color codes. If n is negative, pad with spaces
       on the left side instead of right.
       
       precondition: |n| >= 3 *)
    val pad : string * int -> string

    (* truncate to n chars if too long; if n is
       negative, truncate off the left side instead of
       the right. *)
    val truncate : string * int -> string


    (* return the size in pixels of the string.
       sizey is always font.height. *)
    val sizex : string -> int
    (* ignoring formatting chars *)
    val sizex_plain : string -> int

    (* returns the number of lines in the string,
       which is always at least 1 unless the
       string is empty. *)
    val lines : string -> int

    (* specify the top-left pixel. draws to the
       screen. *)
    val draw : SDL.surface * int * int * string -> unit
    val draw_plain : SDL.surface * int * int * string -> unit

    (* draw a multi-line string.
       the height in pixels of the area drawn is returned. *)
    val drawlines : SDL.surface * int * int * string -> int

    (* same, but centers each line horizontally about the x position *)
    val drawcenter : SDL.surface * int * int * string -> int

end
