signature SDL =
sig


  exception Invalid
  exception SDL of string
  type surface
  type color

  type mousestate
  type joy

  datatype sdlk =
      SDLK_UNKNOWN
    | SDLK_BACKSPACE
    | SDLK_TAB
    | SDLK_CLEAR
    | SDLK_RETURN
    | SDLK_PAUSE
    | SDLK_ESCAPE
    | SDLK_SPACE
    | SDLK_EXCLAIM
    | SDLK_QUOTEDBL
    | SDLK_HASH
    | SDLK_DOLLAR
    | SDLK_AMPERSAND
    | SDLK_QUOTE
    | SDLK_LEFTPAREN
    | SDLK_RIGHTPAREN
    | SDLK_ASTERISK
    | SDLK_PLUS
    | SDLK_COMMA
    | SDLK_MINUS
    | SDLK_PERIOD
    | SDLK_SLASH
    | SDLK_0
    | SDLK_1
    | SDLK_2
    | SDLK_3
    | SDLK_4
    | SDLK_5
    | SDLK_6
    | SDLK_7
    | SDLK_8
    | SDLK_9
    | SDLK_COLON
    | SDLK_SEMICOLON
    | SDLK_LESS
    | SDLK_EQUALS
    | SDLK_GREATER
    | SDLK_QUESTION
    | SDLK_AT
    | SDLK_LEFTBRACKET
    | SDLK_BACKSLASH
    | SDLK_RIGHTBRACKET
    | SDLK_CARET
    | SDLK_UNDERSCORE
    | SDLK_BACKQUOTE
    | SDLK_a
    | SDLK_b
    | SDLK_c
    | SDLK_d
    | SDLK_e
    | SDLK_f
    | SDLK_g
    | SDLK_h
    | SDLK_i
    | SDLK_j
    | SDLK_k
    | SDLK_l
    | SDLK_m
    | SDLK_n
    | SDLK_o
    | SDLK_p
    | SDLK_q
    | SDLK_r
    | SDLK_s
    | SDLK_t
    | SDLK_u
    | SDLK_v
    | SDLK_w
    | SDLK_x
    | SDLK_y
    | SDLK_z
    | SDLK_DELETE
    | SDLK_WORLD_0
    | SDLK_WORLD_1
    | SDLK_WORLD_2
    | SDLK_WORLD_3
    | SDLK_WORLD_4
    | SDLK_WORLD_5
    | SDLK_WORLD_6
    | SDLK_WORLD_7
    | SDLK_WORLD_8
    | SDLK_WORLD_9
    | SDLK_WORLD_10
    | SDLK_WORLD_11
    | SDLK_WORLD_12
    | SDLK_WORLD_13
    | SDLK_WORLD_14
    | SDLK_WORLD_15
    | SDLK_WORLD_16
    | SDLK_WORLD_17
    | SDLK_WORLD_18
    | SDLK_WORLD_19
    | SDLK_WORLD_20
    | SDLK_WORLD_21
    | SDLK_WORLD_22
    | SDLK_WORLD_23
    | SDLK_WORLD_24
    | SDLK_WORLD_25
    | SDLK_WORLD_26
    | SDLK_WORLD_27
    | SDLK_WORLD_28
    | SDLK_WORLD_29
    | SDLK_WORLD_30
    | SDLK_WORLD_31
    | SDLK_WORLD_32
    | SDLK_WORLD_33
    | SDLK_WORLD_34
    | SDLK_WORLD_35
    | SDLK_WORLD_36
    | SDLK_WORLD_37
    | SDLK_WORLD_38
    | SDLK_WORLD_39
    | SDLK_WORLD_40
    | SDLK_WORLD_41
    | SDLK_WORLD_42
    | SDLK_WORLD_43
    | SDLK_WORLD_44
    | SDLK_WORLD_45
    | SDLK_WORLD_46
    | SDLK_WORLD_47
    | SDLK_WORLD_48
    | SDLK_WORLD_49
    | SDLK_WORLD_50
    | SDLK_WORLD_51
    | SDLK_WORLD_52
    | SDLK_WORLD_53
    | SDLK_WORLD_54
    | SDLK_WORLD_55
    | SDLK_WORLD_56
    | SDLK_WORLD_57
    | SDLK_WORLD_58
    | SDLK_WORLD_59
    | SDLK_WORLD_60
    | SDLK_WORLD_61
    | SDLK_WORLD_62
    | SDLK_WORLD_63
    | SDLK_WORLD_64
    | SDLK_WORLD_65
    | SDLK_WORLD_66
    | SDLK_WORLD_67
    | SDLK_WORLD_68
    | SDLK_WORLD_69
    | SDLK_WORLD_70
    | SDLK_WORLD_71
    | SDLK_WORLD_72
    | SDLK_WORLD_73
    | SDLK_WORLD_74
    | SDLK_WORLD_75
    | SDLK_WORLD_76
    | SDLK_WORLD_77
    | SDLK_WORLD_78
    | SDLK_WORLD_79
    | SDLK_WORLD_80
    | SDLK_WORLD_81
    | SDLK_WORLD_82
    | SDLK_WORLD_83
    | SDLK_WORLD_84
    | SDLK_WORLD_85
    | SDLK_WORLD_86
    | SDLK_WORLD_87
    | SDLK_WORLD_88
    | SDLK_WORLD_89
    | SDLK_WORLD_90
    | SDLK_WORLD_91
    | SDLK_WORLD_92
    | SDLK_WORLD_93
    | SDLK_WORLD_94
    | SDLK_WORLD_95
    | SDLK_KP0
    | SDLK_KP1
    | SDLK_KP2
    | SDLK_KP3
    | SDLK_KP4
    | SDLK_KP5
    | SDLK_KP6
    | SDLK_KP7
    | SDLK_KP8
    | SDLK_KP9
    | SDLK_KP_PERIOD
    | SDLK_KP_DIVIDE
    | SDLK_KP_MULTIPLY
    | SDLK_KP_MINUS
    | SDLK_KP_PLUS
    | SDLK_KP_ENTER
    | SDLK_KP_EQUALS
    | SDLK_UP
    | SDLK_DOWN
    | SDLK_RIGHT
    | SDLK_LEFT
    | SDLK_INSERT
    | SDLK_HOME
    | SDLK_END
    | SDLK_PAGEUP
    | SDLK_PAGEDOWN
    | SDLK_F1
    | SDLK_F2
    | SDLK_F3
    | SDLK_F4
    | SDLK_F5
    | SDLK_F6
    | SDLK_F7
    | SDLK_F8
    | SDLK_F9
    | SDLK_F10
    | SDLK_F11
    | SDLK_F12
    | SDLK_F13
    | SDLK_F14
    | SDLK_F15
    | SDLK_NUMLOCK
    | SDLK_CAPSLOCK
    | SDLK_SCROLLOCK
    | SDLK_RSHIFT
    | SDLK_LSHIFT
    | SDLK_RCTRL
    | SDLK_LCTRL
    | SDLK_RALT
    | SDLK_LALT
    | SDLK_RMETA
    | SDLK_LMETA
    | SDLK_LSUPER
    | SDLK_RSUPER
    | SDLK_MODE
    | SDLK_COMPOSE
    | SDLK_HELP
    | SDLK_PRINT
    | SDLK_SYSREQ
    | SDLK_BREAK
    | SDLK_MENU
    | SDLK_POWER
    | SDLK_EURO
    | SDLK_UNDO

  (* for serialization of keyboard configs, for example. *)
  val sdlktostring : sdlk -> string
  val sdlkfromstring : string -> sdlk option

  structure Joystick :
  sig
      (* if the joystick is enabled, then events will
         be returned for it. Otherwise they must be
         checked manually. *)
      datatype event_state = ENABLE | IGNORE
      eqtype hatstate

      val number : unit -> int
      val name : int -> string

      val openjoy : int -> joy
      val closejoy : joy -> unit

      val numaxes : joy -> int
      val numballs : joy -> int
      val numhats : joy -> int
      val numbuttons : joy -> int

      val setstate : event_state -> unit

      val hat_centered : hatstate -> bool
      val hat_up : hatstate -> bool
      val hat_right : hatstate -> bool
      val hat_left : hatstate -> bool
      val hat_down : hatstate -> bool

      (* For serialization of joystick configuration. *)
      val hatstatetostring : hatstate -> string
      val hatstatefromstring : string -> hatstate option
  end

  datatype event =
      E_Active
    | E_KeyDown of { sym : sdlk }
    | E_KeyUp of { sym : sdlk }
    | E_MouseMotion of { which : int, state : mousestate, x : int, y : int, xrel : int, yrel : int }
    | E_MouseDown of { button : int, x : int, y : int }
    | E_MouseUp of { button : int, x : int, y : int }
    | E_JoyAxis of { which : int, axis : int, v : int }
    | E_JoyDown of { which : int, button : int }
    | E_JoyUp of { which : int, button : int }
    | E_JoyHat of { which : int, hat : int, state : Joystick.hatstate }
    | E_JoyBall
    | E_Resize
    | E_Expose
    | E_SysWM
    | E_User
    | E_Quit
    | E_Unknown

  (* Supported platforms. *)
  datatype platform = WIN32 | LINUX | OSX
  val platform : platform

  (* True if we are in console mode (unable to access screen, sound, etc.) *)
  val console_mode : bool

  (* R, G, B, A *)
  val color : Word8.word * Word8.word * Word8.word * Word8.word -> color
  val components : color -> Word8.word * Word8.word * Word8.word * Word8.word

  val makescreen : int * int -> surface
  val makefullscreen : int * int -> surface
  val makeglscreen : int * int -> surface

  (* src: x, y, w, h   dst: x, y *)
  val blit : surface * int * int * int * int * surface * int * int -> unit
  val blitall : surface * surface * int * int -> unit
  val pollevent : unit -> event option

  val surface_width : surface -> int
  val surface_height : surface -> int

  val sdlktos : sdlk -> string

  val flip : surface -> unit
  val glflip : unit -> unit

  val getticks : unit -> Word32.word
  val delay : int -> unit

  (* Test if the button is down in the state. *)
  val mouse_left : mousestate -> bool
  val mouse_middle : mousestate -> bool
  val mouse_right : mousestate -> bool
  val mouse_wheelup : mousestate -> bool
  val mouse_wheeldown : mousestate -> bool

  (* Enable or disable display of the mouse cursor. true displays it, which
     is the initial state. *)
  val show_cursor : bool -> unit

  val clearsurface : surface * color -> unit

  (* draw a pixel to the surface. XXX the alpha component is ignored. *)
  val drawpixel : surface * int * int * color -> unit
  val clippixel : surface * int * int * color -> unit
  val getpixel  : surface * int * int -> color

  val getpixels : surface -> MLton.Pointer.t 

  (* drawcircle (surf, x, y, radius, color)
     draws a circle (not filled)
     *)
  val drawcircle : surface * int * int * int * color -> unit
  (* drawline (surf, x0, y0, x1, y1, color) *)
  val drawline : surface * int * int * int * int * color -> unit
  (* drawrect (surf, x0, y0, x1, y1, color) 
     Axis aligned hollow rectangle. Includes corners. *)
  val drawbox : surface * int * int * int * int * color -> unit

  (* For f in [0.0, 1.0], we get f * c + (1.0 - f) * cc. *)
  val colormixfrac : color * color * real -> color

  (* Blit a pixel with proper alpha compositing. XXX not implemented *)
  val blitpixel : surface * int * int * color -> unit

  (* x, y, width, height *)
  val fillrect  : surface * int * int * int * int * color -> unit

  (* make a surface with the given width and height. it will be uninitialized. *)
  val makesurface : int * int -> surface
  (* invalidates the surface *)
  val freesurface : surface -> unit

  val version : unit -> { major : int, minor : int, patch : int }

  (* These are not separate structures because they rely on access to the
     implementations of surface, color, etc. *)
  structure Image :
  sig

    val load : string -> surface option

  end

  structure Util :
  sig

    (* Make an n-pixel border inside a surface *)
    val outline : surface * int * color -> unit

    (* Create a surface of the given width and height and color. This is
       typically used as an alternative to fillrect when we want to do
       alpha compositing with the screen. *)
    val makealpharect : int * int * color -> surface

    (* make an alpha rectangle with a vertical gradient between the two
       specified colors. bias should be between 0 and 1; a higher bias
       means the gradient will have more of the bottom color. *)
    val makealpharectgrad : { w : int, h : int, 
                              ctop : color, cbot : color, 
                              bias : real } -> surface

    (* create a version of the surface that's 50% transparent *)
    val alphadim : surface -> surface

    (* make a surface twice as wide and twice as tall, doing nearest-neighbor
       interpolation. *)
    val surf2x : surface -> surface
        
    val blit16x : surface * int * int * int * int  * surface * int * int -> unit
  end

end
