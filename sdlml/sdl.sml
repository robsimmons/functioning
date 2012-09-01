structure SDL :> SDL =
struct

  (* a "safe" pointer can be set to null when
     it is imperatively freed, so that we can
     then later block operations on it *)
  type safe = MLton.Pointer.t ref
  type ptr = MLton.Pointer.t 

  (* XXX as RGBA, though it's not clear we use
     this consistently *)
  type color = Word32.word

  type mousestate  = Word8.word
  type joyhatstate = Word8.word

  exception SDL of string
  exception Invalid
  val null = MLton.Pointer.null
(*
  fun check (ref p) = if p = null then raise Invalid else ()
  fun clear r = r := null
*)

  fun !! (ref p) = if p = null then raise Invalid else p

  fun readcstring r =
      let val len = 
          (case Array.findi (fn (_, #"\000") => true | _ => false) r of
               NONE => Array.length r
             | SOME (i, _) => i)
      in
          CharVector.tabulate (len, fn x => Array.sub(r, x))
      end

  (* XXX endianness...?! *)
  fun color (r, g, b, a) = 
    Word32.orb
    (Word32.<< (Word32.fromInt (Word8.toInt r), 0w24),
     Word32.orb
     (Word32.<< (Word32.fromInt (Word8.toInt g), 0w16),
      Word32.orb
      (Word32.<< (Word32.fromInt (Word8.toInt b), 0w8),
       Word32.fromInt (Word8.toInt a))))

  (* 32-bit words, but must be 0w0-0w255. *)
  fun color32 (r : Word32.word, g, b, a) = 
    Word32.orb
    (Word32.<< (r, 0w24),
     Word32.orb
     (Word32.<< (g, 0w16),
      Word32.orb
      (Word32.<< (b, 0w8),
       a)))

  (* Must agree with SDL_events.h *)
  val SDL_QUERY = ~1
  val SDL_IGNORE = 0
  val SDL_DISABLE = 0
  val SDL_ENABLE = 1

  fun components x =
      (Word8.fromInt (Word32.toInt (Word32.andb(Word32.>>(x, 0w24), 0w255))),
       Word8.fromInt (Word32.toInt (Word32.andb(Word32.>>(x, 0w16), 0w255))),
       Word8.fromInt (Word32.toInt (Word32.andb(Word32.>>(x, 0w8), 0w255))),
       Word8.fromInt (Word32.toInt (Word32.andb(x, 0w255))))

  fun components32 x =
      (Word32.andb(Word32.>>(x, 0w24), 0w255),
       Word32.andb(Word32.>>(x, 0w16), 0w255),
       Word32.andb(Word32.>>(x, 0w8), 0w255),
       Word32.andb(x, 0w255))

  type surface = safe

  type joy = safe

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

  datatype platform = WIN32 | LINUX | OSX
      
  datatype event =
    E_Active
  | E_KeyDown of { sym : sdlk }
  | E_KeyUp of { sym : sdlk }
  | E_MouseMotion of { which : int, state : mousestate, 
                       x : int, y : int, xrel : int, yrel : int }
  | E_MouseDown of { button : int, x : int, y : int }
  | E_MouseUp of { button : int, x : int, y : int }
  | E_JoyAxis of { which : int, axis : int, v : int }
  | E_JoyDown of { which : int, button : int }
  | E_JoyUp of { which : int, button : int }
  | E_JoyHat of { which : int, hat : int, state : joyhatstate }
  | E_JoyBall
  | E_Resize
  | E_Expose
  | E_SysWM
  | E_User
  | E_Quit
  | E_Unknown

  fun sdlktos s =
      (case s of
        SDLK_UNKNOWN => "UNKNOWN"
      | SDLK_BACKSPACE => "BACKSPACE"
      | SDLK_TAB => "TAB"
      | SDLK_CLEAR => "CLEAR"
      | SDLK_RETURN => "RETURN"
      | SDLK_PAUSE => "PAUSE"
      | SDLK_ESCAPE => "ESCAPE"
      | SDLK_SPACE => "SPACE"
      | SDLK_EXCLAIM => "EXCLAIM"
      | SDLK_QUOTEDBL => "QUOTEDBL"
      | SDLK_HASH => "HASH"
      | SDLK_DOLLAR => "DOLLAR"
      | SDLK_AMPERSAND => "AMPERSAND"
      | SDLK_QUOTE => "QUOTE"
      | SDLK_LEFTPAREN => "LEFTPAREN"
      | SDLK_RIGHTPAREN => "RIGHTPAREN"
      | SDLK_ASTERISK => "ASTERISK"
      | SDLK_PLUS => "PLUS"
      | SDLK_COMMA => "COMMA"
      | SDLK_MINUS => "MINUS"
      | SDLK_PERIOD => "PERIOD"
      | SDLK_SLASH => "SLASH"
      | SDLK_0 => "0"
      | SDLK_1 => "1"
      | SDLK_2 => "2"
      | SDLK_3 => "3"
      | SDLK_4 => "4"
      | SDLK_5 => "5"
      | SDLK_6 => "6"
      | SDLK_7 => "7"
      | SDLK_8 => "8"
      | SDLK_9 => "9"
      | SDLK_COLON => "COLON"
      | SDLK_SEMICOLON => "SEMICOLON"
      | SDLK_LESS => "LESS"
      | SDLK_EQUALS => "EQUALS"
      | SDLK_GREATER => "GREATER"
      | SDLK_QUESTION => "QUESTION"
      | SDLK_AT => "AT"
      | SDLK_LEFTBRACKET => "LEFTBRACKET"
      | SDLK_BACKSLASH => "BACKSLASH"
      | SDLK_RIGHTBRACKET => "RIGHTBRACKET"
      | SDLK_CARET => "CARET"
      | SDLK_UNDERSCORE => "UNDERSCORE"
      | SDLK_BACKQUOTE => "BACKQUOTE"
      | SDLK_a => "a"
      | SDLK_b => "b"
      | SDLK_c => "c"
      | SDLK_d => "d"
      | SDLK_e => "e"
      | SDLK_f => "f"
      | SDLK_g => "g"
      | SDLK_h => "h"
      | SDLK_i => "i"
      | SDLK_j => "j"
      | SDLK_k => "k"
      | SDLK_l => "l"
      | SDLK_m => "m"
      | SDLK_n => "n"
      | SDLK_o => "o"
      | SDLK_p => "p"
      | SDLK_q => "q"
      | SDLK_r => "r"
      | SDLK_s => "s"
      | SDLK_t => "t"
      | SDLK_u => "u"
      | SDLK_v => "v"
      | SDLK_w => "w"
      | SDLK_x => "x"
      | SDLK_y => "y"
      | SDLK_z => "z"
      | SDLK_DELETE => "DELETE"
      | SDLK_WORLD_0 => "WORLD_0"
      | SDLK_WORLD_1 => "WORLD_1"
      | SDLK_WORLD_2 => "WORLD_2"
      | SDLK_WORLD_3 => "WORLD_3"
      | SDLK_WORLD_4 => "WORLD_4"
      | SDLK_WORLD_5 => "WORLD_5"
      | SDLK_WORLD_6 => "WORLD_6"
      | SDLK_WORLD_7 => "WORLD_7"
      | SDLK_WORLD_8 => "WORLD_8"
      | SDLK_WORLD_9 => "WORLD_9"
      | SDLK_WORLD_10 => "WORLD_10"
      | SDLK_WORLD_11 => "WORLD_11"
      | SDLK_WORLD_12 => "WORLD_12"
      | SDLK_WORLD_13 => "WORLD_13"
      | SDLK_WORLD_14 => "WORLD_14"
      | SDLK_WORLD_15 => "WORLD_15"
      | SDLK_WORLD_16 => "WORLD_16"
      | SDLK_WORLD_17 => "WORLD_17"
      | SDLK_WORLD_18 => "WORLD_18"
      | SDLK_WORLD_19 => "WORLD_19"
      | SDLK_WORLD_20 => "WORLD_20"
      | SDLK_WORLD_21 => "WORLD_21"
      | SDLK_WORLD_22 => "WORLD_22"
      | SDLK_WORLD_23 => "WORLD_23"
      | SDLK_WORLD_24 => "WORLD_24"
      | SDLK_WORLD_25 => "WORLD_25"
      | SDLK_WORLD_26 => "WORLD_26"
      | SDLK_WORLD_27 => "WORLD_27"
      | SDLK_WORLD_28 => "WORLD_28"
      | SDLK_WORLD_29 => "WORLD_29"
      | SDLK_WORLD_30 => "WORLD_30"
      | SDLK_WORLD_31 => "WORLD_31"
      | SDLK_WORLD_32 => "WORLD_32"
      | SDLK_WORLD_33 => "WORLD_33"
      | SDLK_WORLD_34 => "WORLD_34"
      | SDLK_WORLD_35 => "WORLD_35"
      | SDLK_WORLD_36 => "WORLD_36"
      | SDLK_WORLD_37 => "WORLD_37"
      | SDLK_WORLD_38 => "WORLD_38"
      | SDLK_WORLD_39 => "WORLD_39"
      | SDLK_WORLD_40 => "WORLD_40"
      | SDLK_WORLD_41 => "WORLD_41"
      | SDLK_WORLD_42 => "WORLD_42"
      | SDLK_WORLD_43 => "WORLD_43"
      | SDLK_WORLD_44 => "WORLD_44"
      | SDLK_WORLD_45 => "WORLD_45"
      | SDLK_WORLD_46 => "WORLD_46"
      | SDLK_WORLD_47 => "WORLD_47"
      | SDLK_WORLD_48 => "WORLD_48"
      | SDLK_WORLD_49 => "WORLD_49"
      | SDLK_WORLD_50 => "WORLD_50"
      | SDLK_WORLD_51 => "WORLD_51"
      | SDLK_WORLD_52 => "WORLD_52"
      | SDLK_WORLD_53 => "WORLD_53"
      | SDLK_WORLD_54 => "WORLD_54"
      | SDLK_WORLD_55 => "WORLD_55"
      | SDLK_WORLD_56 => "WORLD_56"
      | SDLK_WORLD_57 => "WORLD_57"
      | SDLK_WORLD_58 => "WORLD_58"
      | SDLK_WORLD_59 => "WORLD_59"
      | SDLK_WORLD_60 => "WORLD_60"
      | SDLK_WORLD_61 => "WORLD_61"
      | SDLK_WORLD_62 => "WORLD_62"
      | SDLK_WORLD_63 => "WORLD_63"
      | SDLK_WORLD_64 => "WORLD_64"
      | SDLK_WORLD_65 => "WORLD_65"
      | SDLK_WORLD_66 => "WORLD_66"
      | SDLK_WORLD_67 => "WORLD_67"
      | SDLK_WORLD_68 => "WORLD_68"
      | SDLK_WORLD_69 => "WORLD_69"
      | SDLK_WORLD_70 => "WORLD_70"
      | SDLK_WORLD_71 => "WORLD_71"
      | SDLK_WORLD_72 => "WORLD_72"
      | SDLK_WORLD_73 => "WORLD_73"
      | SDLK_WORLD_74 => "WORLD_74"
      | SDLK_WORLD_75 => "WORLD_75"
      | SDLK_WORLD_76 => "WORLD_76"
      | SDLK_WORLD_77 => "WORLD_77"
      | SDLK_WORLD_78 => "WORLD_78"
      | SDLK_WORLD_79 => "WORLD_79"
      | SDLK_WORLD_80 => "WORLD_80"
      | SDLK_WORLD_81 => "WORLD_81"
      | SDLK_WORLD_82 => "WORLD_82"
      | SDLK_WORLD_83 => "WORLD_83"
      | SDLK_WORLD_84 => "WORLD_84"
      | SDLK_WORLD_85 => "WORLD_85"
      | SDLK_WORLD_86 => "WORLD_86"
      | SDLK_WORLD_87 => "WORLD_87"
      | SDLK_WORLD_88 => "WORLD_88"
      | SDLK_WORLD_89 => "WORLD_89"
      | SDLK_WORLD_90 => "WORLD_90"
      | SDLK_WORLD_91 => "WORLD_91"
      | SDLK_WORLD_92 => "WORLD_92"
      | SDLK_WORLD_93 => "WORLD_93"
      | SDLK_WORLD_94 => "WORLD_94"
      | SDLK_WORLD_95 => "WORLD_95"
      | SDLK_KP0 => "KP0"
      | SDLK_KP1 => "KP1"
      | SDLK_KP2 => "KP2"
      | SDLK_KP3 => "KP3"
      | SDLK_KP4 => "KP4"
      | SDLK_KP5 => "KP5"
      | SDLK_KP6 => "KP6"
      | SDLK_KP7 => "KP7"
      | SDLK_KP8 => "KP8"
      | SDLK_KP9 => "KP9"
      | SDLK_KP_PERIOD => "KP_PERIOD"
      | SDLK_KP_DIVIDE => "KP_DIVIDE"
      | SDLK_KP_MULTIPLY => "KP_MULTIPLY"
      | SDLK_KP_MINUS => "KP_MINUS"
      | SDLK_KP_PLUS => "KP_PLUS"
      | SDLK_KP_ENTER => "KP_ENTER"
      | SDLK_KP_EQUALS => "KP_EQUALS"
      | SDLK_UP => "UP"
      | SDLK_DOWN => "DOWN"
      | SDLK_RIGHT => "RIGHT"
      | SDLK_LEFT => "LEFT"
      | SDLK_INSERT => "INSERT"
      | SDLK_HOME => "HOME"
      | SDLK_END => "END"
      | SDLK_PAGEUP => "PAGEUP"
      | SDLK_PAGEDOWN => "PAGEDOWN"
      | SDLK_F1 => "F1"
      | SDLK_F2 => "F2"
      | SDLK_F3 => "F3"
      | SDLK_F4 => "F4"
      | SDLK_F5 => "F5"
      | SDLK_F6 => "F6"
      | SDLK_F7 => "F7"
      | SDLK_F8 => "F8"
      | SDLK_F9 => "F9"
      | SDLK_F10 => "F10"
      | SDLK_F11 => "F11"
      | SDLK_F12 => "F12"
      | SDLK_F13 => "F13"
      | SDLK_F14 => "F14"
      | SDLK_F15 => "F15"
      | SDLK_NUMLOCK => "NUMLOCK"
      | SDLK_CAPSLOCK => "CAPSLOCK"
      | SDLK_SCROLLOCK => "SCROLLOCK"
      | SDLK_RSHIFT => "RSHIFT"
      | SDLK_LSHIFT => "LSHIFT"
      | SDLK_RCTRL => "RCTRL"
      | SDLK_LCTRL => "LCTRL"
      | SDLK_RALT => "RALT"
      | SDLK_LALT => "LALT"
      | SDLK_RMETA => "RMETA"
      | SDLK_LMETA => "LMETA"
      | SDLK_LSUPER => "LSUPER"
      | SDLK_RSUPER => "RSUPER"
      | SDLK_MODE => "MODE"
      | SDLK_COMPOSE => "COMPOSE"
      | SDLK_HELP => "HELP"
      | SDLK_PRINT => "PRINT"
      | SDLK_SYSREQ => "SYSREQ"
      | SDLK_BREAK => "BREAK"
      | SDLK_MENU => "MENU"
      | SDLK_POWER => "POWER"
      | SDLK_EURO => "EURO"
      | SDLK_UNDO => "UNDO"
               )

  local
  val sdlk =
    Vector.fromList
    ([
     (* 0-7 *)
     SDLK_UNKNOWN, SDLK_UNKNOWN, SDLK_UNKNOWN, SDLK_UNKNOWN,
     SDLK_UNKNOWN, SDLK_UNKNOWN, SDLK_UNKNOWN, SDLK_UNKNOWN,

     SDLK_BACKSPACE,          (* = 8 *)
     SDLK_TAB,                (* = 9 *)

     SDLK_UNKNOWN, SDLK_UNKNOWN,

     SDLK_CLEAR,              (* = 12 *)
     SDLK_RETURN,             (* = 13 *)

     SDLK_UNKNOWN, SDLK_UNKNOWN, SDLK_UNKNOWN, SDLK_UNKNOWN,
     SDLK_UNKNOWN,

     SDLK_PAUSE,              (* = 19 *)

     SDLK_UNKNOWN, SDLK_UNKNOWN, SDLK_UNKNOWN, SDLK_UNKNOWN,
     SDLK_UNKNOWN, SDLK_UNKNOWN, SDLK_UNKNOWN,

     SDLK_ESCAPE,             (* = 27 *)

     SDLK_UNKNOWN, SDLK_UNKNOWN, SDLK_UNKNOWN, SDLK_UNKNOWN,

     SDLK_SPACE,              (* = 32 *)
     SDLK_EXCLAIM,            (* = 33 *)
     SDLK_QUOTEDBL,           (* = 34 *)
     SDLK_HASH,               (* = 35 *)
     SDLK_DOLLAR,             (* = 36 *)

      (* would be percent, apparently on no keyboard
         is this a non-shift key *)
     SDLK_UNKNOWN, (* 37 *)

     SDLK_AMPERSAND,          (* = 38 *)
     SDLK_QUOTE,              (* = 39 *)
     SDLK_LEFTPAREN,          (* = 40 *)
     SDLK_RIGHTPAREN,         (* = 41 *)
     SDLK_ASTERISK,           (* = 42 *)
     SDLK_PLUS,               (* = 43 *)
     SDLK_COMMA,              (* = 44 *)
     SDLK_MINUS,              (* = 45 *)
     SDLK_PERIOD,             (* = 46 *)
     SDLK_SLASH,              (* = 47 *)
     SDLK_0,                  (* = 48 *)
     SDLK_1,                  (* = 49 *)
     SDLK_2,                  (* = 50 *)
     SDLK_3,                  (* = 51 *)
     SDLK_4,                  (* = 52 *)
     SDLK_5,                  (* = 53 *)
     SDLK_6,                  (* = 54 *)
     SDLK_7,                  (* = 55 *)
     SDLK_8,                  (* = 56 *)
     SDLK_9,                  (* = 57 *)
     SDLK_COLON,              (* = 58 *)
     SDLK_SEMICOLON,          (* = 59 *)
     SDLK_LESS,               (* = 60 *)
     SDLK_EQUALS,             (* = 61 *)
     SDLK_GREATER,            (* = 62 *)
     SDLK_QUESTION,           (* = 63 *)
     SDLK_AT,                 (* = 64 *)

     (* (uppercase) *)
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,

     SDLK_LEFTBRACKET,        (* = 91 *)
     SDLK_BACKSLASH,          (* = 92 *)
     SDLK_RIGHTBRACKET,       (* = 93 *)
     SDLK_CARET,              (* = 94 *)
     SDLK_UNDERSCORE,         (* = 95 *)
     SDLK_BACKQUOTE,          (* = 96 *)
     SDLK_a,                  (* = 97 *)
     SDLK_b,                  (* = 98 *)
     SDLK_c,                  (* = 99 *)
     SDLK_d,                  (* = 100 *)
     SDLK_e,                  (* = 101 *)
     SDLK_f,                  (* = 102 *)
     SDLK_g,                  (* = 103 *)
     SDLK_h,                  (* = 104 *)
     SDLK_i,                  (* = 105 *)
     SDLK_j,                  (* = 106 *)
     SDLK_k,                  (* = 107 *)
     SDLK_l,                  (* = 108 *)
     SDLK_m,                  (* = 109 *)
     SDLK_n,                  (* = 110 *)
     SDLK_o,                  (* = 111 *)
     SDLK_p,                  (* = 112 *)
     SDLK_q,                  (* = 113 *)
     SDLK_r,                  (* = 114 *)
     SDLK_s,                  (* = 115 *)
     SDLK_t,                  (* = 116 *)
     SDLK_u,                  (* = 117 *)
     SDLK_v,                  (* = 118 *)
     SDLK_w,                  (* = 119 *)
     SDLK_x,                  (* = 120 *)
     SDLK_y,                  (* = 121 *)
     SDLK_z,                  (* = 122 *)

     SDLK_UNKNOWN, SDLK_UNKNOWN, SDLK_UNKNOWN, SDLK_UNKNOWN,  (* 123, 124, 125, 126 *)

     SDLK_DELETE              (* = 127 *)

     ] @ List.tabulate ((160 - 127) - 1, fn _ => SDLK_UNKNOWN) @ [

     (* FIXME these appear to be off somehow (left/right don't work) *)
     SDLK_WORLD_0,            (* = 160 *)
     SDLK_WORLD_1,            (* = 161 *)
     SDLK_WORLD_2,            (* = 162 *)
     SDLK_WORLD_3,            (* = 163 *)
     SDLK_WORLD_4,            (* = 164 *)
     SDLK_WORLD_5,            (* = 165 *)
     SDLK_WORLD_6,            (* = 166 *)
     SDLK_WORLD_7,            (* = 167 *)
     SDLK_WORLD_8,            (* = 168 *)
     SDLK_WORLD_9,            (* = 169 *)
     SDLK_WORLD_10,           (* = 170 *)
     SDLK_WORLD_11,           (* = 171 *)
     SDLK_WORLD_12,           (* = 172 *)
     SDLK_WORLD_13,           (* = 173 *)
     SDLK_WORLD_14,           (* = 174 *)
     SDLK_WORLD_15,           (* = 175 *)
     SDLK_WORLD_16,           (* = 176 *)
     SDLK_WORLD_17,           (* = 177 *)
     SDLK_WORLD_18,           (* = 178 *)
     SDLK_WORLD_19,           (* = 179 *)
     SDLK_WORLD_20,           (* = 180 *)
     SDLK_WORLD_21,           (* = 181 *)
     SDLK_WORLD_22,           (* = 182 *)
     SDLK_WORLD_23,           (* = 183 *)
     SDLK_WORLD_24,           (* = 184 *)
     SDLK_WORLD_25,           (* = 185 *)
     SDLK_WORLD_26,           (* = 186 *)
     SDLK_WORLD_27,           (* = 187 *)
     SDLK_WORLD_28,           (* = 188 *)
     SDLK_WORLD_29,           (* = 189 *)
     SDLK_WORLD_30,           (* = 190 *)
     SDLK_WORLD_31,           (* = 191 *)
     SDLK_WORLD_32,           (* = 192 *)
     SDLK_WORLD_33,           (* = 193 *)
     SDLK_WORLD_34,           (* = 194 *)
     SDLK_WORLD_35,           (* = 195 *)
     SDLK_WORLD_36,           (* = 196 *)
     SDLK_WORLD_37,           (* = 197 *)
     SDLK_WORLD_38,           (* = 198 *)
     SDLK_WORLD_39,           (* = 199 *)
     SDLK_WORLD_40,           (* = 200 *)
     SDLK_WORLD_41,           (* = 201 *)
     SDLK_WORLD_42,           (* = 202 *)
     SDLK_WORLD_43,           (* = 203 *)
     SDLK_WORLD_44,           (* = 204 *)
     SDLK_WORLD_45,           (* = 205 *)
     SDLK_WORLD_46,           (* = 206 *)
     SDLK_WORLD_47,           (* = 207 *)
     SDLK_WORLD_48,           (* = 208 *)
     SDLK_WORLD_49,           (* = 209 *)
     SDLK_WORLD_50,           (* = 210 *)
     SDLK_WORLD_51,           (* = 211 *)
     SDLK_WORLD_52,           (* = 212 *)
     SDLK_WORLD_53,           (* = 213 *)
     SDLK_WORLD_54,           (* = 214 *)
     SDLK_WORLD_55,           (* = 215 *)
     SDLK_WORLD_56,           (* = 216 *)
     SDLK_WORLD_57,           (* = 217 *)
     SDLK_WORLD_58,           (* = 218 *)
     SDLK_WORLD_59,           (* = 219 *)
     SDLK_WORLD_60,           (* = 220 *)
     SDLK_WORLD_61,           (* = 221 *)
     SDLK_WORLD_62,           (* = 222 *)
     SDLK_WORLD_63,           (* = 223 *)
     SDLK_WORLD_64,           (* = 224 *)
     SDLK_WORLD_65,           (* = 225 *)
     SDLK_WORLD_66,           (* = 226 *)
     SDLK_WORLD_67,           (* = 227 *)
     SDLK_WORLD_68,           (* = 228 *)
     SDLK_WORLD_69,           (* = 229 *)
     SDLK_WORLD_70,           (* = 230 *)
     SDLK_WORLD_71,           (* = 231 *)
     SDLK_WORLD_72,           (* = 232 *)
     SDLK_WORLD_73,           (* = 233 *)
     SDLK_WORLD_74,           (* = 234 *)
     SDLK_WORLD_75,           (* = 235 *)
     SDLK_WORLD_76,           (* = 236 *)
     SDLK_WORLD_77,           (* = 237 *)
     SDLK_WORLD_78,           (* = 238 *)
     SDLK_WORLD_79,           (* = 239 *)
     SDLK_WORLD_80,           (* = 240 *)
     SDLK_WORLD_81,           (* = 241 *)
     SDLK_WORLD_82,           (* = 242 *)
     SDLK_WORLD_83,           (* = 243 *)
     SDLK_WORLD_84,           (* = 244 *)
     SDLK_WORLD_85,           (* = 245 *)
     SDLK_WORLD_86,           (* = 246 *)
     SDLK_WORLD_87,           (* = 247 *)
     SDLK_WORLD_88,           (* = 248 *)
     SDLK_WORLD_89,           (* = 249 *)
     SDLK_WORLD_90,           (* = 250 *)
     SDLK_WORLD_91,           (* = 251 *)
     SDLK_WORLD_92,           (* = 252 *)
     SDLK_WORLD_93,           (* = 253 *)
     SDLK_WORLD_94,           (* = 254 *)
     SDLK_WORLD_95,           (* = 255 *)
     SDLK_KP0,                (* = 256 *)
     SDLK_KP1,                (* = 257 *)
     SDLK_KP2,                (* = 258 *)
     SDLK_KP3,                (* = 259 *)
     SDLK_KP4,                (* = 260 *)
     SDLK_KP5,                (* = 261 *)
     SDLK_KP6,                (* = 262 *)
     SDLK_KP7,                (* = 263 *)
     SDLK_KP8,                (* = 264 *)
     SDLK_KP9,                (* = 265 *)
     SDLK_KP_PERIOD,          (* = 266 *)
     SDLK_KP_DIVIDE,          (* = 267 *)
     SDLK_KP_MULTIPLY,        (* = 268 *)
     SDLK_KP_MINUS,           (* = 269 *)
     SDLK_KP_PLUS,            (* = 270 *)
     SDLK_KP_ENTER,           (* = 271 *)
     SDLK_KP_EQUALS,          (* = 272 *)
     SDLK_UP,                 (* = 273 *)
     SDLK_DOWN,               (* = 274 *)
     SDLK_RIGHT,              (* = 275 *)
     SDLK_LEFT,               (* = 276 *)
     SDLK_INSERT,             (* = 277 *)
     SDLK_HOME,               (* = 278 *)
     SDLK_END,                (* = 279 *)
     SDLK_PAGEUP,             (* = 280 *)
     SDLK_PAGEDOWN,           (* = 281 *)
     SDLK_F1,                 (* = 282 *)
     SDLK_F2,                 (* = 283 *)
     SDLK_F3,                 (* = 284 *)
     SDLK_F4,                 (* = 285 *)
     SDLK_F5,                 (* = 286 *)
     SDLK_F6,                 (* = 287 *)
     SDLK_F7,                 (* = 288 *)
     SDLK_F8,                 (* = 289 *)
     SDLK_F9,                 (* = 290 *)
     SDLK_F10,                (* = 291 *)
     SDLK_F11,                (* = 292 *)
     SDLK_F12,                (* = 293 *)
     SDLK_F13,                (* = 294 *)
     SDLK_F14,                (* = 295 *)
     SDLK_F15,                (* = 296 *)

     SDLK_UNKNOWN,
     SDLK_UNKNOWN,
     SDLK_UNKNOWN,

     SDLK_NUMLOCK,            (* = 300 *)
     SDLK_CAPSLOCK,           (* = 301 *)
     SDLK_SCROLLOCK,          (* = 302 *)
     SDLK_RSHIFT,             (* = 303 *)
     SDLK_LSHIFT,             (* = 304 *)
     SDLK_RCTRL,              (* = 305 *)
     SDLK_LCTRL,              (* = 306 *)
     SDLK_RALT,               (* = 307 *)
     SDLK_LALT,               (* = 308 *)
     SDLK_RMETA,              (* = 309 *)
     SDLK_LMETA,              (* = 310 *)
     SDLK_LSUPER,             (* = 311 *)
     SDLK_RSUPER,             (* = 312 *)
     SDLK_MODE,               (* = 313 *)
     SDLK_COMPOSE,            (* = 314 *)
     SDLK_HELP,               (* = 315 *)
     SDLK_PRINT,              (* = 316 *)
     SDLK_SYSREQ,             (* = 317 *)
     SDLK_BREAK,              (* = 318 *)
     SDLK_MENU,               (* = 319 *)
     SDLK_POWER,              (* = 320 *)
     SDLK_EURO,               (* = 321 *)
     SDLK_UNDO                (* = 322 *)
        ])
  in
    (* XXX should also provide the inverse of this function *)
    fun sdlkey n =
      if n < 0 orelse n > Vector.length sdlk
      then SDLK_UNKNOWN
      else 
          let val r = Vector.sub(sdlk, n)
          in
              (* print ("key @ " ^ Int.toString n ^ " is " ^ sdlktos r ^ "\n"); *)
              r
          end

    (* not fast; linear search *)
    fun keysdl k =
        case Vector.findi (fn (_, x) => x = k) sdlk of
            NONE => raise SDL "bad key??"
          | SOME (i, _) => i

    fun sdlktostring k = Int.toString (keysdl k)
    fun sdlkfromstring s = Option.map sdlkey (Int.fromString s) handle Overflow => NONE
  end

  (* PERF can use mlton pointer structure to do this stuff,
     which will improve performance a lot, but at the cost
     of requiring binary compatibility with whatever version of SDL *)
  val surface_width_ = _import "ml_surfacewidth" : ptr -> int ;
  val surface_height_ = _import "ml_surfaceheight" : ptr -> int ;
  val clearsurface_ = _import "ml_clearsurface" : ptr * Word32.word -> unit ;
  val newevent_ = _import "ml_newevent" : unit -> ptr ;
  val free_ = _import "free" : ptr -> unit ;
  val eventtag_ = _import "ml_eventtag" : ptr -> int ;
  val event_keyboard_sym_ = _import "ml_event_keyboard_sym" : ptr -> int ;

  val event8_2nd_ = _import "ml_event8_2nd" : ptr -> int ;
  val event8_3rd_ = _import "ml_event8_3rd" : ptr -> int ;
  val event8_4th_ = _import "ml_event8_4th" : ptr -> int ;

  val event_mmotion_x_ = _import "ml_event_mmotion_x" : ptr -> int ;
  val event_mmotion_y_ = _import "ml_event_mmotion_y" : ptr -> int ;
  val event_mmotion_xrel_ = _import "ml_event_mmotion_xrel" : ptr -> int ;
  val event_mmotion_yrel_ = _import "ml_event_mmotion_yrel" : ptr -> int ;

  val event_mbutton_x_ = _import "ml_event_mbutton_x" : ptr -> int ;
  val event_mbutton_y_ = _import "ml_event_mbutton_y" : ptr -> int ;
  val event_mbutton_button_ = _import "ml_event_mbutton_button" : ptr -> int ;

  val event_joyaxis_value_ = _import "ml_event_joyaxis_value" : ptr -> int ;

  fun clearsurface (s, w) = clearsurface_ (!! s, w)

  fun surface_width s = surface_width_ (!!s)
  fun surface_height s = surface_height_ (!!s)

  val version_ = _import "ml_version_packed" : unit -> Word32.word ;
  fun version () =
      let
          val p = version_ ()
          fun shifted n = Word32.toInt (Word32.andb(0w255, Word32.>>(p, n)))
      in
          { major = shifted 0w16,
            minor = shifted 0w8,
            patch = shifted 0w0 }
      end

  fun convertevent_ e =
   (* XXX need to get props too... *)
    (case eventtag_ e of
       1 => E_Active
         (* XXX more... *)
     | 2 => E_KeyDown { sym = sdlkey (event_keyboard_sym_ e) }
     | 3 => E_KeyUp { sym = sdlkey (event_keyboard_sym_ e) }
     | 4 => E_MouseMotion
         { which = event8_2nd_ e, 
           state = Word8.fromInt (event8_3rd_ e),
           x = event_mmotion_x_ e,
           y = event_mmotion_y_ e,
           xrel = event_mmotion_xrel_ e,
           yrel = event_mmotion_yrel_ e }
     | 5 => E_MouseDown { button = event_mbutton_button_ e,
                          x = event_mbutton_x_ e,
                          y = event_mbutton_y_ e }
     | 6 => E_MouseUp { button = event_mbutton_button_ e,
                        x = event_mbutton_x_ e,
                        y = event_mbutton_y_ e }
     | 7 => E_JoyAxis { which = event8_2nd_ e,
                        axis = event8_3rd_ e,
                        v = event_joyaxis_value_ e }
     | 8 => E_JoyBall
     | 9 => E_JoyHat { which = event8_2nd_ e,
                       hat = event8_3rd_ e,
                       state = Word8.fromInt (event8_4th_ e) }
     | 10 => E_JoyDown
         { which = event8_2nd_ e, 
           button = event8_3rd_ e }
     | 11 => E_JoyUp
         { which = event8_2nd_ e, 
           button = event8_3rd_ e }
     | 12 => E_Quit
     | 13 => E_SysWM
     (* reserved..
     | 14 => 
     | 15 =>
        *)
     | 16 => E_Resize
     | 17 => E_Expose
     | _ => E_Unknown
       )

  fun pollevent () =
    let
      val p = _import "SDL_PollEvent" : ptr -> int ;
      val e = newevent_ ()
    in
      case p e of
        0 => 
          let in
          (* no event *)
            free_ e;
            NONE
          end
      | _ =>
          let 
            val ret = convertevent_ e
          in
            free_ e;
            SOME ret
          end
    end

  (* These need to agree with SDL_mouse.h, but that ain't going anywhere. *)
  val SDL_BUTTON_LEFT = 0w1 : Word.word
  val SDL_BUTTON_MIDDLE = 0w2 : Word.word
  val SDL_BUTTON_RIGHT = 0w3 : Word.word
  val SDL_BUTTON_WHEELUP = 0w4 : Word.word
  val SDL_BUTTON_WHEELDOWN = 0w5 : Word.word

  fun mouse_left state = Word8.andb(state, Word8.<<(0w1, SDL_BUTTON_LEFT - 0w1)) <> 0w0
  fun mouse_middle state = Word8.andb(state, Word8.<<(0w1, SDL_BUTTON_MIDDLE - 0w1)) <> 0w0
  fun mouse_right state = Word8.andb(state, Word8.<<(0w1, SDL_BUTTON_RIGHT - 0w1)) <> 0w0
  fun mouse_wheelup state = Word8.andb(state, Word8.<<(0w1, SDL_BUTTON_WHEELUP - 0w1)) <> 0w0
  fun mouse_wheeldown state = Word8.andb(state, Word8.<<(0w1, SDL_BUTTON_WHEELDOWN - 0w1)) <> 0w0

  local val sc = _import "SDL_ShowCursor" : int -> int ;
  in
    fun show_cursor true = ignore (sc SDL_ENABLE)
      | show_cursor false = ignore (sc SDL_DISABLE)
  end
      
  local val fl = _import "SDL_Flip" : ptr -> unit ;
  in
    fun flip p = fl (!!p)
  end

  val glflip = _import "SDL_GL_SwapBuffers" : unit -> unit ;


  local 
    val mkfscreen = _import "ml_makefullscreen" : int * int -> ptr ;
  in
    fun makefullscreen (w, h) =
      let
        val p = mkfscreen (w, h)
      in
        if p = null
        then raise SDL "couldn't make screen"
        else ref p
      end
  end

  local 
    val mkscreen = _import "ml_makescreen" : int * int -> ptr ;
  in
    fun makescreen (w, h) =
      let
        val p = mkscreen (w, h)
      in
        if p = null
        then raise SDL "couldn't make screen"
        else ref p
      end
  end

  local 
    val mkscreen = _import "ml_glmakescreen" : int * int -> ptr ;
  in
    fun makeglscreen (w, h) =
      let
        val p = mkscreen (w, h)
      in
        if p = null
        then raise SDL "couldn't make screen"
        else ref p
      end
  end


  local val ba = _import "ml_blitall" : ptr * ptr * int * int -> unit ;
        val b  = _import "ml_blit" : ptr * int * int * int * int * ptr * int * int -> unit ;
  in
    fun blitall (s1, s2, x, y) = 
        let in
            (* print ("blitall to " ^ Int.toString x ^ "," ^ Int.toString y ^ "!\n"); *)
            ba (!!s1, !!s2, x, y)
        end
    fun blit (s, sx, sy, sw, sh, d, dx, dy) = b (!!s, sx, sy, sw, sh, !!d, dx, dy)
  end
   
  (* PERF: Probably porting the C code to unsafe ML code would result in significantly
     improved performance. The C call overhead is big and the compiler can't optimize
     over the call boundary. *)
  local val dp = _import "ml_drawpixel" : ptr * int * int * Word32.word * Word32.word * Word32.word -> unit ;
  in
      fun drawpixel (s, x, y, c) =
          let
              val (r, g, b, _) = components32 c
          in
              if x < 0 orelse y < 0
                 orelse x >= surface_width s
                 orelse y >= surface_height s
              then
                  raise SDL ("drawpixel out of bounds: " ^ Int.toString x ^ "," ^ Int.toString y ^
                              " with surface size: " ^ Int.toString (surface_width s) ^ "x" ^
                              Int.toString (surface_height s))
              else dp (!!s, x, y, r, g, b)
          end
  end

  local val dp = _import "ml_drawpixel" : ptr * int * int * Word32.word * Word32.word * Word32.word -> unit ;
  in
      fun clippixel (s, x, y, c) =
          let
              val (r, g, b, _) = components32 c
          in
              if x < 0 orelse y < 0
                 orelse x >= surface_width s
                 orelse y >= surface_height s
              then ()
              else dp (!!s, x, y, r, g, b)
          end
  end

  local val dp = _import "ml_drawpixel" : ptr * int * int * Word32.word * Word32.word * Word32.word -> unit ;
  in
      fun drawcircle (ss, x0, y0, radius, c) =
          let
              val s = !!ss
              val (r, g, b, _) = components32 c

              val sw = surface_width ss
              val sh = surface_height ss

              fun clippixel (x, y) =
                  if x < 0 orelse y < 0
                     orelse x >= sw
                     orelse y >= sh
                  then ()
                  else dp (s, x, y, r, g, b)

              val f = 1 - radius
              val ddF_x = 1
              val ddF_y = ~2 * radius
              val x = 0
              val y = radius

              val () = clippixel(x0, y0 + radius)
              val () = clippixel(x0, y0 - radius)
              val () = clippixel(x0 + radius, y0)
              val () = clippixel(x0 - radius, y0)

              fun loop (x, y, f, ddF_x, ddF_y) =
                  if x < y
                  then 
                      let
                          (*
                          val () =
                              print ("x: " ^ Int.toString x ^
                                     " y: " ^ Int.toString y ^
                                     " f: " ^ Int.toString f ^
                                     " ddfx: " ^ Int.toString ddF_x ^
                                     " ddfy: " ^ Int.toString ddF_y ^ "\n")
                              *)
                          val (y, f, ddF_y) =
                              if f >= 0
                              then (y - 1, 2 + f + ddF_y, 2 + ddF_y)
                              else (y, f, ddF_y)
                          val x = x + 1
                          val ddF_x = ddF_x + 2
                          val f = ddF_x + f
                      in
                          clippixel(x0 + x, y0 + y);
                          clippixel(x0 - x, y0 + y);
                          clippixel(x0 + x, y0 - y);
                          clippixel(x0 - x, y0 - y);
                          clippixel(x0 + y, y0 + x);
                          clippixel(x0 - y, y0 + x);
                          clippixel(x0 + y, y0 - x);
                          clippixel(x0 - y, y0 - x);
                          loop (x, y, f, ddF_x, ddF_y)
                      end
                  else ()
          in
              loop (x, y, f, ddF_x, ddF_y)
          end
  end

  local 
      val dp = _import "ml_drawpixel" : ptr * int * int * Word32.word * Word32.word * Word32.word -> unit ;
      structure W = Word32

      fun pair_swap (x, y) = (y, x)
      fun pair_map f (x, y) = (f x, f y)
      fun pair_map2 f (x1, y1) (x2, y2) = (f (x1, x2), f (y1, y2))

      fun build ((x0, y0), (x1, y1), (dx, dy), (stepx, stepy), post) =
        let val frac0 = dy - Int.quot (dx, 2)
            fun step (x0, y0, frac) =
              if x0 = x1
              then NONE
              else
                  let val (y0, frac) = if frac >= 0 
                                       then (y0 + stepy, frac - dx) 
                                       else (y0, frac)
                      val x0 = x0 + stepx
                      val frac = frac + dy
                  in SOME ((x0, y0, frac), post (x0, y0)) 
                  end
        in ({step = step, seed = (x0, y0, frac0)}, post (x0, y0)) 
        end

      fun line p0 p1 =
        let
            val d = pair_map2 op- p1 p0
            fun abs c = if c < 0 then (~c, ~1) else (c, 1)
            val ((dx', stepx), (dy', stepy)) = pair_map abs d
            val step = (stepx, stepy)
            val d'' as (dx'', dy'') = pair_map (fn n => n * 2) (dx', dy')

            val swap = pair_swap
            val cvt = (fn x => x)
            val seed =
                if dx'' > dy''
                then (p0, p1, d'', step, cvt)
                else (swap p0, swap p1, swap d'', swap step, swap)
        in build seed
        end

  in
      fun drawline (ss, x0, y0, x1, y1, c) =
          let
              val s = !!ss
              val (r, g, b, _) = components32 c

              val sw = surface_width ss
              val sh = surface_height ss

              fun clippixel (x, y) =
                  if x < 0 orelse y < 0
                     orelse x >= sw
                     orelse y >= sh
                  then ()
                  else dp (s, x, y, r, g, b)

              fun app (f : int * int -> unit) p0 p1 =
                  let 
                      val ({step, seed}, v) = line p0 p1
                      fun loop seed =
                          case step seed of
                              NONE => ()
                            | SOME (seed', v) => (f v; loop seed')
                  in 
                      f v;
                      loop seed
                  end
          in
              (* PERF could pre-clip, or stop as soon as we get off screen? *)
              app clippixel (x0, y0) (x1, y1)
          end
  end

  (* PERF don't need bresenham complexity to do this; pretty easy to pre-clip.
     draws corners multiple times *)
  fun drawbox (surf, x0, y0, x1, y1, c) =
      let in
          drawline (surf, x0, y0, x1, y0, c);
          drawline (surf, x1, y0, x1, y1, c);
          drawline (surf, x0, y1, x1, y1, c);
          drawline (surf, x0, y0, x0, y1, c)
      end
      

  (* PERF: similar *)
  (* XXX no alpha.. *)
  local val gp = _import "ml_getpixela" : ptr * int * int * Word8.word ref * Word8.word ref * Word8.word ref * Word8.word ref -> unit ;
  in
      fun getpixel (s, x, y) =
          let
          in
              if x < 0 orelse y < 0
                 orelse x >= surface_width s
                 orelse y >= surface_height s
              then
                  raise SDL ("getpixel out of bounds: " ^ Int.toString x ^ "," ^ Int.toString y ^
                              " with surface size: " ^ Int.toString (surface_width s) ^ "x" ^
                              Int.toString (surface_height s))
              else 
                let
                  val r = ref 0w0
                  val g = ref 0w0
                  val b = ref 0w0
                  val a = ref 0w0
                in
                  gp (!!s, x, y, r, g, b, a);
                  color (!r, !g, !b, !a)
                end
          end
  end

 local val gps = _import "ml_getpixels" : ptr -> ptr;
 in fun getpixels s = gps (!!s)
 end

 local val rgb = _import "ml_is_rgb" : ptr -> int;
 in fun is_rgb s = if rgb (!!s) <> 0 then true else false
 end

 local val bytes = _import "ml_get_bytes_per_pixel" : ptr -> int;
 in fun get_bytes_per_pixel s = bytes (!!s)
 end

  (* for w in [0, 255] we get w/255 of c, (255 - w)/255 of cc *)
  fun colormixfrac8 (c : color, cc : color, w : Word8.word) =
      let
          val w = Word32.fromInt (Word8.toInt w)
          val (r, g, b, a) = components32 c
          val (rr, gg, bb, aa) = components32 cc

          val factor  = w
          val ofactor = 0w256 - factor

          val >> = Word32.>>
          infix >>

          val rrr = (r * factor + rr * ofactor) >> 0w8;
          val ggg = (g * factor + gg * ofactor) >> 0w8;
          val bbb = (b * factor + bb * ofactor) >> 0w8;
          val aaa = (a * factor + aa * ofactor) >> 0w8;
      in
          color32 (rrr, ggg, bbb, aaa)
      end

  (* For f in [0.0, 1.0], we get f * c + (1.0 - f) * cc. *)
  fun colormixfrac (c : color, cc : color, f : real) =
      colormixfrac8 (c, cc, Word8.fromInt (Real.trunc (f * 256.0)))

  fun blitpixel (s, x, y, c) =
      let
          val (_, _, _, a) = components c
          val oldc = getpixel (s, x, y)
          val newc = colormixfrac8 (c, oldc, a)
      in
          drawpixel (s, x, y, newc)
      end

  local val fr = _import "ml_fillrecta" : ptr * int * int * int * int   * Word32.word * Word32.word * Word32.word * Word32.word -> unit ;
  in
    fun fillrect (s1, x, y, w, h, c) = 
        let 
          val (r, g, b, a) = components32 c
        in
          fr (!!s1, x, y, w, h, r, g, b, a)
        end
  end

  local val fs = _import "SDL_FreeSurface" : ptr -> unit ;
  in fun freesurface s = fs (!!s)
  end
      

  local val ms = _import "ml_makesurface" : int * int * int -> ptr ;
  in
      fun makesurface (w, h) =
          let val s = ms (w, h, 1)
          in
              if s = null
              then raise SDL "couldn't create surface"
              else ref s
          end
  end

  (* **** initialization **** *)
  local 
      val init = _import "ml_init" : unit -> int ;
      val plat = _import "ml_platform" : unit -> int ;
      val cmode = _import "ml_consolemode" : unit -> int ;
  in

    val () = 
      case init () of
        0 => raise SDL "could not initialize"
      | _ => ()

    val platform =
      case plat () of
          1 => WIN32
        | 2 => OSX
        | 3 => LINUX
        | ~1 => raise SDL "Platform was not defined in makefile?"
        | _ => raise SDL "sdl.sml is out of sync with sdl.c"

    val console_mode = cmode () <> 0

  end

  val getticks = _import "SDL_GetTicks" : unit -> Word32.word ;
  val delay = _import "SDL_Delay" : int -> unit ;

  structure Joystick =
  struct
      datatype event_state = ENABLE | IGNORE
      type hatstate = joyhatstate
      
      val number = _import "SDL_NumJoysticks" : unit -> int ;
          
      val oj_  = _import "SDL_JoystickOpen" : int -> MLton.Pointer.t ;
      val cj_  = _import "SDL_JoystickClose" : MLton.Pointer.t -> unit ;
      val nj_  = _import "ml_joystickname" : int * char array -> unit ;
      val jes_ = _import "ml_setjoystate" : int -> unit ;

      val na_ = _import "SDL_JoystickNumAxes" : MLton.Pointer.t -> int ;
      val nb_ = _import "SDL_JoystickNumBalls" : MLton.Pointer.t -> int ;
      val nh_ = _import "SDL_JoystickNumHats" : MLton.Pointer.t -> int ;
      val nu_ = _import "SDL_JoystickNumButtons" : MLton.Pointer.t -> int ;

      fun check n = if n >= 0 andalso n < number ()
                    then n
                    else raise SDL "joystick id out of range"

      fun setstate es =
          jes_ (case es of ENABLE => 1 | IGNORE => 0)
                  
      fun name n =
          let val n = check n
              val MAX_NAME = 512
              val r = Array.array (MAX_NAME, #"\000")
          in
              nj_ (n, r);
              readcstring r
          end

      fun openjoy n = ref (oj_ (check n))

      (* nb, there is SDL_JoystickOpened. but why would we want to
         check this when it should be true by invariant? *)
      fun closejoy j = cj_ (!! j)

      (* from SDL_joystick.h *)
      fun hat_centered h = h = (0w0 : Word8.word)
      fun hat_up h    = Word8.andb(h, 0w1) <> 0w0
      fun hat_right h = Word8.andb(h, 0w2) <> 0w0
      fun hat_down h  = Word8.andb(h, 0w4) <> 0w0
      fun hat_left h  = Word8.andb(h, 0w8) <> 0w0

      fun numaxes j = na_ (!! j)
      fun numballs j = nb_ (!! j)
      fun numhats j = nh_ (!! j)
      fun numbuttons j = nu_ (!! j)

      val hatstatetostring = Word8.toString
      fun hatstatefromstring s = Word8.fromString s handle Overflow => NONE
  end

  structure Image =
  struct
      fun load s =
      let 
          val lp = _import "IMG_Load" : string -> ptr ;
          val p = lp (s ^ "\000")
      in
          if (MLton.Pointer.null = p)
          then NONE
          else SOME (ref p)
      end
  end

  structure Util =
  struct

    fun surf2x src =
        let 
            val dst = makesurface(2 * surface_width src, 
                                  2 * surface_height src)
        in
            Util.for 0 (surface_height src - 1)
            (fn yy =>
             Util.for 0 (surface_width src - 1)
             (fn xx =>
              let val color = getpixel(src, xx, yy)
              in fillrect(dst, xx * 2, yy * 2, 2, 2, color)
              end));
            dst
        end

    local val ad = _import "ml_alphadim" : ptr -> ptr ;
    in
      fun alphadim s = 
        let val p = ad(!!s)
        in
          if p = null
          then raise SDL "couldn't alphadim"
          else ref p
        end
    end

    (* by drawing big rects *)
    fun blit16x (src, sx, sy, sw, sh,  dst, dx, dy) =
      Util.for sy (sy + sh - 1)
      (fn yy =>
       Util.for sx (sx + sw - 1)
       (fn xx =>
        let val color = getpixel(src, xx, yy)
        in
          fillrect(dst, 
                   dx + ((xx - sx) * 16),
                   dy + ((yy - sy) * 16),
                   16, 16, color)
        end))

    (* PERF: reduce overlaps in corners *)
    fun outline (surf, n, c) =
        let 
            val w = surface_width surf
            val h = surface_height surf
        in
            fillrect(surf, 0, 0, w, n, c);
            fillrect(surf, 0, 0, n, h, c);
            fillrect(surf, w - n, 0, n, h, c);
            fillrect(surf, 0, h - n, w, n, c)
        end

    fun makealpharect (w, h, c) =
        let
            val s = makesurface (w, h)
        in
            fillrect(s, 0, 0, w, h, c);
            s
        end

    fun makealpharectgrad { w, h, ctop, cbot, bias } =
        let
            val s = makesurface (w, h)
            val rh = real h
        in
            Util.for 0 (h - 1)
            (fn i =>
             (* XXX: no bias yet *)
             let val frac = 1.0 - (real i / rh)
                 val c = colormixfrac (ctop, cbot, frac)
             in fillrect(s, 0, i, w, 1, c)
             end);
            s
        end

  end (* Util *)
end

