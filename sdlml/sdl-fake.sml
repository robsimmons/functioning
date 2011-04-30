(* This fake implementation of SDL is portable to any implementation
   of SML (particularly SML/NJ), but doesn't do anything. It's useful
   for applications that have some debug output using graphics, and
   don't need user input via SDL to run (fake SDL will not return any
   events ever), or for testing non-graphical parts from the toplevel
   loop. *)
structure SDL :> SDL =
struct

  (* XXX as RGBA, though it's not clear we use
     this consistently *)
  type color = Word32.word
  type mousestate  = Word8.word
  type joyhatstate = Word8.word

  exception SDL of string
  exception Invalid

  (* XXX endianness...?! *)
  fun color (r, g, b, a) = 
    Word32.orb
    (Word32.<< (Word32.fromInt (Word8.toInt r), 0w24),
     Word32.orb
     (Word32.<< (Word32.fromInt (Word8.toInt g), 0w16),
      Word32.orb
      (Word32.<< (Word32.fromInt (Word8.toInt b), 0w8),
       Word32.fromInt (Word8.toInt a))))

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

  type surface = unit
  type joy = unit

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

  fun clearsurface (s, w) = ()

  (* ?? maybe should store requested width/height when it's created. *)
  fun surface_width s = 16
  fun surface_height s = 16

  fun pollevent () = NONE
  fun flip _ = ()
  fun makescreen _ = ()
  fun blitall _ = ()
  fun blit _ = ()
  fun alphadim s = ()
  fun drawpixel _ = ()
  fun getpixel _ = color (0w0, 0w0, 0w0, 0w0)
  fun fillrecta _ = ()
  fun fillrect _ = ()
  fun blit16x _ = ()
  fun freesurface _ = ()
  fun makesurface _ = ()      
  fun surf2x _ = ()
  fun init _ = ()

  (* time apparently never advances.
     we could support this using portable means,
     but let's discourage it. *)
  fun getticks () = 0w42 : Word32.word
  fun delay _ = ()

  structure Joystick =
  struct
      datatype event_state = ENABLE | IGNORE
      type hatstate = joyhatstate

      fun number _ = 0
      fun setstate _ = ()
      fun name _ = raise SDL "No joysticks in Fake."
      fun openjoy n = raise SDL "No joysticks in Fake."
      fun closejoy n = raise SDL "No joysticks in Fake."

      (* from SDL_joystick.h *)
      fun hat_centered h = h = (0w0 : Word8.word)
      fun hat_up h    = Word8.andb(h, 0w1) <> 0w0
      fun hat_right h = Word8.andb(h, 0w2) <> 0w0
      fun hat_down h  = Word8.andb(h, 0w4) <> 0w0
      fun hat_left h  = Word8.andb(h, 0w8) <> 0w0

      fun numaxes j = raise SDL "No joysticks in Fake."
      fun numballs j = raise SDL "No joysticks in Fake."
      fun numhats j = raise SDL "No joysticks in Fake."
      fun numbuttons j = raise SDL "No joysticks in Fake."

      val hatstatetostring = Word8.toString
      fun hatstatefromstring s = Word8.fromString s handle Overflow => NONE
  end

  structure Image =
  struct
      (* Pretend we successfully loaded it, for programs that abort
	 when images are not found. *)
      fun load s = SOME()
  end


end

