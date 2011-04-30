(* Copyright (c) 2010 Rob Arnold et al. GPL; see the file COPYING for details.
   
   TODO(twm): This needs to be cleaned up to conform to local standards. *)
signature SDLMIX =
sig
    exception Error of string
end

signature SDLMUSIC =
sig
    type song

    (* Plays nothing. *)
    val empty : song

    val load : string -> song option
    (* If the song is fading, will block *)
    val release : song -> unit

    (* Only one song can play at a time *)
    val play : song -> unit
    val loop : song -> unit
    val playN : int -> song -> unit

    val is_playing : unit -> bool
    val is_paused : unit -> bool

    (* units are ms *)
    val fade_out : int -> unit 

    val pause : unit -> unit
    val resume : unit -> unit

    (* Range is [0:128], linear and clamped *)
    val get_volume : unit -> int
    val set_volume : int -> unit
end

signature SDLSOUND =
sig
    type clip
    type channel

    (* Must be WAV, AIFF, RIFF, OGG or VOC format *)
    val load : string -> clip
    val release : clip -> unit

    (* Range is [0:128], linear and clamped *)
    val get_volume : clip -> int
    val set_volume : int -> clip -> unit

    val play : clip -> unit
    val loop : clip -> channel

    val stop : channel -> unit
end
