structure SDLMix :> SDLMIX =
struct
    exception Error of string
end

local
    fun tocstr s = s ^ "\000"
in
    structure SDLMusic :> SDLMUSIC =
    struct
        type song = MLton.Pointer.t

        (* Somewhat arbitrary but not unlimited *)
        val chunk_size = 0w4096
            
        (* Ugh; get rid of this bouncecrab travesty *)
        val empty = MLton.Pointer.null

        val init_ = _import "Mix_OpenAudio" : Word32.word * Word16.word * Word32.word * Word32.word -> int ;
        val default_format_ = _import "ml_mix_default_format" : unit -> Word16.word ;
        val music_load_ = _import "ml_mix_loadmus" : string -> song ;

        val () =
	   if init_ (0w44100, default_format_ (), 0w2, chunk_size) <> 0
           then print "unable to open sdlmixer\n"
           else ()

        fun load path = 
            let val song = (music_load_ o tocstr) path
            in
                if MLton.Pointer.null = song 
                then NONE 
                else SOME song
            end

        val release = _import "Mix_FreeMusic" : song -> unit ;

        (* Really returns int but we ignore *)
        val play_ = _import "Mix_PlayMusic" : song * int -> unit ;

        fun play song = play_ (song, 1)

        fun loop song = play_ (song, ~1)

        fun playN n song = play_ (song, n)

        val is_playing_ = _import "Mix_PlayingMusic" : unit -> int ;

        fun is_playing () = is_playing_ () > 0

        val is_paused_ = _import "Mix_PausedMusic" : unit -> int ;

        fun is_paused () = is_paused_ () > 0

        (* Really returns int but we ignore *)
        val fade_out = _import "Mix_FadeOutMusic" : int -> unit ;

        val pause = _import "Mix_PauseMusic" : unit -> unit ;

        val resume = _import "Mix_ResumeMusic" : unit -> unit ;

        val volume_ = _import "Mix_VolumeMusic" : int -> int ;

        val get_volume = fn () => volume_ ~1

        fun set_volume v =
            let val v = if v < 0 then 0 else v
            in
                volume_ v;
                ()
            end
    end

    structure SDLSound :> SDLSOUND =
    struct
        type clip = MLton.Pointer.t
        type native_channel = Word32.word
        type channel = clip * native_channel

        val any_channel = Word32.~ (0w1)

        val load_ = _import "ml_mix_load_clip" : string -> clip ;
        val load = load_ o tocstr

        val release = _import "Mix_FreeChunk" : clip -> unit ;

        val volume_ = _import "Mix_VolumeChunk" : clip * int -> int ;

        val get_volume = fn clip => volume_ (clip, ~1)

        fun set_volume v clip =
            let val v = if v < 0 then 0 else v
            in
                volume_ (clip, v);
                ()
            end

        val allocate_channels = _import "Mix_AllocateChannels" : int -> int ;
        val reserve_channels = _import "Mix_ReserveChannels" : int -> int ;

        val initial_channels = 16
        val reserved_channels = 8

        val _ = allocate_channels initial_channels
        val _ = reserve_channels reserved_channels

        val play_ = _import "Mix_PlayChannelTimed" : native_channel * clip * int * int -> native_channel ;

        fun play clip = (play_ (any_channel, clip, 0, ~1); ())

        fun loop clip = (clip, play_ (any_channel, clip, ~1, ~1))

        val halt_ = _import "Mix_HaltChannel" : native_channel -> unit ;
        val clip_of = _import "Mix_GetChunk" : native_channel -> clip ;

        fun stop (clip, channel) = if clip_of channel = clip then halt_ channel else ()
    end
end
