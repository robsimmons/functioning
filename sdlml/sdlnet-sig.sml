(* The SDL_Net library provides a simple uniform networking library. *)
signature SDLNET =
sig

    exception SDLNet of string

    (* Raises SDLNet on failure *)
    val init : unit -> unit
    val quit : unit -> unit

    (* A resolved address. *)
    type address

    (* raises SDLNet on failure *)
    val resolvehost : string -> address
    val resolveip : address -> string

    (* returns a string like 127.0.0.1 *)
    val atos : address -> string

    structure TCP :
    sig
        type sock
        (* open address port *)
        val connect : address -> int -> sock
        val closesock : sock -> unit
        (* Returns the address and port *)
        val getpeeraddress : sock -> address * int
        val send : sock -> string -> unit
        val sendarray : sock -> { array : Word8.word Array.array,
                                  start : int,
                                  num : int option } -> unit
        val sendvec : sock -> { vec : Word8.word Vector.vector,
                                start : int,
                                num : int option } -> unit

        (* Blocks until a character is available. Returns NONE if the
           connection was closed by the remote site. *)
        val readchar : sock -> char option
        val readbyte : sock -> Word8.word option

        (* Reads until the character satisfies the predicate,
           or the end of the connection is reached. Returns the
           character that satisfied the predicate (not included
           in the string), or NONE if the connection was closed. *)
        val readcharuntil : sock -> (char -> bool) -> string * char option
        (* Same, but with bytes. *)
        val readbyteuntil : sock -> (Word8.word -> bool) -> 
                            Word8Vector.vector * Word8.word option

        (* Reads until a newline character. Discards carriage returns at any
           time. Returns NONE when the connection is closed at the remote end
           and there are no characters remaining. *)
        val readline : sock -> string option

        (* TODO: ReadExactlyN, easy but more efficient. *)

        (* TODO: TCP_Accept *)
        (* TODO: TCP_Listen *)
    end

    (* TODO: UDP *)

end
