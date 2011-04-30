
structure SDLNet :> SDLNET =
struct
    
    exception SDLNet of string

    exception Invalid
    val null = MLton.Pointer.null
    fun check (ref p) = if p = null then raise Invalid else ()
    fun clear r = r := null

    fun !! (ref p) = if p = null then raise Invalid else p


    (* XXX call GetError. How do you make a SML string out of a C pointer? *)
    fun geterror_ () = "(geterror unimplemented)"

    val initialized = ref false
    fun init() =
        let val init_ = _import "SDLNet_Init" : unit -> int ;
        in
            if !initialized
            then raise SDLNet "already initialized"
            else ();
            (case init_ () of
                 0 => ()
               | _ => raise SDLNet ("init: " ^ geterror_ ()));
            initialized := true
        end

    fun quit() =
        let val quit_ = _import "SDLNet_Quit" : unit -> int ;
        in
            if !initialized
            then ()
            else raise SDLNet "quit: not initialized";
            quit_ ();
            initialized := false
        end

    (* A resolved address. Sorry, IPv4-ever. *)
    (* (stored in network byte order) *)
    type address = Word32.word

    (* val resolvehost : string -> address *)
    fun resolvehost s =
        let
            val rh_ = _import "ml_resolvehost" : string * Word32.word ref -> int ;
            val addr = ref 0w0
        in
            if rh_ (s ^ "\000", addr) = 0
            then !addr
            else raise SDLNet ("couldn't resolve " ^ s ^ ": " ^ geterror_())
        end

    fun resolveip w =
        let
            val ri_ = _import "ml_resolveip" :
                Word32.word * char Array.array * int -> int ;

            (* XXX is there word on the longest a DNS entry can be? *)
            val MAX = 1024
            val a = Array.array (MAX, #" ")
        in
            case ri_ (w, a, MAX) of
                ~1 => raise SDLNet ("resolveip: " ^ geterror_ ())
              | n => CharVector.tabulate (n, fn i => Array.sub(a, i))
        end

    fun atos w =
        String.concat
        [Int.toString (Word32.toInt (Word32.andb (0w255, Word32.>>(w, 0w24)))), ".",
         Int.toString (Word32.toInt (Word32.andb (0w255, Word32.>>(w, 0w16)))), ".",
         Int.toString (Word32.toInt (Word32.andb (0w255, Word32.>>(w, 0w8)))), ".",
         Int.toString (Word32.toInt (Word32.andb (0w255, w)))]

    structure G8 = GrowMonoArrayFn(structure A = Word8Array
                                   structure U = Unsafe.Word8Array)
    structure GC = GCharArray

    structure TCP =
    struct
        (* in C, TCPsocket *)
        type sock = MLton.Pointer.t ref
    
        fun connect (a : address) (p : int) =
            let
                val to_ = _import "ml_tcp_open" : Word32.word * int -> MLton.Pointer.t ;

                val t = to_ (a, p)
            in
                if t = null
                then raise SDLNet ("couldn't connect: " ^ geterror_ ())
                else ref t
            end

        fun closesock r =
            let
                val tc_ = _import "SDLNet_TCP_Close" : MLton.Pointer.t -> unit ;
            in
                tc_ (!!r);
                clear r
            end

        fun getpeeraddress r =
            let
                val ar = ref 0w0
                val pr = ref 0
                val gp_ = _import "ml_getpeeraddress" : MLton.Pointer.t * Word32.word ref * int ref -> unit ;
            in
                gp_ (!!r, ar, pr);
                if !ar = 0w0 
                then raise SDLNet ("Couldn't get peer address: " ^ geterror_ ())
                else (!ar, !pr)
            end

        fun send r s =
            let
                val ts_ = _import "SDLNet_TCP_Send" : MLton.Pointer.t * CharVector.vector * int -> int ;
            in
                if ts_ (!!r, s, size s) = size s
                then ()
                else raise SDLNet ("Not all bytes were sent: " ^ geterror_ ())
            end

        fun bounds (arraylen, start, NONE) =
            if start < 0 orelse start > arraylen
            then raise SDLNet "Bad range in send"
            else arraylen - start
          | bounds (arraylen, start, SOME n) =
            if start < 0 orelse (start + n) > arraylen
            then raise SDLNet "Bad range in send"
            else n

        fun sendarray r { array : Word8.word Array.array,
                          start : int,
                          num : int option } =
            let
                val ts_ = _import "ml_send_offset" : MLton.Pointer.t * Word8.word Array.array * int * int -> int ;
                val n = bounds (Array.length array, start, num)
            in
                if ts_ (!!r, array, start, n) = n
                then ()
                else raise SDLNet ("Not all bytes were sent: " ^ geterror_ ())
            end

        fun sendvec r { vec : Word8.word Vector.vector,
                        start : int,
                        num : int option } =
            let
                val ts_ = _import "ml_send_offset" : MLton.Pointer.t * Word8.word Vector.vector * int * int -> int ;
                val n = bounds (Vector.length vec, start, num)
            in
                if ts_ (!!r, vec, start, n) = n
                then ()
                else raise SDLNet ("Not all bytes were sent: " ^ geterror_ ())
            end

        fun readbyte r =
            let
                val tr_ = _import "SDLNet_TCP_Recv" : MLton.Pointer.t * Word8.word ref * int -> int ;
                val buffer = ref 0w0
            in
                case tr_ (!!r, buffer, 1) of
                    1 => SOME (!buffer)
                  | 0 => NONE
                  | _ => raise SDLNet ("Receive error: " ^ geterror_ ())
            end
        
        val readchar = Option.map (chr o Word8.toInt) o readbyte

        fun ('elt, 'vec, 'ga) readuntil
            (empty : unit -> 'ga)
            (rd : sock -> 'elt option)
            (push : 'ga -> 'elt -> unit)
            (finalize : 'ga -> 'vec)
            (filter : 'elt -> bool)
            (r : sock)
            (pred : 'elt -> bool)
            : 'vec * 'elt option =
            let
                val ga = empty ()
                fun loop () =
                    case rd r of
                        NONE => (finalize ga, NONE)
                      | SOME c =>
                            if pred c
                            then (finalize ga, SOME c)
                            else (if filter c then push ga c else (); loop())
            in
                loop ()
            end

        val readcharuntil = readuntil GC.empty readchar GC.append 
            (CharArray.vector o GC.finalize) (fn _ => true)
        val readbyteuntil = readuntil G8.empty readbyte G8.append 
            (Word8Array.vector o G8.finalize) (fn _ => true)

        fun readline r =
            case readuntil GC.empty readchar GC.append 
                (CharArray.vector o GC.finalize) (fn #"\r" => false | _ => true) 
                r (fn #"\n" => true | _ => false) of
                 (s, SOME _) => SOME s
               | ("", NONE) => NONE
               | (s, NONE) => SOME s

    end

    structure UDP =
    struct
        (* in C, UDPsocket *)
        type sock = MLton.Pointer.t ref

    end

end