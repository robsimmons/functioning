structure HTTP :> HTTP =
struct

  datatype result =
      Success of string
    | NetworkFailure of string
    | Rejected of string

  open SDLNet

  exception HTTP of string

  fun parse_url url =
      if size url >= 7 andalso String.substring (url, 0, 7) = "http://"
      then
          let val url = String.substring(url, 7, size url - 7)
              val (site, path) = StringUtil.token (StringUtil.ischar #"/") url
              val path = "/" ^ path
              val (site, port) =
                  case String.fields (StringUtil.ischar #":") site of
                      [site] => (site, 80)
                    | [site, port] =>
                          (site, 
                           case Int.fromString port of
                               NONE => raise HTTP "Alleged URL has non-numeric port"
                             | SOME port => 
                                   (* is that right? or is it 32767? *)
                                   if port <= 0 orelse port > 65535
                                   then raise HTTP "Alleged URL has port out of range"
                                   else port)
                    | _ => raise HTTP "Alleged URL has two colons in its site part"
          in
              if site = ""
              then raise HTTP "Site is empty."
              else ();
              (site, port, path)
          end
      else raise HTTP "Alleged URL doesn't start with http://"

  structure GC = GCharArray

  exception Exit of result
  fun get_url (url : string) (cb : int * int option -> unit) : result =
      let
          val () = cb (0, NONE)
          val (site, port, path) = parse_url url
              
          val () = print ("Connect: " ^ site ^ " : " ^ Int.toString port ^ " GET " ^ path ^ "\n")

          (* XXX if numeric? *)
          val addr = resolvehost site handle SDLNet s => raise Exit (NetworkFailure s)
          val () = cb (0, NONE)

          val () = print ("Resolved to: " ^ atos addr ^ "\n")
          val () = print ("Which reresolves to: " ^ resolveip addr ^ "\n")

          val () = print "Connecting...\n"
          val sock = TCP.connect addr port handle SDLNet s => raise Exit (NetworkFailure s)
          val () = cb (0, NONE)

          val () = print "Connected.\n"

          (* Send the same headers, using lowest common denominator *)
          fun sendheaders () =
              TCP.send sock ("GET " ^ path ^ " HTTP/1.0\r\n" ^
                             (* For distinguishing virtual hosts *)
                             "Host: " ^ site ^ "\r\n" ^
                             "\r\n")
              handle SDLNet s => raise Exit (NetworkFailure s)

          (* First line of result. *)
          fun getresult () =
              case TCP.readline sock of
                  NONE => raise Exit (NetworkFailure "No result")
                | SOME s =>
                      case String.fields (StringUtil.ischar #" ") s of
                          version :: "200" :: rest => ()
                        | version :: rest => raise Exit (Rejected (StringUtil.delimit " " rest))
                        | _ => raise Exit (NetworkFailure ("ill-formed result: " ^ s))

          (* We keep reading headers until there's content.
             This is not a very sophisticated HTTP implementation, so the
             only thing we care about is the Content-Length header, and
             even that is optional. Returns the value and sets the sock up
             so that it's looking at the upcoming content, which should
             be the rest of the connection. *)
          fun getheaders (cl : int option) =
              case TCP.readline sock of
                  NONE => raise Exit (NetworkFailure "No content")
                  (* blank line signals ends of headers *)
                | SOME "" => cl
                | SOME s =>
                      case StringUtil.token (StringUtil.ischar #":") s of
                          ("Content-Length", data) =>
                              (case (cl, Int.fromString data) of
                                   (NONE, SOME i) => (cb (0, SOME i); getheaders (SOME i))
                                 | (_, NONE) => raise Exit (NetworkFailure "ill-formed content-length")
                                 | (SOME _, SOME _) => raise Exit (NetworkFailure "duplicate content-length"))
                        | (_, "") => raise Exit (NetworkFailure ("ill-formed header (no colon-space)"))
                        | _ => getheaders cl

          val () = sendheaders ()
          val () = cb (0, NONE)
          val () = getresult ()
          val () = cb (0, NONE)
          val cl = getheaders NONE
          val () = cb (0, cl)

          val ga = case cl of 
              NONE => GC.empty ()
            | SOME i => GC.init i

          (* Read bytes into ga until the connection is closed. cl just used for callback. *)                  
          fun getcontent cl =
              case TCP.readchar sock of
                  NONE => ()
                | SOME c => 
                      let in
                          GC.append ga c;
                          if GC.length ga mod 1532 = 0
                          then cb (GC.length ga, cl)
                          else ();
                          getcontent cl
                      end
      in
          getcontent cl;
          Success (CharArray.vector (GC.finalize ga))
      end handle Exit r => r
      

end