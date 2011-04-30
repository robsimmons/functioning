
structure HTTPTest =
struct

  open SDLNet
  open HTTP

  exception Nope

  val () = init ()

  val _ = 
      parse_url "http://spacebar.org:4242/file.txt" =
      ("spacebar.org", 4242, "/file.txt") orelse raise Nope
  
  val _ = 
      parse_url "http://spacebar.org/file.txt" =
      ("spacebar.org", 80, "/file.txt") orelse raise Nope

  val _ = 
      parse_url "http://http:4242/file.txt" =
      ("http", 4242, "/file.txt") orelse raise Nope

  val () = ((parse_url ""; raise Nope) handle HTTP _ => ())

  val () = ((parse_url "http://spacebar.org:4:4/file"; raise Nope) handle HTTP _ => ())

  val () = ((parse_url "http://spacebar.org:65536/file"; raise Nope) handle HTTP _ => ())

  val () = ((parse_url "http://spacebar.org:http/file"; raise Nope) handle HTTP _ => ())

  fun expect_37 (_, NONE) = ()
    | expect_37 (_, SOME 37) = ()
    | expect_37 (_, SOME _) = raise Nope

  val () = 
      case get_url "http://spacebar.org/http_test" expect_37 of
          Rejected s => print ("Rejected: " ^ s ^ "\n")
        | NetworkFailure s => print ("Network failed: " ^ s ^ "\n")
        | Success s => if s = "This is for testing sdlnet/http.sml.\n"
                       then print ("Success!\n")
                       else print ("Succeeded with unexpected data: " ^ s ^ "\n")

end