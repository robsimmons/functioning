(* Code for making HTTP queries with SDL_Net. *)
signature HTTP =
sig
    datatype result =
        (* Success, with the data *)
        Success of string
        (* Can't connect, ill-formed headers, etc. With error message. *)
      | NetworkFailure of string
        (* Server 404, etc. With error message. *)
      | Rejected of string

    exception HTTP of string

    (* get_url "http://site:port/path"
       Returns the site, port (default 80) and path components,
       raising HTTP in case it is ill-formed. *)
    val parse_url : string -> string * int * string

    (* get_url url callback 
       Fetch the URL (must be of the form http://site[:port]/path)
       using HTTP GET. The callback is called periodically (including
       immediately) with the number of bytes received and the total
       number of bytes we're expecting, if known. Returns a result
       which can be either success, a network failure, or a server
       error. Raises HTTP if the URL is ill-formed. *)
    val get_url : string -> (int * int option -> unit) -> result

end