(* When running without a console on some platforms (e.g. mingw),
   print will raise an exception. Redefine print so that it *)
val print_works = 
    let in
        (* unfortunately this cannot be the empty string, or it
           is optimized out *)
        print "Made with Standard ML.\n"; 
        true
    end handle _ => false

(* Shadow the global print function. There are other ways to
   print that would fail too, but this is the only one we use. *)
val print = fn s => if print_works then print s else ()
