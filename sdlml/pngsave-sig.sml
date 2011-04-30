signature PNGSAVE =
sig
    (* pngsave (filename, width, height, bytes)

       Bytes is width * height * 4 count RGBA. 
       Returns true on success. *)
    val save : string * int * int * Word8.word array -> bool
end