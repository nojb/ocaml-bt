type t =
  int * (string -> int -> unit)

let bind (n, f) (m, g) =
  (n+m, fun s i -> f s i; g s (i+n))
  
let (>>=) = bind
let (>>) = bind
  
module type S = sig
  val int64 : int64 -> t
  val int32 : int32 -> t
  val int16 : int -> t
  val int8 : int -> t
end

external swap16 : int -> int     = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"
external unsafe_set_8  : string -> int -> int -> unit   = "%string_unsafe_set"
external unsafe_set_16 : string -> int -> int -> unit   = "%caml_string_set16u"
external unsafe_set_32 : string -> int -> int32 -> unit = "%caml_string_set32u"
external unsafe_set_64 : string -> int -> int64 -> unit = "%caml_string_set64u"

module BE = struct
  let int8 n =
    1, fun s i -> unsafe_set_8 s i n
  let int16 n =
    2, fun s i -> if Sys.big_endian then unsafe_set_16 s i n else unsafe_set_16 s i (swap16 n)
  let int32 n =
    4, fun s i -> if Sys.big_endian then unsafe_set_32 s i n else unsafe_set_32 s i (swap32 n)
  let int64 n =
    8, fun s i -> if Sys.big_endian then unsafe_set_64 s i n else unsafe_set_64 s i (swap64 n)
end

module LE = struct
    let int8 n =
    1, fun s i -> unsafe_set_8 s i n
  let int16 n =
    2, fun s i -> if Sys.big_endian then unsafe_set_16 s i (swap16 n) else unsafe_set_16 s i n
  let int32 n =
    4, fun s i -> if Sys.big_endian then unsafe_set_32 s i (swap32 n) else unsafe_set_32 s i n
  let int64 n =
    8, fun s i -> if Sys.big_endian then unsafe_set_64 s i (swap64 n) else unsafe_set_64 s i n
end

let string str =
  String.length str, fun s i -> String.blit str 0 s i (String.length str)

let substring s off len =
  if String.length s > off + len then invalid_arg "Put.substring";
  len, fun str i -> String.blit s 0 str i len

let char c =
  1, fun s i -> s.[i] <- c

let run (n, f) =
  let s = String.create n in
  f s 0;
  s
