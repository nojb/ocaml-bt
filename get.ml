type 'a result =
  | Success of 'a * int
  | Failure

type 'a t =
  string -> int -> 'a result

let bind (p : 'a t) (f : 'a -> 'b t) : 'b t =
  fun s i ->
    match p s i with
    | Success (x, j) -> f x s j
    | Failure -> Failure

let return x =
  fun _ i -> Success (x, i)

let fail =
  fun _ _ -> Failure

let (>>=) = bind
let (>|=) p f = bind p (fun x -> return (f x))

module type S = sig
  val uint8 : int t
  val sint8 : int t
  val uint16 : int t
  val sint16 : int t
  val int32 : int32 t
  val int64 : int64 t
end

external swap16 : int -> int     = "%bswap16"
external swap32 : int32 -> int32 = "%bswap_int32"
external swap64 : int64 -> int64 = "%bswap_int64"
external unsafe_get_u8  : string -> int -> int   = "%string_unsafe_get"
external unsafe_get_u16 : string -> int -> int   = "%caml_string_get16u"
external unsafe_get_32 : string -> int -> int32 = "%caml_string_get32u"
external unsafe_get_64 : string -> int -> int64 = "%caml_string_get64u"

let sign8 v =
  (v lsl (Sys.word_size-9)) asr (Sys.word_size-9)

let sign16 v =
  (v lsl (Sys.word_size-17)) asr (Sys.word_size-17)

module BE = struct
  let uint8 s i =
    if String.length s < i+1 then Failure
    else Success (unsafe_get_u8 s i, i+1)
  let sint8 s i =
    if String.length s < i+1 then Failure
    else Success (sign8 (unsafe_get_u8 s i), i+1)
  let uint16 s i =
    if String.length s < i+2 then Failure
    else if Sys.big_endian then Success (unsafe_get_u16 s i, i+2)
    else Success (swap16 (unsafe_get_u16 s i), i+2)
  let sint16 s i =
    if String.length s < i+2 then Failure
    else if Sys.big_endian then
      Success (sign16 (unsafe_get_u16 s i), i+2)
    else
      Success (sign16 (swap16 (unsafe_get_u16 s i)), i+2)
  let int32 s i =
    if String.length s < i+4 then Failure
    else if Sys.big_endian then Success (unsafe_get_32 s i, i+4)
    else Success (swap32 (unsafe_get_32 s i), i+4)
  let int64 s i =
    if String.length s < i+8 then Failure
    else if Sys.big_endian then Success (unsafe_get_64 s i, i+8)
    else Success (swap64 (unsafe_get_64 s i), i+8)
end

module LE = struct
  let uint8 s i =
    if String.length s < i+1 then Failure
    else Success (unsafe_get_u8 s i, i+1)
  let sint8 s i =
    if String.length s < i+1 then Failure
    else Success (sign8 (unsafe_get_u8 s i), i+1)
  let uint16 s i =
    if String.length s < i+2 then Failure
    else if Sys.big_endian then Success (swap16 (unsafe_get_u16 s i), i+2)
    else Success (unsafe_get_u16 s i, i+2)
  let sint16 s i =
    if String.length s < i+2 then Failure
    else if Sys.big_endian then Success (sign16 (swap16 (unsafe_get_u16 s i)), i+2)
    else Success (sign16 (unsafe_get_u16 s i), i+2)
  let int32 s i =
    if String.length s < i+4 then Failure
    else if Sys.big_endian then Success (swap32 (unsafe_get_32 s i), i+4)
    else Success (unsafe_get_32 s i, i+4)
  let int64 s i =
    if String.length s < i+8 then Failure
    else if Sys.big_endian then Success (swap64 (unsafe_get_64 s i), i+8)
    else Success (unsafe_get_64 s i, i+8)
end

let string s i =
  let len = String.length s in
  Success (String.sub s i (len-i), len)

let end_of_input s i =
  if i >= String.length s then Success ((), i)
  else Failure

let either p1 p2 s i =
  match p1 s i with
  | Success _ as res -> res
  | Failure -> p2 s i

exception Get_error

let run p s =
  match p s 0 with
  | Success (x, _) -> x
  | Failure -> raise Get_error

let run_full p s =
  match p s 0 with
  | Success (x, j) ->
    if j <> String.length s then raise Get_error
    else x
  | Failure ->
    raise Get_error
