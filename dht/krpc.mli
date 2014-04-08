type trans_id =
  string
    
type dict =
  (string * Bcode.t) list

type t =
  | Query of string * dict
  | Response of dict
  | Error of int64 * string

val encode : trans_id * t -> string
val decode_exn : string -> trans_id * t
