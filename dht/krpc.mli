type trans_id =
  string
    
type dict =
  (string * Bcode.t) list

type msg =
  | Query of string * dict
  | Response of dict
  | Error of int64 * string

val encode : trans_id * msg -> string
val decode_exn : string -> trans_id * msg

type t
type addr = Ip.t * int

val create : (addr -> string -> dict -> msg) -> t

val send : Udp.t -> Ip.t * int -> trans_id * msg -> [`Response of dict | `Error | `Timeout] Lwt.t
