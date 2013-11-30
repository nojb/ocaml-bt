type t =
  | BInt of int64
  | BString of string
  | BList of t list
  | BDict of (string * t) list

val find : string -> t -> t option
val to_list : t -> t list option
val to_int64 : t -> int64 option
val to_int : t -> int option
val to_string : t -> string option

val from_file : string -> t

val bencode : t -> string
val from_string : string -> t
