type t =
  | BInt of int64
  | BString of string
  | BList of t list
  | BDict of (string * t) list

val find : string -> t -> t
val to_list : t -> t list
val to_int64 : t -> int64
val to_int : t -> int
val to_string : t -> string
val to_dict : t -> (string * t) list

val bdecode : t Get.t
val bencode : t -> Put.t

(* val from_file : string -> t *)
(* val from_string : string -> t *)
