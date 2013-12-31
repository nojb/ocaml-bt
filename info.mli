type piece_info = {
  piece_offset  : int64;
  piece_length  : int;
  piece_digest  : Word160.t
}

type file_info = {
  file_path     : string list;
  file_size     : int64
}

type t = {
  name : string;
  (* comment : string option; *)
  info_hash : Word160.t;
  (* announce_list : Uri.t list list; *)
  pieces : piece_info array;
  piece_length : int;
  total_length : int64;
  files : file_info list;
  encoded : string
}

val create : Bcode.t -> t

val get_piece : t -> int -> string
val length : t -> int

(* type partial *)

(* val of_info_hash : Word160.t -> (t -> unit) -> partial *)
  
(* val got_info_length : partial -> int -> bool *)
(* val request_lost : partial -> int -> unit *)
(* val next_piece : partial -> int option *)
(* val got_piece : partial -> int -> string -> unit *)
