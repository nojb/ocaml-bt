type file_info = {
  file_path     : string list;
  file_size     : int64
}

type t
(*   name : string; *)
(*   info_hash : SHA1.t; *)
(*   hashes : SHA1.t array; *)
(*   piece_count : int; *)
(*   piece_length : int; *)
(*   block_size : int; *)
(*   last_piece_size : int; *)
(*   total_length : int64; *)
(*   files : file_info list; *)
(*   encoded : string *)
(* } *)

val create : Bcode.t -> t
val get_piece : t -> int -> string
val length : t -> int
val block_number : t -> int -> int
val total_length : t -> int64
val piece_count : t -> int
val piece_length : t -> int -> int
val piece_offset : t -> int -> int64
val block_count : t -> int -> int
val pp : Format.formatter -> t -> unit
val block_offset : t -> int -> int -> int64
(* val block_size : t -> int -> int -> int *)
val hash : t -> int -> SHA1.t
val block : t -> int -> int -> int * int * int
val iter_files : t -> (file_info -> unit Lwt.t) -> unit Lwt.t
