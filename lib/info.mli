type file_info = {
  file_path     : string list;
  file_size     : int64
}

type t = {
  name : string;
  info_hash : Word160.t;
  hashes : Word160.t array;
  piece_length : int;
  total_length : int64;
  files : file_info list;
  encoded : string
}

val create : Bcode.t -> t
val get_piece : t -> int -> string
val length : t -> int
val piece_length : t -> int -> int
val piece_offset : t -> int -> int64
val pp : Format.formatter -> t -> unit
