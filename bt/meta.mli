type file_info = {
  file_path     : string list;
  file_size     : int64
}

type t = {
  name : string;
  info_hash : SHA1.t;
  hashes : SHA1.t array;
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
val block_count : t -> int -> int
val pp : Format.formatter -> t -> unit
val block_offset : t -> int -> int -> int64
