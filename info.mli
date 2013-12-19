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
  comment : string option;
  info_hash : Word160.t;
  announce_list : Uri.t list list;
  pieces : piece_info array;
  piece_length : int;
  total_length : int64;
  files : file_info list;
}

val create : Bcode.t -> t
