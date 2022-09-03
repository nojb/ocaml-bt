type file = { length : int; path : string }

type t = {
  announce : string;
  comment : string;
  files : file list;
  piece_length : int;
  pieces : string array;
  info_hash : string;
}

val to_sexp : t Sexp.Encoder.t

val decoder : t Bencode.Decoder.t

val length : t -> int

val piece_length : t -> int -> int
