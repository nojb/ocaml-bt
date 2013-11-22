module PeerId : sig
  type t
  val to_string : t -> string
  val of_string : string -> t
  val of_input_channel : Lwt_io.input_channel -> t Lwt.t
  val to_hex_short : t -> string
end

type peer_id = PeerId.t

module Digest : sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
  val to_bin : t -> string
  val from_bin : string -> t
  val of_input_channel : Lwt_io.input_channel -> t Lwt.t
  val string : string -> t
end

type digest =
  Digest.t

type state =
  | Seeding
  | Leeching

type piece_info = {
  piece_offset  : int64;
  piece_length  : int;
  piece_digest  : digest
}

type file_info = {
  file_path     : string list;
  file_size     : int64
}

type t = {
  name          : string;
  info_hash     : digest;
  announce_list : Uri.t list list;
  pieces        : piece_info array;
  piece_length  : int;
  total_length  : int64;
  files         : file_info list
} 

val bytes_left : Bits.t -> piece_info array -> int64
val make : Bcode.t -> t
val gen_peer_id : unit -> peer_id
val pp : ?fmt:Format.formatter -> t -> unit
