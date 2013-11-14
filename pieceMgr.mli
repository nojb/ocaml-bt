module IntSet : Set.S
module BlockSet : Set.S
module PieceBlockSet : Set.S
module IntMap : Map.S

type progress_info = {
  no_blocks : int;
  have_blocks : BlockSet.t;
  pending_blocks : BlockSet.t
}

type db = {
  in_progress : progress_info IntMap.t;
  have : IntSet.t;
  pending : IntSet.t;
  downloading : PieceBlockSet.t;
  all_pieces : Torrent.piece_info array
  (** info about all the pieces in the torrent *)
}

val start :
  super_ch: Msg.super_msg Lwt_pipe.t ->
  ch: Msg.piece_mgr_msg Lwt_pipe.t ->
  status_ch : Msg.status_msg Lwt_pipe.t ->
  db ->
  info_hash : Torrent.Digest.t ->
  Proc.Id.t

val create_piece_db : Bits.t -> Torrent.piece_info array -> db
