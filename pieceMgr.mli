type db

val start :
  super_ch: Msg.super_msg Lwt_pipe.t ->
  ch: Msg.piece_mgr_msg Lwt_pipe.t ->
  status_ch : Status.msg Lwt_pipe.t ->
  db ->
  info_hash : Info.Digest.t ->
  Proc.Id.t

val create_piece_db : Bits.t -> Info.piece_info array -> db
