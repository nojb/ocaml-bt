val start :
  super_ch: Msg.super_msg Lwt_pipe.t ->
  ch: Msg.peer_mgr_msg Lwt_pipe.t ->
  peer_id: Info.peer_id ->
  info_hash: Info.Digest.t ->
  piece_mgr_ch: Msg.piece_mgr_msg Lwt_pipe.t ->
  pieces: Info.piece_info array ->
  Proc.Id.t
