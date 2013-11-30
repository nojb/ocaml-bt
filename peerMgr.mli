val start :
  ch: Msg.peer_mgr_msg Lwt_pipe.t ->
  fs_ch: Fs.msg Lwt_pipe.t ->
  choke_mgr_ch: ChokeMgr.msg Lwt_pipe.t ->
  peer_id: Info.peer_id ->
  info_hash: Info.Digest.t ->
  piece_mgr_ch: PieceMgr.msg Lwt_pipe.t ->
  pieces: Info.piece_info array ->
  Proc.Id.t
