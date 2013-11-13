val start :
  Lwt_io.input_channel ->
  Lwt_io.output_channel ->
  peer_mgr_ch : Msg.peer_mgr_msg Lwt_pipe.t ->
  Torrent.Digest.t ->
  pieces : Torrent.piece_info array ->
  piece_mgr_ch : Msg.piece_mgr_msg Lwt_pipe.t ->
  Msg.child list
