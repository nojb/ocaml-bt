val start :
  super_ch:Msg.super_msg Lwt_pipe.t ->
  status_ch:Msg.status_msg Lwt_pipe.t ->
  peer_id:Torrent.peer_id ->
  peer_mgr_ch:Msg.peer_mgr_msg Lwt_pipe.t ->
  ch:Msg.torrent_mgr_msg Lwt_pipe.t ->
  Proc.Id.t
