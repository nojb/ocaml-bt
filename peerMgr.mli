val max_peers : int ref

val start :
  super_ch:Msg.super_msg Lwt_pipe.t ->
  ch:Msg.peer_mgr_msg Lwt_pipe.t ->
  peer_id:Torrent.peer_id ->
  Proc.Id.t
