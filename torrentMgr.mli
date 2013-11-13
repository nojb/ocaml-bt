val start :
  send_super:(Msg.super_msg option -> unit) -> 
  send_status:(Msg.status_msg option -> unit) ->
  peer_id:Torrent.peer_id ->
  send_peer_mgr:(Msg.peer_mgr_msg option -> unit) ->
  msgs:Msg.torrent_mgr_msg Lwt_stream.t ->
  Proc.Id.t
