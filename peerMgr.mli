val max_peers : int ref

val start :
  send_super:(Msg.super_msg option -> unit) ->
  msgs:Msg.peer_mgr_msg Lwt_stream.t ->
  send:(Msg.peer_mgr_msg option -> unit) ->
  peer_id:Torrent.peer_id ->
  Proc.Id.t
