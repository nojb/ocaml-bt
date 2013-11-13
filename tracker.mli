val start :
  send_super:(Msg.super_msg option -> unit) ->
  msgs:Msg.tracker_msg Lwt_stream.t ->
  send:(Msg.tracker_msg option -> unit) ->
  info_hash:Torrent.Digest.t ->
  peer_id:Torrent.peer_id ->
  local_port:int ->
  tier:Uri.t list ->
  send_status:(Msg.status_msg option -> unit) ->
  send_peer_mgr:(Msg.peer_mgr_msg option -> unit) ->
  Proc.Id.t
