val start :
  msg_supervisor:(Supervisor.supervisor_msg -> unit) ->
  info_hash:Torrent.Digest.t ->
  tier:Uri.t list ->
  peer_id:Torrent.peer_id ->
  local_port:int ->
  w_status_ch:(Messages.status_msg -> unit) ->
  w_peer_mgr_ch:(Messages.peer_mgr_msg -> unit) ->
  (Messages.tracker_msg -> unit)
