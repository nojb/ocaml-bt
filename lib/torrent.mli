val download :
  net:Eio.Net.t ->
  clock:Eio.Time.clock ->
  info_hash:string ->
  peer_id:string ->
  meta:Meta.t ->
  peers:Tracker.Response.Peer.t list ->
  unit
