val get :
  net:Eio.Net.t ->
  addr:Eio.Net.Sockaddr.stream ->
  info_hash:string ->
  peer_id:string ->
  port:int ->
  uploaded:int ->
  downloaded:int ->
  left:int ->
  event:string ->
  unit
