type msg =
  | Stop
  | TrackerTick of int
  | Start
  | Complete

val start :
  super_ch:Msg.super_msg Lwt_pipe.t ->
  ch: msg Lwt_pipe.t ->
  info_hash: Info.Digest.t ->
  peer_id: Info.peer_id ->
  local_port:int ->
  tier:Uri.t list ->
  status_ch: Status.msg Lwt_pipe.t ->
  peer_mgr_ch: Msg.peer_mgr_msg Lwt_pipe.t ->
  Proc.Id.t
