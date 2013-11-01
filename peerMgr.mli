open Messages
(* open Supervisor *)
(* open Spawn *)

val max_peers : int ref

val start :
  monitor:Monitor.t ->
  peer_mgr_ch:peer_mgr_msg Lwt_stream.t ->
  mgr_ch:mgr_msg Lwt_stream.t ->
  w_mgr_ch:(mgr_msg -> unit) ->
  peer_id:Torrent.peer_id ->
  unit
  (* supervisor_msg Lwt_stream.t -> *)
  (* (supervisor_msg -> unit) -> *)
  (* thread_id *)
