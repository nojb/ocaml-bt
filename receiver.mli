(* open Supervisor *)
open Messages

val start :
  (* monitor:Monitor.t -> *)
  msg_supervisor:(Supervisor.supervisor_msg -> unit) ->
  (* (supervisor_msg -> unit) -> *)
  Lwt_io.input_channel ->
  (msg_ty -> unit) -> unit
  (* Spawn.thread_id *)
