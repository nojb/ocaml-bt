(* open Supervisor *)
open Messages

val start :
  monitor:Monitor.t ->
  (* (supervisor_msg -> unit) -> *)
  Lwt_io.input_channel ->
  (msg_ty -> unit) -> unit
  (* Spawn.thread_id *)
