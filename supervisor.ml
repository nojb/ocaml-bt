open Printf

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let create_stream () =
  let s, w = Lwt_stream.create () in
  s, (fun x -> w (Some x))

exception Stop

module Id = struct
  type t = int * string
  let last_id = ref 0
  let fresh name =
    let new_id = !last_id in
    incr last_id;
    (new_id, name)
  let to_string (id, name) = sprintf "%s [#%d]" name id
  let equal (id, _) (id', _) = id = id'
  let compare (id, _) (id', _) = id - id'
  let hash (id, _) = id
end

type thread_id = Id.t

module H = Hashtbl.Make (Id)

let all_threads = H.create 17

let debug id ?exn (fmt : ('a, 'b, 'c, 'd) format4) =
  match exn with
  | None ->
    ksprintf (fun msg ->
      Lwt_log.debug_f "%s: %s" (Id.to_string id) msg) fmt
  | Some exn ->
    ksprintf (fun msg ->
      Lwt_log.debug_f ~exn "%s: %s" (Id.to_string id) msg) fmt

type supervisor_msg =
  | IAmDying of thread_id
  (** A child process is dying *)
  | PleaseDie
  (** A supervisor has requested me to shut down *)
  | SpawnNew of string * (thread_id -> unit Lwt.t) * child_info
  (** A new worker thread should be started  *)

and child_info =
  | HSupervisor of (supervisor_msg -> unit)
  (** This child is a supervisor, the argument is
      a callback to send messages to that supervisor *)
  | HWorker
  (** This child is a worker thread *)

let string_of_child_info = function
  | HSupervisor _ -> "HSupervisor"
  | HWorker -> "HWorker"

let string_of_msg = function
  | IAmDying id ->
    sprintf "IAmDying: id: %s" (Id.to_string id)
  | PleaseDie ->
    "PleaseDie"
  | SpawnNew (name, _, ci) ->
    sprintf "SpawnNew: name: %s child_info: %s" name (string_of_child_info ci)

module M = Map.Make (Id)

type restart_policy =
  | AllForOne
  (** If any of the supervisor's children dies, then
      all the rest are canceled and the supervisor's death
      is signaled to its supervisor in turn *)
  | OneForOne
  (** If any of the supervisor's children dies, it
      is noted but no action is taken in response to it *)

type state = {
  (* name : string; *)
  chan : supervisor_msg Lwt_stream.t;
  (** Stream on which we receive messages from from our
      supervisor and from our children processes *)
  msg_me : supervisor_msg -> unit;
  (** This callback is passed to any new child process
      so that they can send messages back to this supervisor *)
  (* msg_supervisor : supervisor_msg -> unit; *)
  (** Callback to send messages to our supervisor *)
  policy : restart_policy;
  (** The supervisor's policy *)
}

let default_cleanup () =
  Lwt.return_unit

let null_msg_supervisor =
  fun _ -> ()

(** Spawns a process that will notify a supervisor via
    [msg_supervisor] when it dies *)
let spawn
  ?msg_supervisor:(msg_supervisor=null_msg_supervisor) ~name
  ?cleanup:(cleanup=default_cleanup) f =
  let id = Id.fresh name in
  let t, w = Lwt.wait () in
  let t' = t >>= fun () ->
    debug id "Starting" >>
    try_lwt
      f id >> debug id "Stopped" >> (msg_supervisor (IAmDying id);
      Lwt.return ())
    with
    | Lwt.Canceled ->
      (* This should only happen when canceled from
         the supservisor; if a process is canceled
         from any other part, then the supervisor will
         not be alerted to the fact that the process has
         died and will keep tracking it. It will be
         in effect a 'Zombie' process *)
      debug id "Canceled" >>= cleanup
    | exn ->
      debug id ~exn "Exiting due to Ex" >>= cleanup >|= fun () ->
      msg_supervisor (IAmDying id)
  in
  H.add all_threads id t';
  Lwt.on_termination t' (fun () -> H.remove all_threads id);
  Lwt.wakeup w ();
  id

let kill id =
  try
    Lwt.cancel (H.find all_threads id)
  with
  | Not_found -> ()

(** Finishes the child. Note that it is not
    necessary to remove the child from the
    children's list [st.children] because this function
    is only called when the supervisor is dying. *)
let finish_child id st id' = function
  | HWorker ->
    kill id'
  | HSupervisor msg_sup ->
    msg_sup PleaseDie

let handle_message id st msg children : child_info M.t Lwt.t =
  debug id "%s" (string_of_msg msg) >>
  match msg with
  | IAmDying id' ->
    begin match st.policy with
    | AllForOne ->
      (* iterates serially over every other child *)
      (* Lwt_list.iter_s (finish_child c) st.children *)
      M.iter (finish_child id st) children;
      raise_lwt Stop
    | OneForOne ->
      Lwt.return (M.remove id' children)
    end
  | SpawnNew (name, f, ci) ->
    (* debug id "Spawning: %s" name >>= fun () -> *)
    let id' = spawn ~msg_supervisor:st.msg_me ~name f in
    Lwt.return (M.add id' ci children)
  | PleaseDie ->
    M.iter (finish_child id st) children;
    raise_lwt Stop

(* let spawn name w_sup_ch ?cleanup:(cleanup=default_cleanup) f = *)
(*   Spawn.spawn name (fun id -> *)
(*     try_lwt *)
(*       f id *)
(*     with *)
(*     | Lwt.Canceled -> *)
(*       (* This should only happen when canceled from *)
(*        * the supservisor; if a process is canceled *)
(*        * from any other part, then the supervisor will *)
(*        * not be alerted to the fact that the process has *)
(*        * died and will keep tracking it. *) *)
(*       debug id "Process Canceled"; *)
(*       cleanup () *)
(*     | Stop -> *)
(*       debug id "Process Stopped"; *)
(*       cleanup () >|= fun () -> w_sup_ch (IAmDying id) *)
(*     | exn -> *)
(*       debug id ~exn:exn "Process Exiting due to Ex"; *)
(*       cleanup () >|= fun () -> w_sup_ch (IAmDying id)) *)

let start policy fromSup =
  let from_child, msg_me = create_stream () in
  let chan = Lwt_stream.choose [from_child; fromSup] in
  let st = {chan; msg_me; policy} in
  let event_loop id =
    try_lwt
      Lwt_stream.fold_s (handle_message id st) chan M.empty >>= fun _ ->
      Lwt.return ()
    with
    | Stop -> Lwt.return ()
  in
  event_loop

let spawn_worker msg_supervisor name f =
  msg_supervisor (SpawnNew (name, f, HWorker))

let spawn_supervisor msg_supervisor name policy =
  let supC, msgSupC = create_stream () in
  let f = start policy supC in
  msg_supervisor (SpawnNew (name, f, HSupervisor msgSupC));
  msgSupC

(* let start_top policy name = *)
(*   let supC, msg = create_stream () in *)
(*   ignore (spawn name (start policy msg supC)); *)
(*   msg *)

let top_level name policy =
  let supC, msgSupC = create_stream () in
  ignore (spawn ~name (start policy supC));
  msgSupC
