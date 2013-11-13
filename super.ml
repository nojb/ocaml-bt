let (>>=) = Lwt.(>>=)

let string_of_msg = function
  | Msg.IAmDying id ->
    Printf.sprintf "IAmDying: %s" (Proc.Id.to_string id)
  | Msg.PleaseDie ->
    "PleaseDie"
  | Msg.SpawnNew (Msg.Worker _) ->
    "SpawnNew: HWorker"
  | Msg.SpawnNew (Msg.Supervisor _) ->
    "SpawnNew: HSupervisor"

exception Stop

type restart_policy =
  | AllForOne
  | OneForOne

type child_info =
  | HWorker
  | HSupervisor of (Msg.super_msg option -> unit)

module H = Hashtbl.Make (Proc.Id)

type t = {
  name : string;
  policy : restart_policy;
  send_super : Msg.super_msg option -> unit;
  msgs : Msg.super_msg Lwt_stream.t;
  send : Msg.super_msg option -> unit;
  children : child_info H.t;
  id : Proc.Id.t
}

let debug t ?exn fmt =
  Printf.ksprintf (fun msg -> Lwt_log.debug_f ?exn "%s %s: %s" t.name
    (Proc.Id.to_string t.id) msg) fmt

let default_stop send_super id =
  send_super (Some (Msg.IAmDying id));
  Lwt.return_unit

let finish_child id = function
  | HWorker ->
    Proc.kill id
  | HSupervisor send ->
    send (Some Msg.PleaseDie)

let spawn_child t = function
  | Msg.Worker f ->
    H.replace t.children (f t.send) HWorker
  | Msg.Supervisor f ->
    let id, send = f t.send in
    H.replace t.children id (HSupervisor send)

let handle_message t msg =
  debug t "%s" (string_of_msg msg) >>= fun () ->
  match msg with
  | Msg.IAmDying id ->
    begin match t.policy with
    | AllForOne ->
      H.iter finish_child t.children;
      raise_lwt Stop
    | OneForOne ->
      H.remove t.children id;
      Lwt.return_unit
    end
  | Msg.PleaseDie ->
    H.iter finish_child t.children;
    raise_lwt Stop
  | Msg.SpawnNew child ->
    spawn_child t child;
    Lwt.return_unit

let start policy name ~children ~send_super ~msgs ~send =
  let run id =
    let t =
      { name; send_super; msgs; send; policy; children = H.create 17; id }
    in
    List.iter (spawn_child t) children;
    try_lwt
      Lwt_stream.iter_s (handle_message t) t.msgs
    with
    | Stop -> Lwt.return_unit
  in
  let id =
    Proc.spawn (Proc.catch run (default_stop send_super))
  in
  id, send
