let (>>=) = Lwt.(>>=)

let debug = Proc.debug

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
  | HSupervisor of Msg.super_msg Lwt_pipe.t

module H = Hashtbl.Make (Proc.Id)

type t = {
  name : string;
  policy : restart_policy;
  super_ch : Msg.super_msg Lwt_pipe.t;
  ch : Msg.super_msg Lwt_pipe.t;
  children : child_info H.t;
  id : Proc.Id.t
}

let default_stop super id =
  Lwt_pipe.write super (Msg.IAmDying id);
  Lwt.return_unit

let finish_child id = function
  | HWorker ->
    Proc.kill id
  | HSupervisor ch ->
    Lwt_pipe.write ch Msg.PleaseDie

let spawn_child t = function
  | Msg.Worker f ->
    let id = f t.ch in
    ignore (debug t.id "Spawned %s" (Proc.Id.to_string id));
    H.replace t.children id HWorker
  | Msg.Supervisor f ->
    let id, ch = f t.ch in
    ignore (debug t.id "Spawned %s" (Proc.Id.to_string id));
    H.replace t.children id (HSupervisor ch)

let handle_message t msg =
  debug t.id "%s" (string_of_msg msg) >>= fun () ->
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

let start policy name ~children ~super_ch ~ch =
  let run id =
    let t =
      { name; super_ch; ch; policy; children = H.create 17; id }
    in
    List.iter (spawn_child t) children;
    try_lwt
      Lwt_pipe.iter_s (handle_message t) ch
    with
    | Stop -> Lwt.return_unit
  in
  let id =
    Proc.spawn ~name run (default_stop super_ch) (fun _ -> Lwt.return_unit)
  in
  id, ch
