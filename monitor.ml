type policy =
  | AllForOne
  | OneForOne

let last_stamp = ref 0

let fresh_stamp () =
  let stamp = !last_stamp in
  incr last_stamp;
  stamp

type id = {
  name : string;
  parent : t option;
  stamp : int
}

and t = {
  mid : id;
  mutable children : (id * child) list;
  policy : policy;
  mutable dying : bool
}

and child =
  | Monitor of t
  | Worker

module ThreadId = struct
  type t = id
  let compare id1 id2 = id1.stamp - id2.stamp
  let to_string id =
    Printf.sprintf "%s [#%d]" id.name id.stamp
end

let current id =
  id.parent

let fresh_id parent name = {
  stamp = fresh_stamp ();
  parent;
  name
}

let create ?parent policy name = {
  mid = fresh_id parent name;
  children = [];
  policy;
  dying = false
}

let all_threads : (int, unit Lwt.t) Hashtbl.t = Hashtbl.create 17

let debug id ?exn fmt =
  match exn with
  | None ->
    Printf.ksprintf (fun msg ->
      Lwt_log.debug_f "%s [#%d]: %s%!" id.name id.stamp msg) fmt
  | Some exn ->
    Printf.ksprintf (fun msg ->
      Lwt_log.debug_f ~exn "%s [#%d]: %s%!" id.name id.stamp msg) fmt

let name m =
  m.mid.name

let kill id =
  try
    Lwt.cancel (Hashtbl.find all_threads id.stamp)
  with
  | Not_found -> ()

let rec die m =
  let kill_one (id, child) =
    match child with
    | Worker ->
      (* We do not need to remove id from our list of
       * children because since we are dying anyway the whole
       * thing is going away. *)
      kill id
    | Monitor m ->
      die m
  in
  if not m.dying then begin
    m.dying <- true;
    List.iter kill_one m.children;
    match m.mid.parent with
    | None -> ()
    | Some m' ->
      m'.children <- List.remove_assq m.mid m'.children;
      match m'.policy with
      | AllForOne -> die m'
      | OneForOne -> ()
  end

let default_cleanup () =
  Lwt.return_unit

let (>>=) = Lwt.(>>=)

(* FIXME there is a bug: when we [die m] then we kill
 * a number of threads via [kill id]. In turn the [on_termination]
 * call below will cause [die m] to be called recursively - clearly
 * something we do not want.
 * We could add a flag [dying] to [m] so that [die m] would not
 * do anything if [m] is already dying. *)

let spawn ?parent ~name ?cleanup:(cleanup=default_cleanup) f =
  let id = fresh_id parent name in
  let handle_exn = function
    | Lwt.Canceled ->
      (* This should only happen when canceled from
       * the supservisor; if a process is canceled
       * from any other part, then the supervisor will
       * not be alerted to the fact that the process has
       * died and will keep tracking it. *)
      debug id "Process Canceled" >>= cleanup
    (* | Stop -> *)
    (*   debug id "Process Stopped"; *)
    (*   cleanup () >|= fun () -> w_sup_ch (IAmDying id) *)
    | exn ->
      debug id ~exn:exn "Process Exiting due to Ex" >>= cleanup
  in
  let prune_child () =
    (* ignore (debug id "Process Finished"); *)
    Hashtbl.remove all_threads id.stamp;
    match id.parent with
    | Some m ->
      m.children <- List.remove_assq id m.children;
      begin match m.policy with
      | AllForOne -> die m
      | OneForOne -> ()
      end
    | None -> ()
  in
  let t, w = Lwt.wait () in
  let t' = t >>= fun () ->
    match parent with
    | None -> f id >> debug id "Process Stopped"
    | Some m ->
      debug m.mid "Starting Process: %s" (ThreadId.to_string id) >>
      f id >> debug id "Process Stopped"
  in
  Hashtbl.add all_threads id.stamp t';
  Lwt.on_termination t' prune_child;
  (* Lwt.on_success t' (fun _ -> ignore (debug id "Process Stopped")); *)
  Lwt.on_failure t' (fun exn -> ignore (handle_exn exn));
  Lwt.wakeup w ()
