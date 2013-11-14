module Id = struct
  type t = int * string option
  let last = ref 0
  let to_string = function
    | (x, None) -> Printf.sprintf "[#%d]" x
    | (x, Some name) -> Printf.sprintf "%s [#%d]" name x
  let fresh name =
    let x = !last in
    incr last;
    (x, name)
  let hash (x, _) = x
  let equal (x, _) (y, _) = (x - y) = 0
  let compare (x, _) (y, _) = x - y
end

module H = Hashtbl.Make (Id)

let all_threads : unit Lwt.t H.t = H.create 17

let kill id =
  try
    Lwt.cancel (H.find all_threads id)
  with
  | Not_found -> ()

let debug id ?exn fmt =
  Printf.ksprintf (fun msg ->
    Lwt_log.debug_f ?exn "%s: %s" (Id.to_string id) msg) fmt

let (>>=) = Lwt.(>>=)

let protect f =
  fun id ->
    try_lwt
      f id >>= fun res ->
      debug id "Process terminating gracefully" >>= fun () ->
      Lwt.return res
    with
    | exn ->
      debug id ~exn "Process terminated by exception" >>= fun () ->
      raise_lwt exn

let exec ?name f =
  let id = Id.fresh name in
  let t, w = Lwt.wait () in
  let t' = t >>= f in
  Lwt.on_termination t' (fun () -> H.remove all_threads id);
  Lwt.wakeup w id;
  id, t'

let run ?name f =
  let _, t = exec ?name (protect f) in
  t

let async ?name f =
  ignore (run ?name f)

let spawn ?name f on_stop on_cleanup =
  let f id =
    try_lwt
      (protect f) id >>= fun () ->
      on_cleanup id >>= fun () ->
      on_stop id
    with
    | Lwt.Canceled ->
      on_cleanup id
    | exn ->
      on_cleanup id >>= fun () ->
      on_stop id
  in
  let id, _ = exec ?name f in
  id
