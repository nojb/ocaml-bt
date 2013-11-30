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
      debug id "Process starting" >>= fun () ->
      f id >>= fun res ->
      debug id "Process terminating gracefully" >>= fun () ->
      Lwt.return res
    with
    | exn ->
      debug id ~exn "Process terminated by exception" >>= fun () ->
      raise_lwt exn

let async ?name f =
  let id = Id.fresh name in
  ignore ((protect f) id)

let spawn ?name f =
  let id = Id.fresh name in
  let t, w = Lwt.wait () in
  let t' = t >>= protect f in
  H.add all_threads id t';
  Lwt.on_termination t' (fun () -> H.remove all_threads id);
  Lwt.wakeup w id;
  id
