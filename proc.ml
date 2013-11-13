module Id = struct
  type t = int
  let last = ref 0
  let to_string x = Printf.sprintf "[#%d]" x
  let fresh () =
    let x = !last in
    incr last;
    x
  let hash x = x
  let equal x y = (x - y) = 0
  let compare x y = x - y
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

let spawn f =
  let id = Id.fresh () in
  let t, w = Lwt.wait () in
  let t' = t >>= f in
  Lwt.on_termination t' (fun () -> H.remove all_threads id);
  Lwt.wakeup w id;
  id

let cleanup f on_stop on_cleanup =
  fun id ->
    try_lwt
      f id >>= fun () ->
      debug id "Process terminating gracefully" >>= fun () ->
      on_cleanup id >>= fun () ->
      on_stop id
    with
    | Lwt.Canceled ->
      debug id "Process terminated by supervisor" >>= fun () ->
      on_cleanup id
    | exn ->
      debug id ~exn "Process terminated by exception" >>= fun () ->
      on_cleanup id >>= fun () ->
      on_stop id

let catch f on_stop =
  cleanup f on_stop (fun _ -> Lwt.return_unit)
