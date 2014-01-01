let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

type t = {
  accept : Lwt_unix.file_descr * Unix.sockaddr -> unit;
  stop : unit -> unit;
  mutable port : int option
}

let failwith_lwt fmt =
  Printf.ksprintf (fun msg -> Lwt.fail (Failure msg)) fmt

let port_range = [6881; 6882; 6883; 6884; 6885; 6886; 6887; 6888; 6889]
let max_listen_backlog = 5

let string_of_sockaddr = function
  | Unix.ADDR_UNIX s ->
    s
  | Unix.ADDR_INET (addr, port) ->
    Unix.string_of_inet_addr addr ^ ":" ^ string_of_int port
    
let start_server accept stop_thread : int option =
  let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  let rec accept_loop () =
    Lwt.pick [stop_thread >|= (fun () -> `Stop);
              Lwt_unix.accept fd >|= (fun x -> `Accept x)] >>= function
    | `Stop ->
      Lwt.return_unit
    | `Accept x ->
      accept x;
      accept_loop ()
  in
  let rec loop = function
    | [] ->
      Trace.infof "No available port for the BitTorrent client!";
      None
    | port :: rest ->
      let sa = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
      try
        Lwt_unix.bind fd sa;
        Lwt_unix.listen fd max_listen_backlog;
        Trace.infof "Starting peer server at %s" (string_of_sockaddr sa);
        Lwt.async (fun () ->
            Lwt.catch accept_loop
              (fun exn ->
                 Trace.infof ~exn "Server error";
                 Lwt.return_unit));
        Some port
      with
      | exn ->
        Trace.infof ~exn "Server could not bind to %s, trying next port" (string_of_sockaddr sa);
        loop rest
  in
  loop port_range

let create accept =
  let stop_server, wake_stop = Lwt.wait () in
  let stop () = Lwt.wakeup wake_stop () in
  let self =
    { accept; (* FIXME it is not necessary to save this in [self] ? *)
      stop;
      port = None }
  in
  self.port <- start_server accept stop_server;
  self

let stop self =
  self.stop ()

let port self =
  match self.port with
  | None -> raise Not_found
  | Some port -> port
