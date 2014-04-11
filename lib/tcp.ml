let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)
              
type socket = Lwt_unix.file_descr

let create_socket () =
  Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0
    
let close sock =
  Lwt_unix.close sock
    
let write sock s =
  let rec loop off len =
    if len <= 0 then Lwt.return ()
    else Lwt_unix.write sock s off len >>= fun n ->
      loop (off + n) (len - n)
  in
  loop 0 (String.length s)
    
let read sock n =
  let s = String.create n in
  let rec loop off len =
    if len <= 0 then Lwt.return s
    else
      Lwt_unix.read sock s off len >>= function
      | 0 -> Lwt.fail End_of_file
      | n -> loop (off + n) (len - n)
  in
  loop 0 n

(* let bind sock addr = *)
(*   Lwt_unix.bind sock (Addr.to_sockaddr addr) *)

(* let listen ?(backlog = 5) sock = *)
(*   Lwt_unix.listen sock backlog *)

(* let accept sock = *)
(*   Lwt_unix.accept sock >>= fun (fd, sa) -> *)
(*   Lwt.return (fd, Addr.of_sockaddr sa) *)

let connect sock addr =
  Lwt_unix.connect sock (Addr.to_sockaddr addr)

let listen ?(backlog = 5) sock port handle =
  Lwt_unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_any, port));
  Lwt_unix.listen sock backlog;
  let t, w = Lwt.wait () in
  let w = lazy (Lwt.wakeup w ()) in
  let rec loop () =
    Lwt.pick [(Lwt_unix.accept sock >|= fun (fd, sa) -> `Accept (fd, sa));
              (t >|= fun () -> `Stop)]
    >>= function
    | `Accept (fd, sa) ->
      handle fd (Addr.of_sockaddr sa);
      loop ()
    | `Stop ->
      Lwt.return ()
  in
  Lwt.catch loop (fun e -> Lwt_log.debug_f ~exn:e "listen") |> ignore;
  (fun () -> Lazy.force w)

let getpeeraddr sock =
  Addr.of_sockaddr (Lwt_unix.getpeername sock)

(* let peer_receive_timeout = 5. *)

(* let set_timeout sock = *)
(*   Lwt_unix.setsockopt_float sock Unix.SO_RCVTIMEO 5. *)
