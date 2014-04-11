let max_udp_packet_size = 4096

type socket = Lwt_unix.file_descr

open Lwt

let create_socket () =
  let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  sock

let send sock s (ip, p) =
  Lwt_unix.sendto sock s 0 (String.length s) [] (Unix.ADDR_INET (ip, p)) >>= fun n ->
  if n < String.length s then
    Lwt_log.debug_f "[udp] send: could not send all the data (requested=%d,sent=%d)"
      (String.length s) n
  else
    Lwt.return ()

let recv =
  let buf = String.create max_udp_packet_size in
  fun sock ->
    Lwt_unix.recvfrom sock buf 0 max_udp_packet_size [] >>= fun n, iaddr ->
    let addr = match iaddr with
      | Unix.ADDR_UNIX _ -> assert false
      | Unix.ADDR_INET x -> x
    in
    String.sub buf 0 n, addr

(* let listen_on ?(queue_size = 3) host port = *)
(*   let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in *)
(*   let sa = Unix.ADDR_INET (Unix.inet_addr_any, port) in *)
(*   let strm, push = Lwt_stream.create () in *)
(*   let t, w = Lwt.wait () in *)
(*   let w = lazy (Lwt.wakeup w ()) in *)
(*   Lwt_unix.bind sock sa; *)
(*   Lwt_unix.listen sock queue_size; *)
(*   let buf = String.create max_udp_packet_size in *)
(*   let rec loop () = *)
(*     Lwt.pick [ *)
(*       (Lwt_unix.recvfrom sock buf 0 max_udp_packet_size [] >|= fun (n, sa) -> `Read (n, sa)); *)
(*       (t >|= fun () -> `Stop) *)
(*     ] >>= function *)
(*     | `Read (n, sa) -> *)
(*       let sa = match sa with *)
(*         | Unix.ADDR_UNIX _ -> assert false *)
(*         | Unix.ADDR_INET (host, port) -> (host, port) *)
(*       in *)
(*       push (Some (String.sub buf 0 n, sa)); *)
(*       if n >= max_udp_packet_size then *)
(*         Printf.eprintf *)
(*           "UDP: received packet of size >= max_udp_packet_size; some data may have been lost.\n%!"; *)
(*       loop () *)
(*     | `Stop -> *)
(*       Lwt.return () *)
(*   in *)
(*   Lwt.catch loop (fun e -> push None; Lwt_log.info_f ~exn:e "UDP: stop listening") |> ignore; *)
(*   strm, (fun () -> Lazy.force w) *)
