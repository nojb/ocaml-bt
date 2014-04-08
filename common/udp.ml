let max_udp_packet_size = 4096

type address = Unix.inet_addr * int

type t = Lwt_unix.file_descr

let to_string (addr, port) =
  Printf.sprintf "%s:%d" (Unix.string_of_inet_addr addr) port

let to_string_compact (addr, port) =
  let s = Unix.string_of_inet_addr addr in
  Scanf.sscanf s "%d.%d.%d.%d" (fun a b c d -> assert false)

open Lwt

let listen_on ?host ?port () =
  let sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  let sa = Unix.INET_ADDR (Unix.inet_addr_any, port) in
  let strm, push = Lwt_stream.create () in
  let t, w = Lwt.wait () in
  let w = lazy (Lwt.wakeup w ()) in
  Lwt_unix.bind sock sa;
  Lwt_unix.listen sa queue_size;
  let buf = String.create max_udp_packet_size in
  let rec loop () =
    Lwt.pick [
      (Lwt_unix.recvfrom sock buf 0 max_udp_packet_size [] >|= fun (n, sa) -> `Read (n, sa));
      (t >|= fun () -> `Stop)
    ] >>= function
    | `Read (n, sa) ->
      push (Some (String.sub buf 0 n, sa));
      if n >= max_udp_packet_size then
        Printf.eprintf
          "UDP: received packet of size >= max_udp_packet_size; some data may have been lost.\n%!";
      loop ()
    | `Stop ->
      Lwt.return ()
  in
  Lwt.catch loop (fun e -> push None; Lwt_log ~exn:e "UDP: stop listening") |> ignore;
  strm, (fun () -> Lazy.force w)
