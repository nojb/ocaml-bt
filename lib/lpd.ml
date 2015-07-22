(* The MIT License (MIT)

   Copyright (c) 2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
   FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
   COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

let mcast_addr = "239.192.152.143"
let mcast_port = 6771

open Cohttp

module Http = Request.Make (String_io.M)

let template =
  Printf.sprintf
    "BT-SEARCH * HTTP/1.1\r\n\
     Host: %s:%u\r\n\
     Port: %u\r\n\
     Infohash: %a\r\n\
     \r\n\r\n" mcast_addr mcast_port

let get h name =
  match Header.get h name with
  | None -> Printf.ksprintf invalid_arg "Header not found : %S" name
  | Some s -> s

let str = Bytes.create 200

let start fd info_hash push =
  let rec loop () =
    Lwt_unix.recvfrom fd str 0 (Bytes.length str) [] >>= fun (n, sa) ->
    let ip, port = match sa with Unix.ADDR_INET (ip, port) -> ip, port | _ -> assert false in
    let buf = { String_io.str; pos = 0; len = n } in
    match Http.read buf with
    | `Ok r when r.Request.meth = `Other "BT-SEARCH" ->
        let ih = get r.Request.headers "Infohash" in
        let p = get r.Request.headers "Port" in
        Log.info "Received announce from = %s:%d ih = %s listen = %s"
          (Unix.string_of_inet_addr ip) port ih p;
        let ih = SHA1.of_raw @@ Cstruct.of_string @@ ih in
        push (ih, (ip, int_of_string p));
        loop ()
    | `Ok r ->
        Log.error "Unxpected HTTP method : %S" (Code.string_of_method r.Request.meth);
        loop ()
    | `Invalid s ->
        Log.error "Invalid HTTP request : %S" s;
        loop ()
    | `Eof ->
        Log.error "Unexpected end of file in HTTP request ";
        loop ()
  in
  loop ()

let start fd info_hash push =
  Lwt.catch
    (fun () -> start fd info_hash push)
    (fun e ->
       Lwt.wrap2 Log.error "loop exn : %S ; restarting ..." (Printexc.to_string e))

let start fd info_hash push =
  let rec loop () = start fd info_hash push >>= loop in
  loop ()

let announce_delay = 5. *. 60.

let announce fd port info_hash =
  let msg = template port SHA1.sprint_hex info_hash in
  let sa = Lwt_unix.ADDR_INET (Unix.inet_addr_of_string mcast_addr, mcast_port) in
  let rec loop () =
    Lwt_unix.sendto fd msg 0 (String.length msg) [] sa >>= fun _ ->
    Lwt_unix.sleep announce_delay >>=
    loop
  in
  loop ()

let announce fd port info_hash =
  Lwt.catch
    (fun () -> announce fd port info_hash)
    (fun e ->
       Lwt.wrap2 Log.error "announce exn : %s ; restarting ..." (Printexc.to_string e))

let announce fd port info_hash =
  let rec loop () = announce fd port info_hash >>= loop in
  loop ()

let start ~port ~info_hash push =
  let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0 in
  let fd2 = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0 in
  Lwt_unix.setsockopt fd Lwt_unix.SO_REUSEADDR true;
  Lwt_unix.setsockopt fd2 Lwt_unix.SO_REUSEADDR true;
  (* Lwt_unix.mcast_set_loop fd2 false; *)
  Lwt_unix.bind fd (Lwt_unix.ADDR_INET (Unix.inet_addr_any, mcast_port));
  Log.debug "Joining multicast group %s:%d" mcast_addr mcast_port;
  (* Lwt_unix.mcast_add_membership fd (Unix.inet_addr_of_string mcast_addr); *)
  Lwt.ignore_result (start fd info_hash push);
  Lwt.ignore_result (announce fd2 port info_hash)
