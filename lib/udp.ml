(* The MIT License (MIT)

   Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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
