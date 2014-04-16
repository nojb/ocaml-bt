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

let write_bitstring sock (s, off, len) =
  assert (off land 7 = 0 && len land 7 = 0);
  let rec loop off len =
    if len <= 0 then Lwt.return ()
    else Lwt_unix.write sock s off len >>= fun n ->
      loop (off + n) (len - n)
  in
  loop (off lsr 3) (len lsr 3)
    
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

let read_int32_be sock =
  read sock 4 >|= fun s ->
  bitmatch Bitstring.bitstring_of_string s with
  | { n : 32 } -> n

let write_int32_be sock n =
  write_bitstring sock (BITSTRING { n : 32 })

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
  let doit () =
    Lwt.catch loop (fun e -> Log.error ~exn:e "error while listening"; Lwt.return ())
  in
  Lwt.async doit;
  (fun () -> Lazy.force w)

let getpeeraddr sock =
  Addr.of_sockaddr (Lwt_unix.getpeername sock)

(* let peer_receive_timeout = 5. *)

(* let set_timeout sock = *)
(*   Lwt_unix.setsockopt_float sock Unix.SO_RCVTIMEO 5. *)
