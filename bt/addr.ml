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
              
module Ip = struct
  type t = Unix.inet_addr

  let any = Unix.inet_addr_any

  let loopback = Unix.inet_addr_loopback

  let of_string s =
    let he = Unix.gethostbyname s in
    he.Unix.h_addr_list.(0)

  let of_string_noblock s =
    Lwt_unix.gethostbyname s >>= fun he ->
    Lwt.return he.Unix.h_addr_list.(0)

  let to_string ip =
    Unix.string_of_inet_addr ip

  let of_ints a b c d =
    Unix.inet_addr_of_string (Printf.sprintf "%d.%d.%d.%d" a b c d)

  let to_ints ip =
    Scanf.sscanf (Unix.string_of_inet_addr ip) "%d.%d.%d.%d" (fun a b c d -> (a, b, c, d))

  let of_string_compact s =
    bitmatch s with
    | { a : 8; b : 8; c : 8; d : 8 } ->
      of_ints a b c d
end

type t = Ip.t * int

let port (_, p) = p

let ip (ip, _) = ip

let to_string (ip, p) = Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) p

let to_string_compact (ip, p) =
  let (a, b, c, d) = Ip.to_ints ip in
  let pl, ph = p land 0xff, (p land 0xff00) lsr 8 in
  Printf.sprintf "%c%c%c%c%c%c"
    (Char.chr a) (Char.chr b) (Char.chr c) (Char.chr d) (Char.chr ph) (Char.chr pl)

let to_sockaddr (ip, p) =
  Unix.ADDR_INET (ip, p)

let of_sockaddr = function
  | Unix.ADDR_UNIX _ -> failwith "of_sockaddr"
  | Unix.ADDR_INET (ip, p) -> (ip, p)

let of_string_compact s =
  bitmatch s with
  | { ip : 4 * 8 : bitstring; p : 16 } ->
    (Ip.of_string_compact ip, p)

module Set = Set.Make (struct type t1 = t type t = t1 let compare = compare end)
