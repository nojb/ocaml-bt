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

type query =
  | Ping
  | FindNode of SHA1.t
  | GetPeers of SHA1.t
  | Announce of SHA1.t * int * string

type node_info = SHA1.t * Addr.t

type nodes =
  | Exact of node_info
  | Closest of node_info list

type peers =
  | Peers of Addr.t list
  | Closest of node_info list

type response =
  | Pong
  | Nodes of nodes
  | Peers of string * peers

type t

val ping : t -> Addr.t -> SHA1.t option Lwt.t
val find_node : t -> Addr.t -> SHA1.t -> (SHA1.t * nodes) Lwt.t
val get_peers : t -> Addr.t -> SHA1.t -> (SHA1.t * string * peers) Lwt.t
val announce : t -> Addr.t -> int -> string -> SHA1.t -> SHA1.t Lwt.t