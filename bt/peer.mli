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

(** Peer *)

type block = int * int * int

type wire

type write_fun = bigstring -> int -> int -> unit

val create : write_fun -> wire
val handshake : wire -> sha1 -> sha1 -> unit
val on_handshake : wire -> (sha1 * sha1 * extension list) Lwt.t
val peer_choking : wire -> bool Lwt_react.S.t
val am_choking : wire -> bool Lwt_react.S.t
val choke : wire -> unit
val unchoke : wire -> unit
val peer_interested : wire -> bool Lwt_react.S.t
val am_interested : wire -> bool Lwt_react.S.t
val interested : wire -> unit
val uninterested : wire -> unit
val bitfield : wire -> bitfield -> unit
val have : wire -> int -> unit
val has : wire -> bitfield
val on_bitfield : wire -> unit Lwt_react.E.t
val request : wire -> block -> string Lwt.t
val cancel : wire -> block -> unit
val on_request : wire -> (int * int * int * string Lwt.u) Lwt_react.E.t
val peer_requests : wire -> (int * int * int) list Lwt_react.S.t
val requests : wire -> (int * int * int) list Lwt_react.S.t
val set_timeout : wire -> float -> unit
val port : wire -> int -> unit
val on_port : wire -> int Lwt_react.S.t
val set_keep_alive : wire -> bool -> unit
val on_keep_alive : wire -> unit Lwt_react.E.t
val on_upload : wire -> int64 Lwt_react.S.t
val on_download : wire -> int64 Lwt_condition.t
val on_extended : wire -> (int * string) Lwt_react.E.t
val handle_input : wire -> bigstring -> int -> int -> unit
