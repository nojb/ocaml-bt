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

type t

type event =
  | Choked
  | Unchoked
  | Interested
  | Not_interested
  | Have of int

val of_sockaddr : Unix.inet_addr -> int -> t
val of_file_descr : Lwt_unix.file_descr -> t
val connect : t -> unit Lwt.t
  
val handshake : t -> id:Word160.t -> ih:Word160.t -> Word160.t Lwt.t

val peer_choking : t -> bool
val peer_interested : t -> bool
val has_piece : t -> idx:int -> bool
val have : t -> Bits.t
                  
val send_choke : t -> unit
val send_unchoke : t -> unit
val send_interested : t -> unit
val send_not_interested : t -> unit
val send_have : t -> idx:int -> unit
  
val request_block : t -> int -> int -> int -> string Lwt.t
(** [request_block p idx off len] requests the block with index [idx], at offset
    [off] of length [len] from peer [p]. *)
    
val request_piece : t -> ?block_size:int -> int -> int -> string Lwt.t
val request_info : t -> string Lwt.t
    
val add_handler : t -> (event -> unit) -> unit

val wait_ready : t -> unit Lwt.t

(* val has_fast_ext : t -> bool *)
(* val has_lt_ext : t -> bool *)
