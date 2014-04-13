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
  | Choked of (int * int * int) list
  | Unchoked
  | Interested
  | NotInterested
  | Have of int
  | BlockRequested of int * int * int
  | BlockReceived of int * int * string
  | MetaPiece of int * string
  | BitField of Bits.t
  | Port of int
  | Finished
  | AvailableMetadata of int
  | MetaRequested of int
  | GotMetaPiece of int * string
  | RejectMetaPiece of int

val create : Tcp.socket -> Word160.t -> t

val start : t -> (event -> unit) -> unit

val id : t -> Word160.t

val addr : t -> Addr.t

val send_extended_handshake : t -> unit

val peer_choking : t -> bool
val peer_interested : t -> bool
val has_piece : t -> idx:int -> bool
val have : t -> Bits.t
                  
val send_choke : t -> unit
val send_unchoke : t -> unit
val send_interested : t -> unit
val send_not_interested : t -> unit
val send_have : t -> idx:int -> unit

val send_reject_meta : t -> int -> unit
val send_meta_piece : t -> int -> int * string -> unit

val send_block : t -> int -> int -> string -> unit
  
val request_block : t -> int -> int -> int -> string Lwt.t
    
val request_piece : t -> ?block_size:int -> int -> int -> string Lwt.t

val request_meta_piece : t -> int -> unit
