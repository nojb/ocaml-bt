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
  | Have of int
  | HaveBitfield of Bits.t
  | BlockRequested of int * int * int
  | BlockReceived of int * int * string
  | Port of int
  | Finished
  | AvailableMetadata of int
  | MetaRequested of int
  | GotMetaPiece of int * string
  | RejectMetaPiece of int

type event_callback = event -> unit
type get_metadata_func = unit -> int option
type get_block_func = int -> (int * int) list

val create_has_meta : IO.t -> Addr.t -> SHA1.t -> event_callback -> Metadata.t
  -> get_block_func -> t

val create_no_meta : IO.t -> Addr.t -> SHA1.t -> event_callback -> get_metadata_func ->
  t

val start : t -> unit

val id : t -> SHA1.t

val addr : t -> Addr.t

val send_extended_handshake : t -> unit

val peer_choking : t -> bool
val peer_interested : t -> bool
val am_choking : t -> bool
val client_interested : t -> bool
val has_piece : t -> int -> bool
val have : t -> Bits.t
                  
val send_choke : t -> unit
val send_unchoke : t -> unit
val send_interested : t -> unit
val send_not_interested : t -> unit
val send_have : t -> int -> unit
val send_have_bitfield : t -> Bits.t -> unit
val send_cancel : t -> int * int -> unit
  
val send_reject_meta : t -> int -> unit
val send_meta_piece : t -> int -> int * string -> unit

val send_block : t -> int -> int -> string -> unit
  
val upload_rate : t -> float
val download_rate : t -> float
val reset_rates : t -> unit

val got_metadata : t -> Metadata.t -> get_block_func -> unit

val worked_on_piece : t -> int -> bool
val strike : t -> int

val is_seed : t -> bool

val time : t -> float
val piece_data_time : t -> float

val close : t -> unit
