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

type t

type pex_flags = {
  pex_encryption : bool;
  pex_seed : bool;
  pex_utp : bool;
  pex_holepunch : bool;
  pex_outgoing : bool
}

(** Important peer events.  They are broadcast and handled in
    {!Client.handle_peer_event} and there they are re-routed to the relevant objects
    (Peer manager, Torrent, Choker, Requester, etc.). *)
type event =
  | Choked
  (** The peer has choked us. *)
  | Have of int
  (** The peer has sent us a HAVE message. *)
  | HaveBitfield of Bits.t
  (** The peer has sent us a BITFIELD message. *)
  | BlockRequested of int * int * int
  (** The peer has requested a block from us. *)
  | BlockReceived of int * int * string
  (** The peer has sent us a block. *)
  | Finished
  (** The peer connection has closed. *)
  | AvailableMetadata of int
  (** The peer has notified us that they are willing to transmit metadata
      informatio via ut_metadata method. *)
  | MetaRequested of int
  (** The peer has requested a metadata piece from us. *)
  | GotMetaPiece of int * string
  (** The peer has sent us a metadata piece. *)
  | RejectMetaPiece of int
  (** The peer has rejected a request for metadata information from us. *)
  | GotPEX of (Addr.t * pex_flags) list * Addr.t list
  (** The peer has sent us ut_pex data: added peers, dropped peers. *)

type event_callback = event -> unit
type get_metadata_func = unit -> int option
type get_block_func = int -> (int * int) list

val create_has_meta : IO.t -> Addr.t -> SHA1.t -> event_callback -> Metadata.t
  -> get_block_func -> t
(** [create_has_meta sock addr id cb m r] creates a peer in the block-requesting
    stage.  [sock] is the socket used to communicate with the peer, [addr] is
    its address, [id] is its ID, [cb] is the callback invoked on important
    events (currently, this is set to {!Client.handle_peer_event}), [m] is the
    metainfo dictionary, and [r] is the function used to pick blocks to request
    from this peer (see {!Requester.get_next_requests}). *)

val create_no_meta : IO.t -> Addr.t -> SHA1.t -> event_callback -> get_metadata_func ->
  t
(** [create_no_meta sock addr id cb r] creates a peer in the metainfo-requesting
    stage.  [sock] is the socket used to communicate with this peer, [addr] is
    its address, [id] is its ID, [cb] is the callback invoked on important
    events (currently, this is set to {!Client.handle_peer_event}), [m] is the
    metainfo dictionary, and [r] is the function used to pick metainfo pieces to
    request from this peer (see
    {!IncompleteMetadata.get_next_metadata_request}). *)

val start : t -> unit
(** Start the peer's event loop. *)

val id : t -> SHA1.t
(** The peer ID. *)

val addr : t -> Addr.t
(** The peer address. *)

val send_extended_handshake : t -> unit
(** Send LTEP handshake.  Currently supported extension are [ut_metadata] and
    [ut_pex]. *)

val peer_choking : t -> bool
(** Whether the peer is choking us. *)
  
val peer_interested : t -> bool
(** Whether the peer is interested in us. *)
  
val am_choking : t -> bool
(** Whether we are choking this peer. *)
  
val client_interested : t -> bool
(** Whether we are interested in this peer. *)
  
val has_piece : t -> int -> bool
(** Whether this peer has a particular piece. *)
  
val have : t -> Bits.t
(** The bitfield of pieces this peer has. *)
                  
val send_choke : t -> unit
(** Send a CHOKE message.  If the peer is already choked, do not send
    anything. *)
  
val send_unchoke : t -> unit
(** Send an UNCHOKE message.  If the peer is already unchoked, do not send
    anything. *)
  
val send_interested : t -> unit
(** Send a INTERESTED command.  If we are already interested in this peer, do
    not send anything. *)
  
val send_not_interested : t -> unit
(** Send a NOT_INTERESTED message.  If we are already not interested in this
    peer, do not send anything. *)
  
val send_have : t -> int -> unit
(** Send a HAVE message.  If this peer already has the piece, do not send
    anything. *)
  
val send_have_bitfield : t -> Bits.t -> unit
(** Send a BITFIELD message. *)
  
val send_cancel : t -> int * int -> unit
(** Send a CANCEL message. *)
  
val send_reject_meta : t -> int -> unit
(** Reject a request for a metainfo piece. *)
  
val send_meta_piece : t -> int -> int * string -> unit
(** Send a metainfo piece. *)

val send_block : t -> int -> int -> string -> unit
(** Send a block. *)
  
val upload_rate : t -> float
(** The current upload speed. *)
  
val download_rate : t -> float
(** The current download speed. *)
  
val reset_rates : t -> unit
(** Reset the download/upload speed computation. *)

val got_metadata : t -> Metadata.t -> get_block_func -> unit
(** Called when the full metadata is received. The peer should be in the
    metainfo-requesting stage. *)

val worked_on_piece : t -> int -> bool
(** Whether this peer has sent us blocks of a particular piece. *)
  
val strike : t -> int
(** Mark this peer as having participated in a piece that failed its SHA1 hash
    check.  Returns the updated number of strikes. *)

val is_seed : t -> bool
(** Whether this peer already has all the pieces. *)

val time : t -> float
(** Peer creation time. *)
  
val piece_data_time : t -> float
(** The last time this peer sent us a block. *)
  
val close : t -> unit
(** Disconnect this peer. *)

val send_pex : t -> Addr.t list -> unit
(** Sends a periodic PEX message (if supported).  The address list passed
    is the list of currently connected peers. *)
