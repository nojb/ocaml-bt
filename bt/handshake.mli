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

(** Handshake negotiation.  Also handles encryption/decryption of
    connections. *)

type t

type crypto_mode =
  | Require
  (** Require encrypted connections. *)
  | Prefer
  (** Prefer encrypted connections: first try to establish encrypted connection,
      but then retry with an unencrypted one if the first try fails. *)

type encryption_mode =
  | Crypto of crypto_mode
  (** Prefer/require encrypted connections *)
  | Plain
  (** Use plain-text connections *)
    
type result =
  | Success of SHA1.t * Bits.t
  (** Handshake successfull: peer ID, peer extension bitfield. *)
  | Failed
  (** Handshake failed. *)

type handshake_callback = result -> unit

val incoming : id:SHA1.t -> ih:SHA1.t -> encryption_mode -> IO.t -> handshake_callback -> t
(** [incoming id ih m sock cb] creates a new handshake for an peer that has
    contacted us.  [id] is the client ID, [ih] is the torrent info-hash, [m]
    specifies our encryption preferences, [sock] is the socket via which we talk
    to the peer, and [cb] is the callback to invoke with the handshake
    results. *)
  
val outgoing : id:SHA1.t -> ih:SHA1.t -> encryption_mode -> IO.t -> handshake_callback -> t
(** [outgoing id ih m sock cb] creates a new handshake for a peer we have
    contacted.  [id] is the client ID, [ih] is the torrent info-hash, [m]
    specifies our encryption preferences, [sock] is the socket via which we talk
    to the peer, and [cb] is the callback to invoke with the handshake
    results. *)

val abort : t -> unit
(** Abort the handshake.  The callback will be invoked with [Failed]. *)
  
val addr : t -> Addr.t
(** The remote address. *)
