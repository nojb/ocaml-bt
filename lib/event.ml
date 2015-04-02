(* The MIT License (MIT)

   Copyright (c) 2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

module ARC4 = Nocrypto.Cipher_stream.ARC4

type pex_flags = {
  pex_encryption : bool;
  pex_seed : bool;
  pex_utp : bool;
  pex_holepunch : bool;
  pex_outgoing : bool
}

type addr = Unix.inet_addr * int

type event =
  | PeersReceived of addr list

  | PeerConnected of (ARC4.key * ARC4.key) option * Lwt_unix.file_descr * Bits.t * SHA1.t

  | PieceVerified of int

  | PieceFailed of int

  | TorrentComplete

  | Choked of SHA1.t

  | Unchoked of SHA1.t

  | Interested of SHA1.t

  | NotInterested of SHA1.t

  | Have of SHA1.t * int

  | HaveBitfield of SHA1.t * Bits.t

  | BlockRequested of SHA1.t * int * int * int

  | BlockReceived of SHA1.t * int * int * Cstruct.t

  | PeerDisconnected of SHA1.t

  | AvailableMetadata of SHA1.t * int

  | MetaRequested of SHA1.t * int

  | IncomingConnection of Lwt_unix.file_descr * Unix.sockaddr

  | GotMetaPiece of SHA1.t * int * Cstruct.t

  | RejectMetaPiece of SHA1.t * int

  | GotPEX of SHA1.t * (addr * pex_flags) list * addr list

  | DHTPort of SHA1.t * int

  | ConnectPeer of addr * float

  | ConnectFailed of addr

  | NoEvent
