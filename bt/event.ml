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

type pex_flags = {
  pex_encryption : bool;
  pex_seed : bool;
  pex_utp : bool;
  pex_holepunch : bool;
  pex_outgoing : bool
}

type event =
  | PeersReceived of Addr.t list

  | Announce of Tracker.Tier.t * Tracker.event option

  | PeerConnected of Lwt_unix.file_descr * SHA1.t * Bits.t

  | PieceVerified of int

  | PieceFailed of int

  | TorrentComplete

  | Choked of SHA1.t

  | Unchoked of SHA1.t

  | Interested of SHA1.t

  | NotInterested of SHA1.t

  | Have of SHA1.t * int

  | HaveBitfield of SHA1.t * Bits.t

  | BlockRequested of SHA1.t * int * int

  | BlockReceived of SHA1.t * int * int * string

  | HandshakeFailed of Addr.t

  | PeerDisconnected of SHA1.t

  | AvailableMetadata of SHA1.t * int

  | MetaRequested of SHA1.t * int

  | GotMetaPiece of SHA1.t * int * string

  | RejectMetaPiece of SHA1.t * int

  | GotPEX of SHA1.t * (Addr.t * pex_flags) list * Addr.t list

  | DHTPort of SHA1.t * int

  | ConnectPeer of Addr.t * float

  | NoEvent
