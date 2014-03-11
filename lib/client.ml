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

let max_connections = 5
    
type t = {
  id : Word160.t;
  ih : Word160.t;
  trackers : Tracker.t list;
}

let bittorrent_id_prefix = "-OC0001-"

let download cl =
  Lwt_list.iter_p
    (fun tr ->
      Tracker.query tr cl.ih 44443 cl.id >>= fun resp ->
      let peers = resp.Tracker.peers in
      let peers = List.map (fun (addr, port) -> Peer.of_sockaddr addr port) peers in
      Lwt_list.iter_p (fun p ->
          Peer.connect p >>= fun () ->
          Peer.handshake p ~id:cl.id ~ih:cl.ih >>= fun id' ->
          Peer.request_info p >>= fun s ->
          let info = Info.create (Get.run Bcode.bdecode s) in
          Info.pp Format.std_formatter info;
          Lwt.return ()) peers) cl.trackers

let of_magnet mg =
  let id = Word160.peer_id bittorrent_id_prefix in
  { id; ih = mg.Magnet.xt; trackers = List.map Tracker.create mg.Magnet.tr }
