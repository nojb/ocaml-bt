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

let section = Log.make_section "Choker"

let info ?exn fmt = Log.info section ?exn fmt

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

type t = {
  peer_mgr : PeerMgr.t;
  torrent : Torrent.t;
  on_manual : unit Lwt_condition.t
}

let create pm tor =
  { peer_mgr = pm; torrent = tor; on_manual = Lwt_condition.create () }

let max_downloaders_unchoke = 4
let unchoking_frequency = 10.0
let optimistic_unchoke_iterations = 3
let rate_computation_iterations = 2

let unchoke_peers bt optimistic =
  let aux compare_peers =
    let peers =
      PeerMgr.fold_peers (fun p l -> if Peer.is_snubbing p then l else p :: l) bt.peer_mgr []
    in
    let peers = List.sort compare_peers peers in
    let rec loop n = function
      | [] -> ()
      | (p :: peers) as l ->
        if n < max_downloaders_unchoke then
          if Peer.am_choking p then begin
            Peer.send_unchoke p;
            if Peer.peer_interested p then loop (n + 1) peers
            else loop n peers
          end else
            loop n peers
        else
          let r = ref (Random.int (List.length l)) in
          let choke_or_optimistic_unchoke p =
            if !r = 0 then Peer.send_unchoke p else Peer.send_choke p;
            decr r
          in
          if optimistic then List.iter choke_or_optimistic_unchoke l
          else List.iter Peer.send_choke l
    in
    loop 0 peers
  in
  if Torrent.is_complete bt.torrent then
    aux (fun a b -> compare (Peer.upload_rate b) (Peer.upload_rate a))
  else
    aux (fun a b -> compare (Peer.download_rate b) (Peer.download_rate a))

let reset_peer_rates bt =
  PeerMgr.iter_peers Peer.reset_rates bt.peer_mgr

let rechoke_downloads bt =
  let h = Torrent.have bt.torrent in
  let doit p =
    let rec loop i =
      if i >= Bits.length h then false
      else if not (Bits.is_set h i) && Peer.has_piece p i then true else loop (i+1)
    in
    if loop 0 then Peer.send_interested p else Peer.send_not_interested p
  in
  PeerMgr.iter_peers doit bt.peer_mgr
  
let rec rechoke_pulse bt optimistic rateiter =
  info "rechoking (optimistic=%d,rateiter=%d)" optimistic rateiter;
  let optimistic = if optimistic = 0 then optimistic_unchoke_iterations else optimistic - 1 in
  let rateiter = if rateiter = 0 then rate_computation_iterations else rateiter - 1 in
  unchoke_peers bt (optimistic = 0);
  rechoke_downloads bt;
  if rateiter = 0 then reset_peer_rates bt;
  Lwt.pick [Lwt_unix.sleep unchoking_frequency; Lwt_condition.wait bt.on_manual] >>= fun () ->
  rechoke_pulse bt optimistic rateiter

let start ch =
  Lwt.async (fun () -> rechoke_pulse ch 1 1)

let rechoke ch =
  Lwt_condition.broadcast ch.on_manual ()
