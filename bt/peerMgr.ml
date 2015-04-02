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

open Event

type addr = Unix.inet_addr * int

type peer =
  { mutable reconnect : bool;
    mutable retries : int;
    mutable timeout : float }

type swarm =
  { size : int;
    wires : (SHA1.t, addr) Hashtbl.t;
    mutable connections : addr list;
    queue : addr Queue.t;
    peers : (addr, peer) Hashtbl.t }

let default_size = 100
let reconnect_wait = [1.; 5.; 15.; 30.; 60.; 120.; 300.; 600.]

let create ?(size = default_size) () =
  { size;
    wires = Hashtbl.create 3;
    connections = [];
    queue = Queue.create ();
    peers = Hashtbl.create 3 }

let drain sw =
  if List.length sw.connections >= sw.size then
    NoEvent
  else
    match try Some (Queue.pop sw.queue) with Queue.Empty -> None with
    | None ->
        NoEvent
    | Some addr ->
        let p = Hashtbl.find sw.peers addr in
        sw.connections <- addr :: sw.connections;
        ConnectPeer (addr, p.timeout)

let add sw addr =
  if Hashtbl.mem sw.peers addr then
    NoEvent
  else begin
    Hashtbl.add sw.peers addr { reconnect = false; retries = 0; timeout = List.hd reconnect_wait };
    Queue.push addr sw.queue;
    drain sw
  end

let peer_disconnected sw id =
  match try Some (Hashtbl.find sw.wires id) with Not_found -> None with
  | Some addr ->
      Hashtbl.remove sw.wires id;
      sw.connections <- List.filter (fun addr' -> addr <> addr') sw.connections;
      let p = Hashtbl.find sw.peers addr in
      begin if not p.reconnect || p.retries >= List.length reconnect_wait then
          Hashtbl.remove sw.peers addr
        else begin
          p.timeout <- List.nth reconnect_wait p.retries;
          p.retries <- p.retries + 1;
          Queue.push addr sw.queue
        end
      end;
      drain sw
  | None ->
      drain sw

let handshake_ok sw addr id =
  Hashtbl.add sw.wires id addr;
  let p = Hashtbl.find sw.peers addr in
  p.reconnect <- true

let handshake_failed sw addr =
  sw.connections <- List.filter (fun addr' -> addr <> addr') sw.connections;
  drain sw

(* let max_peer_count = 20 *)

(* let need_more_peers bt = *)
(*   (Hashtbl.length bt.peers + Hashtbl.length bt.connecting < max_peer_count) (\* && not (is_complete bt) *\) *)

(* let min_upload_idle_secs = 60.0 *)
(* let max_upload_idle_secs = 60.0 *. 5.0 *)

(* let is_bad_peer pm p = *)
(*   if (\* is_seed pm && *\) Peer.is_seed p then begin *)
(*     debug "closing peer %s because we are both seeds" (Peer.to_string p); *)
(*     true (\* FIXME pex *\) *)
(*   end *)
(*   else *)
(*     let relax_strictness_if_fewer_than_n = truncate ((float max_peer_count) *. 0.9 +. 0.5) in *)
(*     let c = peer_count pm in *)
(*     let strictness = *)
(*       if c >= relax_strictness_if_fewer_than_n then 1.0 *)
(*       else float c /. float relax_strictness_if_fewer_than_n *)
(*     in *)
(*     let limit = max_upload_idle_secs -. *)
(*                 ((max_upload_idle_secs -. min_upload_idle_secs) *. strictness) in *)
(*     let idle_time = Unix.time () -. max (Peer.time p) (Peer.piece_data_time p) in *)
(*     if idle_time > limit then begin *)
(*       debug "closing peer %s because it's been %ds since we shared anything" *)
(*         (Peer.to_string p) (truncate idle_time); *)
(*       true *)
(*     end *)
(*     else *)
(*       false *)

(* let close_bad_peers pm = *)
(*   iter_peers (fun p -> if is_bad_peer pm p then close_peer pm p) pm *)

(* let reconnect_pulse_delay = 0.5 *)

(* let rec reconnect_pulse bt = *)
(*   close_bad_peers bt; *)
(*   (\* if need_more_peers bt then *\) *)
(*   (\* debug "reconnect_pulse: will try to connect to %d peers" *\) *)
(*   (\* (max_peer_count - (Hashtbl.length bt.peers + Hashtbl.length bt.connecting)); *\) *)
(*   while need_more_peers bt && List.length bt.saved > 0 do *)
(*     let addr = List.hd bt.saved in *)
(*     bt.saved <- List.tl bt.saved; *)
(*     Hashtbl.add bt.connecting addr (); *)
(*     Lwt.async (fun () -> connect_peer bt addr) *)
(*   done; *)
(*   Lwt_unix.sleep reconnect_pulse_delay >>= fun () -> reconnect_pulse bt *)

(* let pex_delay = 60.0 *)

(* let rec pex_pulse pm = *)
(*   let pex = Hashtbl.fold (fun addr _ l -> addr :: l) pm.peers [] in *)
(*   iter_peers (fun p -> Peer.send_pex p pex) pm; *)
(*   Lwt_unix.sleep pex_delay >>= fun () -> pex_pulse pm *)

(* let torrent_loaded pm m tor get_next_requests = *)
(*   debug "torrent_loaded (have %d pieces)" (Torrent.numgot tor); *)
(*   Hashtbl.iter (fun _ p -> Peer.got_metadata p m (get_next_requests p)) pm.peers; *)
(*   let b = Torrent.have tor in *)
(*   Hashtbl.iter begin fun _ p -> *)
(*     for i = 0 to Bits.length b - 1 do *)
(*       if Bits.is_set b i then Peer.send_have p i *)
(*     done *)
(*   end pm.peers; *)
(*   pm.info <- HasMeta (m, tor, get_next_requests) *)

(* let max_bad_pieces_per_peer = 5 *)

(* let got_bad_piece pm i = *)
(*   iter_peers (fun p -> *)
(*       if Peer.worked_on_piece p i then *)
(*         if Peer.strike p >= max_bad_pieces_per_peer then *)
(*           close_peer pm p) pm *)
