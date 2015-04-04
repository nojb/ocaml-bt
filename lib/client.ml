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

module L    = Log
module Log  = Log.Make (struct let section = "Client" end)
module ARC4 = Nocrypto.Cipher_stream.ARC4
module Cs   = Nocrypto.Uncommon.Cs

let (>>=) = Lwt.(>>=)

let listen_ports = [50000]
let choke_timeout = 5.
let rechoke_interval = 10.
let rechoke_optimistic_duration = 2
let rechoke_slots = 10
let block_size = 1 lsl 14 (* 16384 *)
let max_requests = 5

open Event

type t =
  { id : SHA1.t;
    ih : SHA1.t;
    trackers : Uri.t list;
    peer_mgr : PeerMgr.swarm;
    chan : event Lwt_stream.t;
    push : event -> unit }

let proto = Cstruct.of_string "\019BitTorrent protocol"

let extensions =
  let bits = Bits.create (8 * 8) in
  (* Bits.set bits Wire.ltep_bit; *)
  (* Bits.set bits Wire.dht_bit; *)
  Bits.to_cstruct bits

let handshake_len =
  Cstruct.len proto + 8 (* extensions *) + 20 (* info_hash *) + 20 (* peer_id *)

let handshake_message peer_id info_hash =
  Cs.concat [ proto; extensions; SHA1.to_raw info_hash; SHA1.to_raw peer_id ]

let parse_handshake cs =
  assert (Cstruct.len cs = handshake_len);
  match Cstruct.get_uint8 cs 0 with
  | 19 ->
      let proto' = Cstruct.sub cs 0 20 in
      if Cs.equal proto' proto then
        let ext = Cstruct.sub cs 20 8 in
        let info_hash = Cstruct.sub cs 28 20 in
        let peer_id = Cstruct.sub cs 48 20 in
        (ext, SHA1.of_raw info_hash, SHA1.of_raw peer_id)
      else
        failwith "bad proto"
  | _ ->
      failwith "bad proto length"

let buf_size = 1024

let negotiate fd t =
  let read_buf = Cstruct.create buf_size in
  let rec loop = function
    | `Ok (t, Some cs) ->
        Lwt_cstruct.complete (Lwt_cstruct.write fd) cs >>= fun () ->
        loop (Handshake.handle t Cs.empty)
    | `Ok (t, None) ->
        Lwt_cstruct.read fd read_buf >>= begin function
        | 0 ->
            failwith "eof"
        | n ->
            loop (Handshake.handle t (Cstruct.sub read_buf 0 n))
        end
    | `Error err ->
        failwith err
    | `Success (mode, rest) ->
        Lwt.return (`Ok (mode, rest))
  in
  loop t

let connect_to_peer info_hash ip port timeout =
  let my_id = SHA1.generate () in (* FIXME FIXME *)
  let fd = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  Lwt_unix.gethostbyaddr ip >>= fun he ->
  let sa = Lwt_unix.ADDR_INET (he.Lwt_unix.h_addr_list.(0), port) in
  Lwt_unix.connect fd sa >>= fun () ->
  negotiate fd Handshake.(outgoing ~info_hash Both) >>= function
  | `Ok (mode, rest) ->
      Lwt_cstruct.complete (Lwt_cstruct.write fd) (handshake_message info_hash my_id) >>= fun () ->
      assert (Cstruct.len rest <= handshake_len);
      let hs = Cstruct.create handshake_len in
      Cstruct.blit rest 0 hs 0 (Cstruct.len rest);
      Lwt_cstruct.complete (Lwt_cstruct.read fd) (Cstruct.shift hs (Cstruct.len rest)) >>= fun () ->
      let ext, info_hash', peer_id = parse_handshake hs in
      assert (SHA1.equal info_hash info_hash');
      Lwt.return (mode, fd, ext, peer_id)
  | `Error _ ->
      assert false

let connect_to_peer info_hash push = function
  | Some (addr, timeout) ->
      let ip, port = addr in
      ignore
        (Lwt.try_bind
           (fun () -> connect_to_peer info_hash ip port timeout)
           (fun (mode, fd, ext, peer_id) ->
              Lwt.wrap1 push @@ PeerConnected (mode, fd, Bits.of_cstruct ext, peer_id))
           (fun _ -> Lwt.wrap1 push @@ ConnectFailed addr))
  | None -> ()

  (* | TorrentLoaded dl -> *)
  (*         (\* debug "torrent loaded (good=%d,total=%d)" *\) *)
  (*         (\*   (Torrent.numgot dl) (Metadata.piece_count meta - Torrent.numgot dl); *\) *)
  (*         (\* PeerMgr.torrent_loaded bt.peer_mgr meta dl (get_next_requests bt); FIXME FIXME *\) *)
  (*         let ch = Choker.create bt.peer_mgr dl in *)
  (*         if Torrent.is_complete dl then *)
  (*           bt.stage <- HasMeta (meta, Seeding (dl, ch)) *)
  (*         else begin *)
  (*           let r = Requester.create meta dl in *)
  (*           bt.stage <- HasMeta (meta, Leeching (dl, ch, r)); *)
  (*           PeerMgr.iter_peers (fun p -> Requester.got_bitfield r (Peer.have p)) bt.peer_mgr *)
  (*         end; *)
  (*         Choker.start ch *)
(*     end *)

let am_choking peers id =
  try
    let p = Hashtbl.find peers id in
    Peer.am_choking p
  with
  | Not_found -> true

let peer_interested peers id =
  try
    let p = Hashtbl.find peers id in
    Peer.peer_interested p
  with
  | Not_found -> false

let welcome push mode fd exts id =
  let p = Peer.create id (fun e -> push (PeerEvent (id, e))) fd mode in
  Peer.extended_handshake p;
  (* if Bits.is_set exts Wire.dht_bit then Peer.send_port p 6881; (\* FIXME fixed port *\) *)
  p

let rechoke_compare (p1, salt1) (p2, salt2) =
  if Peer.download_speed p1 <> Peer.download_speed p2 then
    compare (Peer.download_speed p2) (Peer.download_speed p1)
  else
  if Peer.upload_speed p1 <> Peer.upload_speed p2 then
    compare (Peer.upload_speed p2) (Peer.upload_speed p1)
  else
  if Peer.am_choking p1 <> Peer.am_choking p2 then
    compare (Peer.am_choking p1) (Peer.am_choking p2)
  else
    compare salt1 salt2

let rechoke peers =
  let rec loop opt nopt =
    let opt, nopt = if nopt > 0 then opt, nopt - 1 else None, nopt in
    let wires =
      let add _ p wires =
        match (* Peer.is_seeder p, *)false, opt with (* FIXME FIXME *)
        | true, _ ->
            Peer.choke p;
            wires
        | false, Some opt when SHA1.equal (Peer.id p) opt ->
            wires
        | false, _ ->
            (p, Random.int max_int) :: wires
      in
      Hashtbl.fold add peers []
    in
    let wires = List.sort rechoke_compare wires in
    let rec select n acc = function
      | (p, _) as w :: wires when n < rechoke_slots ->
          let n = if Peer.peer_interested p then n + 1 else n in
          select n (w :: acc) wires
      | wires ->
          begin match opt with
          | Some _ ->
              acc, wires, opt, nopt
          | None ->
              let wires = List.filter (fun (p, _) -> Peer.peer_interested p) wires in
              if List.length wires > 0 then
                let (p, _) as opt = List.nth wires (Random.int (List.length wires)) in
                (opt :: acc), wires, Some (Peer.id p), rechoke_optimistic_duration
              else
                acc, wires, None, nopt
          end
    in
    let unchoke, choke, opt, nopt = select 0 [] wires in
    List.iter (fun (p, _) -> Peer.unchoke p) unchoke;
    List.iter (fun (p, _) -> Peer.choke p) choke;
    Lwt_unix.sleep choke_timeout >>= fun () -> loop opt nopt
  in
  loop None rechoke_optimistic_duration

(* Requester BEGIN *)

(* type active_block = { *)
(*   a_piece : int; *)
(*   a_block : int; *)
(*   a_peer : Peer.t; *)
(*   a_sent_at : float *)
(* } *)

(* type pending_piece = { *)
(*   p_index : int; *)
(*   mutable p_reqs : int; *)
(*   p_salt : int; *)
(*   mutable p_complete : bool *)
(* } *)

(* type t = { *)
(*   meta : Metadata.t; *)
(*   tor : Torrent.t; *)
(*   rarity : int array; *)
(*   pending : pending_piece array; *)
(*   mutable active : active_block list *)
(* } *)

(* let compare_by_weight r p1 p2 = *)
(*   let w p = *)
(*     let m = Torrent.missing_blocks_in_piece r.tor p.p_index in *)
(*     let reqs = p.p_reqs in *)
(*     let a = if m > reqs then m - reqs else Metadata.block_count r.meta p.p_index + reqs in *)
(*     (a, r.rarity.(p.p_index)) *)
(*   in *)
(*   match p1.p_complete, p2.p_complete with *)
(*   | true, true -> 0 *)
(*   | true, false -> 1 *)
(*   | false, true -> -1 *)
(*   | false, false -> *)
(*     let (a1, h1) = w p1 in *)
(*     let (a2, h2) = w p2 in *)
(*     if a2 < a1 then 1 else *)
(*     if a1 < a2 then -1 else *)
(*     if h1 < h2 then -1 else *)
(*     if h2 < h1 then 1 else *)
(*     if p1.p_salt < p2.p_salt then 1 else *)
(*     if p2.p_salt < p1.p_salt then -1 *)
(*     else 0 *)

(* let get_next_requests t peer n = *)
(*   Array.sort (compare_by_weight t) t.pending; *)
(*   let rec loop acc i = *)
(*     if i >= Array.length t.pending || List.length acc >= n then List.rev acc *)
(*     else *)
(*       let p = t.pending.(i) in *)
(*       if p.p_complete then List.rev acc *)
(*       else *)
(*       if not (Peer.has_piece peer p.p_index) then loop acc (i+1) *)
(*       else *)
(*         let rec loop' acc j = *)
(*           if List.length acc >= n then List.rev acc else *)
(*           if j >= Metadata.block_count t.meta p.p_index then loop acc (i+1) else *)
(*           if List.exists (fun r -> r.a_piece = p.p_index && r.a_block = j) t.active then loop' acc (j+1) else *)
(*           if not (Torrent.has_block t.tor p.p_index j) then loop' ((p, j) :: acc) (j+1) *)
(*           else loop' acc (j+1) *)
(*         in *)
(*         loop' acc 0 *)
(*   in *)
(*   let reqs = loop [] 0 in *)
(*   List.iter (fun (p, j) -> *)
(*       t.active <- {a_piece = p.p_index; a_block = j; a_peer = peer; a_sent_at = Unix.time ()} :: t.active; *)
(*       p.p_reqs <- p.p_reqs + 1) reqs; *)
(*   List.map (fun (p, j) -> p.p_index, j) reqs *)

(* let lookup t i = *)
(*   let rec loop j = *)
(*     if j >= Array.length t.pending then raise Not_found *)
(*     else if t.pending.(j).p_index = i then t.pending.(j) else loop (j+1) *)
(*   in *)
(*   loop 0 *)

(* let decrease_request_count t i = *)
(*   let p = lookup t i in *)
(*   p.p_reqs <- p.p_reqs - 1 *)

(* let request_ttl_secs = 90 *)

(* let refill_upkeep_period_msec = 10.0 *)

(* let rec upkeep_pulse t = *)
(*   let now = Unix.time () in *)
(*   let too_old = now -. float request_ttl_secs in *)
(*   let old, keep = List.partition (fun r -> r.a_sent_at <= too_old) t.active in *)
(*   t.active <- keep; *)
(*   List.iter (fun r -> *)
(*       (\* debug "cancelling %d(%d/%d) because request is too old" *\) *)
(*         (\* r.a_piece r.a_block (Metadata.block_count t.meta r.a_piece); *\) *)
(*       Peer.send_cancel r.a_peer (r.a_piece, r.a_block); *)
(*       decrease_request_count t r.a_piece) old; *)
(*   Lwt_unix.sleep refill_upkeep_period_msec >>= fun () -> *)
(*   upkeep_pulse t *)

(* let peer_declined_all_requests tor peer = *)
(*   List.iter (fun r -> if r.a_peer == peer then decrease_request_count tor r.a_piece) tor.active; *)
(*   tor.active <- List.filter (fun r -> r.a_peer != peer) tor.active *)

(* let got_block tor peer i b = *)
(*   List.iter begin fun r -> *)
(*     if r.a_piece = i && r.a_block = b then begin *)
(*       decrease_request_count tor i; *)
(*       if r.a_peer != peer then Peer.send_cancel r.a_peer (i, b) *)
(*     end *)
(*   end tor.active; *)
(*   tor.active <- List.filter (fun r -> r.a_piece <> i || r.a_block <> b) tor.active *)

(* let got_have self piece = *)
(*   self.rarity.(piece) <- self.rarity.(piece) + 1 *)

(* let lost_have self piece = *)
(*   self.rarity.(piece) <- self.rarity.(piece) - 1 *)

(* let got_piece r i = *)
(*   let p = lookup r i in *)
(*   p.p_complete <- true *)

(* let got_bad_piece r i = *)
(*   (\* FIXME *\) *)
(*   () *)

(* let create m t = *)
(*   let numpieces = Metadata.piece_count m in *)
(*   let create_pending_piece i = *)
(*     { p_index = i; p_reqs = 0; p_salt = Random.bits (); p_complete = Torrent.has_piece t i } *)
(*   in *)
(*   let r = *)
(*     { meta = m; tor = t; *)
(*       pending = Array.init numpieces create_pending_piece; active = []; *)
(*       rarity = Array.create numpieces 0 } *)
(*   in *)
(*   Lwt.async (fun () -> upkeep_pulse r); *)
(*   r *)

(* Requester END *)

(* Torrent BEGIN *)

module Torrent = struct

  type t =
    { meta : Metadata.t;
      store : Store.t;
      completed : Bits.t array;
      mutable amount_left : int64;
      handle : event -> unit }

  let create meta handle =
    let numpieces = Metadata.piece_count meta in
    Store.create (Metadata.files meta) >>= fun store ->
    let dl =
      { meta; store;
        completed = Array.init numpieces (fun i -> Bits.create @@ Metadata.block_count meta i);
        amount_left = Metadata.total_length meta;
        handle }
    in
    let plen = Metadata.piece_length dl.meta in
    let rec loop good acc i =
      if i >= numpieces then
        Lwt.return (good, acc)
      else
        let off = Metadata.offset dl.meta i 0 in
        Store.digest dl.store off (plen i) >>= fun digest ->
        if SHA1.equal digest (Metadata.hash dl.meta i) then begin
          Bits.set_all dl.completed.(i);
          loop (good + 1) Int64.(sub acc (of_int (plen i))) (i + 1)
        end else
          loop good acc (i+1)
    in
    (* let start_time = Unix.gettimeofday () in *)
    loop 0 (Metadata.total_length dl.meta) 0 >>= fun (good, amount_left) ->
    (* let end_time = Unix.gettimeofday () in *)
    (* debug "loaded, %d/%d good pieces, %Ld bytes left, in %.0fs" *)
    (* good numpieces amount_left (end_time -. start_time); *)
    dl.amount_left <- amount_left;
    Lwt.return dl

  let get_block t i off len =
    Store.read t.store (Metadata.offset t.meta i off) len

  let is_complete self =
    let rec loop i =
      (i >= Array.length self.completed) || (Bits.has_all self.completed.(i) && loop (i + 1))
    in
    loop 0

  let got_block t i off s =
    (* t.down <- Int64.add t.down (Int64.of_int (String.length s)); *)
    let b = off / block_size in
    match Bits.is_set t.completed.(i) b with
    | false ->
        Bits.set t.completed.(i) b;
        (* debug "got block %d:%d (%d remaining)" idx b *)
        (* (Bits.length t.completed.(idx) - Bits.count t.completed.(idx)); *)
        Store.write t.store (Metadata.offset t.meta i off) s >>= fun () ->
        if Bits.has_all t.completed.(i) then begin
          Store.read t.store (Metadata.offset t.meta i off)
            (Metadata.piece_length t.meta i) >>= fun s ->
          if SHA1.(equal (digest s) @@ Metadata.hash t.meta i) then begin
            t.amount_left <- Int64.(sub t.amount_left (of_int (Metadata.piece_length t.meta i)));
            t.handle (PieceVerified i);
            if is_complete t then
              Lwt.wrap1 t.handle TorrentComplete
            else
              Lwt.return_unit
          end
          else begin
            Bits.clear t.completed.(i);
            Lwt.wrap1 t.handle @@ PieceFailed i
          end
        end
        else
          Lwt.return_unit
    | ture ->
        (* debug "received a block we already have" *)
        Lwt.return_unit

  let got_block t i off s =
    ignore
      (Lwt.catch (fun () -> got_block t i off s)
         (fun _ -> (* log warning *) Lwt.return_unit))

  let amount_left self =
    self.amount_left

  let numgot self =
    Array.fold_left (fun n b -> if Bits.has_all b then n+1 else n) 0 self.completed

  let have self =
    let have = Bits.create (Array.length self.completed) in
    Array.iteri (fun i b -> if Bits.has_all b then Bits.set have i) self.completed;
    have

  let has_piece self i =
    Bits.has_all self.completed.(i)

  let has_block t i j _ =
    Bits.is_set t.completed.(i) (j / block_size)

  let missing_blocks_in_piece t i =
    Bits.count_zeroes t.completed.(i)

  let have_size t =
    let n = ref 0L in
    for i = 0 to Array.length t.completed - 1 do
      if Bits.has_all t.completed.(i) then
        n := Int64.add !n (Int64.of_int (Metadata.piece_length t.meta i))
    done;
    !n

end
(* Torrent END *)

type piece =
  | Pending of int
  | Active of int array
  | Finished

let request_block parts = function
  | false ->
      let rec loop i =
        if i >= Array.length parts then
          None
        else
        if parts.(i) = 0 then
          (parts.(i) <- 1; Some i)
        else
          loop (i + 1)
      in
      loop 0
  | true ->
      let rec loop min index i =
        if i >= Array.length parts then
          if index >= 0 then
            (parts.(index) <- parts.(index) + 1; Some index)
          else
            None
        else
        if parts.(i) >= 0 && (parts.(i) < min || min < 0) then
          loop parts.(i) i (i + 1)
        else
          loop min index (i + 1)
      in
      loop (-1) (-1) 0

let request_endgame pieces has =
  let rec loop i =
    if i >= Array.length pieces then
      None
    else
      match pieces.(i) with
      | Finished
      | Pending _ ->
          loop (i + 1)
      | Active parts ->
          if has i then
            match request_block parts true with
            | Some j ->
                Some (i, j)
            | None ->
                None
          else
            loop (i + 1)
  in
  loop 0

let request_pending pieces has start finish =
  if start < 0 || start > finish || finish > Array.length pieces then
    invalid_arg "request_pending";
  let rec loop i =
    if i >= finish then
      None
    else
      match pieces.(i) with
      | Pending length ->
          let nparts = (length + block_size - 1) / block_size in
          let parts = Array.make nparts 0 in
          pieces.(i) <- Active parts;
          begin match request_block parts false with
          | None ->
              assert false
          | Some j ->
              Some (i, j)
          end
      | Active _
      | Finished ->
          loop (i + 1)
  in
  loop start

let request_pending pieces has =
  let n = Random.int (Array.length pieces + 1) in
  match request_pending pieces has n (Array.length pieces) with
  | None ->
      begin match request_pending pieces has 0 n with
      | None ->
          request_endgame pieces has
      | Some _ as r ->
          r
      end
  | Some _ as r ->
      r

let request_active pieces has =
  let rec loop i =
    if i >= Array.length pieces then
      request_pending pieces has
    else
      match pieces.(i) with
      | Finished
      | Pending _ ->
          loop (i + 1)
      | Active parts ->
          if has i then
            match request_block parts false with
            | None ->
                loop (i + 1)
            | Some j ->
                Some (i, j)
          else
            loop (i + 1)
  in
  loop 0

let request p pieces =
  if Peer.requests p < max_requests then
    match request_active pieces (Peer.has p) with
    | Some (i, j) ->
        ()
    | None ->
        ()

let update_requests pieces peers =
  Hashtbl.iter (fun _ p -> request p pieces) peers

let send_block store i off len u =
  let off = Int64.(add (mul (of_int i) (of_int block_size)) (of_int off)) in
  Lwt.ignore_result (Store.read store off len >>= Lwt.wrap2 Lwt.wakeup u)

let share_torrent bt meta store pieces peers =
  let peer id f = try let p = Hashtbl.find peers id in f p with Not_found -> () in
  (* Hashtbl.iter (fun _ p -> Requester.got_bitfield r (Peer.has p)) peers; *)
  let rec loop () =
    Lwt_stream.next bt.chan >>= function
    | PeersReceived addrs ->
        (* debug "received %d peers" (List.length addrs); *)
        List.iter (fun addr -> connect_to_peer bt.ih bt.push (PeerMgr.add bt.peer_mgr addr)) addrs;
        loop ()

    | PieceVerified i ->
        (* debug "piece %d verified and written to disk" i; *)
        (* PeerMgr.got_piece bt.peer_mgr i; *)
        (* Requester.got_piece r i; *)
        loop ()

    | PieceFailed i ->
        (* Announcer.add_bytes *)
        (* debug "piece %d failed hashcheck" i; *)
        (* Requester.got_bad_piece r i; *)
        (* PeerMgr.got_bad_piece bt.peer_mgr i; *)
        loop ()

    | ConnectFailed addr ->
        connect_to_peer bt.ih bt.push (PeerMgr.handshake_failed bt.peer_mgr addr);
        loop ()

    | PeerEvent (id, Peer.PeerDisconnected) ->
        connect_to_peer bt.ih bt.push (PeerMgr.peer_disconnected bt.peer_mgr id);
        Hashtbl.remove peers id;
        (* if not (am_choking peers id) && peer_interested peers id then Choker.rechoke ch; *)
        (* Requester.peer_declined_all_requests r id; FIXME FIXME *)
        (* Requester.lost_bitfield r (Peer.have p); FIXME FIXME *)
        loop ()

    | PeerEvent (_, Peer.Choked) ->
        (* Requester.peer_declined_all_requests r p; FIXME FIXME *)
        loop ()

    | PeerEvent (_, Peer.Unchoked)
    | PeerEvent (_, Peer.Have _)
    | PeerEvent (_, Peer.HaveBitfield _) ->
        update_requests pieces peers;
        loop ()

    | PeerEvent (_, Peer.Interested)
    | PeerEvent (_, Peer.NotInterested) ->
        (* if not (am_choking peers id) then Choker.rechoke ch; *)
        loop ()

    | PeerEvent (id, Peer.MetaRequested i) ->
        peer id
          (Peer.metadata_piece (Metadata.length meta) i (Metadata.get_piece meta i));
        loop ()

    | PeerEvent (_, Peer.GotMetaPiece _)
    | PeerEvent (_, Peer.RejectMetaPiece _) ->
        loop ()

    | PeerEvent (_, Peer.BlockRequested (i, off, len, u)) ->
        if pieces.(i) = Finished then send_block store i off len u;
        loop ()

    | PeerEvent (_, Peer.BlockReceived (i, off, s)) ->
        (* FIXME *)
        (* debug "got block %d/%d (piece %d) from %s" b *)
        (*   (Metadata.block_count meta idx) idx (Peer.to_string p); *)
        (* (Util.string_of_file_size (Int64.of_float (Peer.download_rate p))); *)
        (* Requester.got_block r p idx b; *)
        (* Torrent.got_block t p idx b s; *)
        (* FIXME FIXME *)
        loop ()

    | PeerEvent (_, Peer.GotPEX (added, dropped)) ->
        (* debug "got pex from %s added %d dropped %d" (Peer.to_string p) *)
        (*   (List.length added) (List.length dropped); *)
        List.iter (fun (addr, _) -> connect_to_peer bt.ih bt.push (PeerMgr.add bt.peer_mgr addr)) added;
        loop ()

    | PeerEvent (_, Peer.DHTPort _) ->
        (* debug "got dht port %d from %s" i (Peer.to_string p); *)
        (* let addr, _ = Peer.addr p in *)
        (* Lwt.async begin fun () -> *)
        (*   DHT.ping bt.dht (addr, i) >|= function *)
        (*   | Some (id, addr) -> *)
        (*       DHT.update bt.dht Kademlia.Good id addr *)
        (*   | None -> *)
        (*       debug "%s did not reply to dht ping on port %d" (Peer.to_string p) i *)
        (* end; *)
        (* FIXME *)
        loop ()

    | PeerConnected (mode, sock, exts, id) ->
        let p = welcome bt.push mode sock exts id in
        (* let p = Peer.create_has_meta id meta (\*  (Requester.get_next_requests r) *\) in *)
        (* Lwt.async (fun () -> reader_loop bt.push sock p); *)
        (* if Bits.is_set exts Wire.ltep_bit then Peer.send_extended_handshake p; *)
        (* if Bits.is_set exts Wire.dht_bit then Peer.send_port p 6881; (\* FIXME fixed port *\) *)
        (* (\* Peer.send_have_bitfield p (Torrent.have tor) *\) *)
        (* (\* FIXME *\) *)
        Hashtbl.replace peers id p;
        loop ()

    | PeerEvent (_, Peer.AvailableMetadata _)
    | NoEvent ->
        loop ()
  in
  loop ()

let load_torrent bt meta =
  Torrent.create meta bt.push (* >|= fun dl -> *)
(* bt.push (TorrentLoaded dl) *)

(* IncompleteMetadata BEGIN *)

module IncompleteMetadata = struct
  type t =
    { info_hash : SHA1.t;
      length : int;
      pieces : Bits.t;
      raw : Cstruct.t }

  let metadata_block_size = 1 lsl 14
  let metadata_max_size = 1 lsl 22

  let create ~info_hash ~length =
    let size = (length + metadata_block_size - 1) / metadata_block_size in
    { info_hash; length; pieces = Bits.create size; raw = Cstruct.create length }

  let add m n buf =
    if n < 0 || n >= Bits.length m.pieces then invalid_arg "add";
    Bits.set m.pieces n;
    Cstruct.blit buf 0 m.raw (n * metadata_block_size) (Cstruct.len buf);
    match Bits.has_all m.pieces with
    | true ->
        if SHA1.(equal (digest m.raw) m.info_hash) then
          `Verified m.raw
        else
          `Failed
    | false ->
        `More

  let iter_missing f m =
    for i = 0 to Bits.length m.pieces - 1 do
      if not (Bits.is_set m.pieces i) then
        f i
    done
end

(* IncompleteMetadata END *)

let rec fetch_metadata bt =
  let peers = Hashtbl.create 20 in
  let peer id f = try let p = Hashtbl.find peers id in f p with Not_found -> () in
  let rec loop m =
    Lwt_stream.next bt.chan >>= fun e ->
    match m, e with
    | None, PeerEvent (id, Peer.AvailableMetadata len) ->
        (* debug "%s offered %d bytes of metadata" (Peer.to_string p) len; *)
        (* FIXME *)
        let m' = IncompleteMetadata.create bt.ih len in
        peer id (fun p -> IncompleteMetadata.iter_missing (Peer.request_metadata_piece p) m');
        loop (Some m')

    | Some m', PeerEvent (id, Peer.AvailableMetadata len) ->
        peer id (fun p -> IncompleteMetadata.iter_missing (Peer.request_metadata_piece p) m');
        loop m

    | _, PeersReceived addrs ->
        (* debug "received %d peers" (List.length addrs); *)
        List.iter (fun addr -> connect_to_peer bt.ih bt.push (PeerMgr.add bt.peer_mgr addr)) addrs;
        loop m

    | _, PeerConnected (mode, fd, exts, id) ->
        let p = welcome bt.push mode fd exts id in
        Hashtbl.replace peers id p;
        loop m

    | _, PeerEvent (id, Peer.PeerDisconnected) ->
        connect_to_peer bt.ih bt.push (PeerMgr.peer_disconnected bt.peer_mgr id);
        Hashtbl.remove peers id;
        loop m

    | _, ConnectFailed addr ->
        connect_to_peer bt.ih bt.push (PeerMgr.handshake_failed bt.peer_mgr addr);
        loop m

    | _, PeerEvent (id, Peer.MetaRequested i) ->
        peer id (fun p -> Peer.reject_metadata_request p i);
        loop m

    | Some m', PeerEvent (_, Peer.GotMetaPiece (i, s)) ->
        (* debug "got metadata piece %d/%d from %s" i *)
        (*   (IncompleteMetadata.piece_count m) (Peer.to_string p); *)
        begin match IncompleteMetadata.add m' i s with
        | `Failed ->
            (* debug "metadata hash check failed; trying again"; *)
            loop None
        | `Verified raw ->
              (* debug "got full metadata"; *)
            let m' = Metadata.create (Bcode.decode raw) in
            Lwt.return m'
        | `More ->
            loop m
        end

    | None, PeerEvent (_, Peer.GotMetaPiece _) ->
        loop m

    | _, PeerEvent (_, Peer.GotPEX (added, dropped)) ->
        (* debug "got pex from %s added %d dropped %d" (Peer.to_string p) *)
        (*   (List.length added) (List.length dropped); *)
        List.iter (fun (addr, _) -> connect_to_peer bt.ih bt.push (PeerMgr.add bt.peer_mgr addr)) added;
        loop m

    | _, PeerEvent (_, Peer.DHTPort i) ->
        (* debug "got dht port %d from %s" i (Peer.to_string p); *)
        (* let addr, _ = Peer.addr p in *)
        (* Lwt.async begin fun () -> *)
        (*   DHT.ping bt.dht (addr, i) >|= function *)
        (*   | Some (id, addr) -> *)
        (*       DHT.update bt.dht Kademlia.Good id addr *)
        (*   | None -> *)
        (*       debug "%s did not reply to dht ping on port %d" (Peer.to_string p) i *)
        (* end *)
        (* FIXME *)
        loop m

    | _, PieceVerified _
    | _, PieceFailed _
    | _, TorrentComplete
    | _, PeerEvent (_, Peer.Unchoked)
    | _, PeerEvent (_, Peer.Choked)
    | _, PeerEvent (_, Peer.Interested)
    | _, PeerEvent (_, Peer.NotInterested)
    | _, PeerEvent (_, Peer.Have _)
    | _, PeerEvent (_, Peer.HaveBitfield _)
    | _, PeerEvent (_, Peer.RejectMetaPiece _)
    | _, PeerEvent (_, Peer.BlockRequested _)
    | _, PeerEvent (_, Peer.BlockReceived _)
    | _, NoEvent ->
        loop m
  in
  loop None

module LPD  = struct

  module Log = L.Make (struct let section = "LPD" end)

  let mcast_addr = "239.192.152.143"
  let mcast_port = 6771

  open Cohttp

  module Http = Request.Make (String_io.M)

  let template =
    Printf.sprintf
      "BT-SEARCH * HTTP/1.1\r\n\
       Host: %s:%u\r\n\
       Port: %u\r\n\
       Infohash: %s\r\n\
       \r\n\r\n" mcast_addr mcast_port

  let get h name =
    match Header.get h name with
    | None -> Printf.ksprintf invalid_arg "Header not found : %S" name
    | Some s -> s

  let str = Bytes.create 200

  let start fd info_hash push =
    let rec loop () =
      Lwt_unix.recvfrom fd str 0 (Bytes.length str) [] >>= fun (n, sa) ->
      let ip, port = match sa with Unix.ADDR_INET (ip, port) -> ip, port | _ -> assert false in
      let buf = { String_io.str; pos = 0; len = n } in
      match Http.read buf with
      | `Ok r when r.Request.meth = `Other "BT-SEARCH" ->
          let ih = get r.Request.headers "Infohash" in
          let p = get r.Request.headers "Port" in
          Log.info "Received announce from = %s:%d ih = %s listen = %s"
            (Unix.string_of_inet_addr ip) port ih p;
          if SHA1.equal (SHA1.of_hex ih) info_hash then push (PeersReceived [ip, int_of_string p]);
          loop ()
      | `Ok r ->
          Log.error "Unxpected HTTP method : %S" (Code.string_of_method r.Request.meth);
          loop ()
      | `Invalid s ->
          Log.error "Invalid HTTP request : %S" s;
          loop ()
      | `Eof ->
          Log.error "Unexpected end of file in HTTP request ";
          loop ()
    in
    loop ()

  let start fd info_hash push=
    Lwt.catch
      (fun () -> start fd info_hash push)
      (fun e ->
         Lwt.wrap2 Log.error "loop exn : %S ; restarting ..." (Printexc.to_string e))

  let start fd info_hash push =
    let rec loop () = start fd info_hash push >>= loop in
    loop ()

  let announce_delay = 5. *. 60.

  let announce fd port info_hash =
    let msg = template port @@ SHA1.to_hex info_hash in
    let sa = Lwt_unix.ADDR_INET (Unix.inet_addr_of_string mcast_addr, mcast_port) in
    let rec loop () =
      Lwt_unix.sendto fd msg 0 (String.length msg) [] sa >>= fun _ ->
      Lwt_unix.sleep announce_delay >>=
      loop
    in
    loop ()

  let announce fd port info_hash =
    Lwt.catch
      (fun () -> announce fd port info_hash)
      (fun e ->
         Lwt.wrap2 Log.error "announce exn : %s ; restarting ..." (Printexc.to_string e))

  let announce fd port info_hash =
    let rec loop () = announce fd port info_hash >>= loop in
    loop ()

  let start ~port ~info_hash push =
    Log.debug "LPD.start";
    let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0 in
    let fd2 = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0 in
    Lwt_unix.setsockopt fd Lwt_unix.SO_REUSEADDR true;
    Lwt_unix.setsockopt fd2 Lwt_unix.SO_REUSEADDR true;
    Lwt_unix.mcast_set_loop fd2 false;
    Lwt_unix.bind fd (Lwt_unix.ADDR_INET (Unix.inet_addr_any, mcast_port));
    Log.debug "Joining multicast group...";
    Lwt_unix.mcast_add_membership fd (Unix.inet_addr_of_string mcast_addr);
    Lwt.ignore_result (start fd info_hash push);
    Lwt.ignore_result (announce fd2 port info_hash)

end

let start bt =
  List.iter (Tracker.announce ~info_hash:bt.ih (fun peers -> bt.push (PeersReceived peers)) bt.id) bt.trackers;
  fetch_metadata bt >>= fun meta ->
  load_torrent bt meta >>= fun tor ->
  assert false

let listen_backlog = 5

let start_server ?(port = 0) push =
  let fd = Lwt_unix.(socket PF_INET SOCK_STREAM 0) in
  Lwt_unix.bind fd (Unix.ADDR_INET (Unix.inet_addr_any, 0));
  Lwt_unix.listen fd listen_backlog;
  (* debug "listening on port %u" port; *)
  let rec loop () =
    Lwt_unix.accept fd >>= fun (fd, sa) ->
    (* debug "accepted connection from %s" (Util.string_of_sockaddr sa); *)
    push (IncomingConnection (fd, sa));
    loop ()
  in
  loop ()

let create mg =
  let chan, push = Lwt_stream.create () in
  let push x = push (Some x) in
  let id = SHA1.generate ~prefix:"OCAML" () in
  let peer_mgr = PeerMgr.create () in
  Lwt.async (fun () -> start_server push);
  { id; ih = mg.Magnet.xt; trackers = mg.Magnet.tr; chan;
    push; peer_mgr }
