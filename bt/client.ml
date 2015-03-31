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

let section = Log.make_section "Client"

let debug ?exn fmt = Log.debug section ?exn fmt

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let listen_ports = [50000]

type event =
  | PeersReceived of Addr.t list
  | GotMetadata of Metadata.t
  | TorrentLoaded of Torrent.t
  | Announce of Tracker.Tier.t * Tracker.event option
  | GotMetaLength of IncompleteMetadata.t
  | GotBadMeta
  | PeerEvent of Peer.event
  | PeerJoined of IO.t * SHA1.t * Bits.t

type no_meta_stage =
  | NoMetaLength
  | PartialMeta of IncompleteMetadata.t

type has_meta_stage =
  | Loading
  | Leeching of Torrent.t * Choker.t * Requester.t
  | Seeding of Torrent.t * Choker.t

type stage =
  | NoMeta of no_meta_stage
  | HasMeta of Metadata.t * has_meta_stage

type t = {
  id : SHA1.t;
  ih : SHA1.t;
  mutable trackers : Tracker.Tier.t list;
  peer_mgr : PeerMgr.t;
  chan : event Lwt_stream.t;
  push : event -> unit;
  mutable stage : stage;
  listener : Listener.t;
  dht : DHT.t
}

let push_peers_received bt xs =
  bt.push (PeersReceived xs)

let push_metadata bt info =
  bt.push (GotMetadata info)

let get_next_requests bt p n =
  match bt.stage with
  | HasMeta (_, Leeching (_, _, r)) ->
      (* if not (Peer.peer_choking p) then *)Requester.get_next_requests r p n
  (* else [] *)
  | HasMeta _ -> []
  | NoMeta _ -> []

let get_next_metadata_request bt p =
  match bt.stage with
  | NoMeta (PartialMeta m) ->
      IncompleteMetadata.get_next_metadata_request m
  | _ ->
      None

let handle_peer_event bt e =
  match e with
  | Peer.Finished p ->
      PeerMgr.peer_finished bt.peer_mgr p;
      begin match bt.stage with
      | HasMeta (_, Leeching (_, ch, r)) ->
          Requester.peer_declined_all_requests r p;
          Requester.lost_bitfield r (Peer.have p);
          if not (Peer.am_choking p) && Peer.peer_interested p then Choker.rechoke ch
      | HasMeta (_, Seeding (_, ch)) ->
          if not (Peer.am_choking p) && Peer.peer_interested p then Choker.rechoke ch
      | _ -> ()
      end
  | Peer.AvailableMetadata (p, len) ->
      debug "%s offered %d bytes of metadata" (Peer.to_string p) len;
      begin match bt.stage with
      | NoMeta NoMetaLength ->
          bt.push (GotMetaLength (IncompleteMetadata.create bt.ih len))
      | _ ->
          ()
      end
  | Peer.Choked p ->
      begin match bt.stage with
      | HasMeta (_, Leeching (_, _, r)) ->
          Requester.peer_declined_all_requests r p
      | _ ->
          ()
      end
  | Peer.Interested p
  | Peer.NotInterested p ->
      begin match bt.stage with
      | HasMeta (_, Leeching (_, ch, _))
      | HasMeta (_, Seeding (_, ch)) ->
          if not (Peer.am_choking p) then Choker.rechoke ch
      | _ ->
          ()
      end
  | Peer.Have (p, i) ->
      begin match bt.stage with
      | HasMeta (_, Leeching (_, _, r)) ->
          Requester.got_have r i
      | _ -> ()
      end
  | Peer.HaveBitfield (p, b) ->
      begin match bt.stage with
      | HasMeta (_, Leeching (_, _, r)) ->
          Requester.got_bitfield r b
      | _ -> ()
      end
  | Peer.MetaRequested (p, i) ->
      begin match bt.stage with
      | NoMeta _ ->
          Peer.send_reject_meta p i
      | HasMeta (meta, _) ->
          Peer.send_meta_piece p i (Metadata.length meta, Metadata.get_piece meta i)
      end
  | Peer.GotMetaPiece (p, i, s) ->
      begin match bt.stage with
      | NoMeta (PartialMeta meta) ->
          debug "got metadata piece %d/%d from %s" i
            (IncompleteMetadata.piece_count meta) (Peer.to_string p);
          if IncompleteMetadata.add_piece meta i s then begin
            match IncompleteMetadata.verify meta with
            | Some meta ->
                debug "got full metadata";
                push_metadata bt (Metadata.create (Bcode.decode meta))
            | None ->
                debug "metadata hash check failed; trying again";
                bt.push GotBadMeta
          end
      | _ ->
          ()
      end
  | Peer.RejectMetaPiece (p, i) ->
      debug "%s rejected request for metadata piece %d" (Peer.to_string p) i
  | Peer.BlockRequested (p, idx, b) ->
      begin match bt.stage with
      | HasMeta (_, Leeching (dl, _, _))
      | HasMeta (_, Seeding (dl, _)) ->
          if Torrent.has_piece dl idx then
            let aux _ = Torrent.get_block dl idx b >|= Peer.send_block p idx b in
            Lwt.async aux
      | _ ->
          ()
      end
  | Peer.BlockReceived (p, idx, b, s) ->
      begin match bt.stage with
      | HasMeta (meta, Leeching (t, _, r)) ->
          debug "got block %d/%d (piece %d) from %s" b
            (Metadata.block_count meta idx) idx (Peer.to_string p);
          (* (Util.string_of_file_size (Int64.of_float (Peer.download_rate p))); *)
          Requester.got_block r p idx b;
          Torrent.got_block t p idx b s
      | _ ->
          ()
      end
  | Peer.GotPEX (p, added, dropped) ->
      debug "got pex from %s added %d dropped %d" (Peer.to_string p)
        (List.length added) (List.length dropped);
      List.iter (fun (a, _) -> PeerMgr.handle_received_peer bt.peer_mgr a) added
  | Peer.DHTPort (p, i) ->
      debug "got dht port %d from %s" i (Peer.to_string p);
      let addr, _ = Peer.addr p in
      Lwt.async begin fun () ->
        DHT.ping bt.dht (addr, i) >|= function
        | Some (id, addr) ->
            DHT.update bt.dht Kademlia.Good id addr
        | None ->
            debug "%s did not reply to dht ping on port %d" (Peer.to_string p) i
      end

let reader_loop bt p =
  let ic = IO.in_channel (Peer.sock p) in
  let input = Lwt_stream.from
      (fun () -> Wire.read ic >>= fun msg ->
        (* debug "got message from %s : %s" (string_of_node p.node) (Wire.string_of_message msg); *)
        Lwt.return (Some msg))
  in
  let rec loop f =
    Lwt.pick
      [(Lwt_stream.next input >|= fun x -> `Ok x);
       (* (Lwt_condition.wait p.on_stop >|= fun () -> `Stop); *)
       (Lwt_unix.sleep (float Peer.keepalive_delay) >|= fun () -> `Timeout)]
    >>= function
    | `Ok x ->
        begin match f x with
        | None ->
            loop f
        | Some e ->
            handle_peer_event bt e;
            loop f
        end
        (* let () = f x in loop f *)
    (* | `Stop -> Lwt.return () *)
    | `Timeout -> Lwt.fail Peer.Timeout
  in
  loop (Peer.got_message p)

let handle_torrent_event bt = function
  | Torrent.PieceVerified i ->
      debug "piece %d verified and written to disk" i;
      begin match bt.stage with
      | HasMeta (_, Leeching (_, _, r)) ->
          PeerMgr.got_piece bt.peer_mgr i;
          Requester.got_piece r i
      | _ ->
          ()
      end
  | Torrent.PieceFailed i ->
      begin match bt.stage with
      | HasMeta (_, Leeching (_, _, r)) ->
          (* Announcer.add_bytes *)
          debug "piece %d failed hashcheck" i;
          Requester.got_bad_piece r i;
          PeerMgr.got_bad_piece bt.peer_mgr i
      | _ ->
          ()
      end
  | Torrent.TorrentComplete ->
      debug "torrent completed!";
      begin match bt.stage with
      | HasMeta (meta, Leeching (t, ch, _)) ->
          (* FIXME stop requester ? *)
          bt.stage <- HasMeta (meta, Seeding (t, ch))
      | _ ->
          ()
      end

let handle_event bt = function
  | GotMetaLength m ->
      bt.stage <- NoMeta (PartialMeta m)
  | GotBadMeta ->
      bt.stage <- NoMeta NoMetaLength
  (* | IncomingPeer (sock, addr) -> *)
  (*   PeerMgr.handle_incoming_peer bt.peer_mgr sock addr *)
  | PeersReceived addrs ->
      debug "received %d peers" (List.length addrs);
      List.iter (PeerMgr.handle_received_peer bt.peer_mgr) addrs
  | GotMetadata meta ->
      begin match bt.stage with
      | NoMeta _ ->
          bt.stage <- HasMeta (meta, Loading);
          let aux () =
            Torrent.create meta (handle_torrent_event bt) >|= fun dl ->
            bt.push (TorrentLoaded dl)
          in
          Lwt.async aux
      | HasMeta _ ->
          ()
      end
  | TorrentLoaded dl ->
      begin match bt.stage with
      | HasMeta (meta, Loading) ->
          (* debug "torrent loaded (good=%d,total=%d)" *)
          (*   (Torrent.numgot dl) (Metadata.piece_count meta - Torrent.numgot dl); *)
          (* PeerMgr.torrent_loaded bt.peer_mgr meta dl (get_next_requests bt); FIXME FIXME *)
          let ch = Choker.create bt.peer_mgr dl in
          if Torrent.is_complete dl then
            bt.stage <- HasMeta (meta, Seeding (dl, ch))
          else begin
            let r = Requester.create meta dl in
            bt.stage <- HasMeta (meta, Leeching (dl, ch, r));
            PeerMgr.iter_peers (fun p -> Requester.got_bitfield r (Peer.have p)) bt.peer_mgr
          end;
          Choker.start ch
      | _ ->
          ()
      end
  | Announce (tier, event) ->
      let doit () =
        (* FIXME port *)
        Tracker.Tier.query tier ~ih:bt.ih ?up:None ?down:None ?left:None ?event ?port:(Listener.port bt.listener) ~id:bt.id >>= fun resp ->
        debug "announce to %s successful, reannouncing in %ds"
          (Tracker.Tier.to_string tier) resp.Tracker.interval;
        push_peers_received bt resp.Tracker.peers;
        Lwt_unix.sleep (float resp.Tracker.interval) >|= fun () ->
        bt.push (Announce (tier, None))
      in
      let safe_doit () =
        Lwt.catch doit (fun exn -> debug ~exn "announce failure"; Lwt.return ())
      in
      Lwt.async safe_doit
  | PeerEvent e ->
      handle_peer_event bt e
  | PeerJoined (sock, id, exts) ->
      let p = match bt.stage with
        | HasMeta (m, _) ->
            Peer.create_has_meta sock (IO.addr sock) id
              (fun e -> bt.push (PeerEvent e)) m (get_next_requests bt)
        | NoMeta r ->
            Peer.create_no_meta sock (IO.addr sock) id
              (fun e -> bt.push (PeerEvent e)) (get_next_metadata_request bt)
      in
      Lwt.async (fun () -> reader_loop bt p);
      Peer.start p;
      (* Hashtbl.add bt.peers addr !!p; FIXME XXX *)
      if Bits.is_set exts Wire.ltep_bit then Peer.send_extended_handshake p;
      if Bits.is_set exts Wire.dht_bit then Peer.send_port p 6881; (* FIXME fixed port *)
      begin match bt.stage with
      | HasMeta (_, Leeching (tor, _, _))
      | HasMeta (_, Seeding (tor, _)) -> Peer.send_have_bitfield p (Torrent.have tor)
      | HasMeta (_, Loading)
      | NoMeta _ -> ()
      end

let (!!) = Lazy.force

let rec event_loop bt =
  Lwt_stream.next bt.chan >|= handle_event bt >>= fun () -> event_loop bt

let start bt =
  List.iter (fun tier -> bt.push (Announce (tier, Some Tracker.STARTED))) bt.trackers;
  Listener.start bt.listener ();
  PeerMgr.start bt.peer_mgr;
  DHT.start bt.dht;
  Lwt.async begin fun () ->
    DHT.auto_bootstrap bt.dht DHT.bootstrap_nodes >>= fun () ->
    DHT.query_peers bt.dht bt.ih begin fun (id, addr) token peers ->
      bt.push (PeersReceived peers);
      Lwt.async begin fun () ->
        Lwt.catch
          (fun () -> DHT.announce bt.dht addr 6881 token bt.ih >>= fun _ -> Lwt.return ())
          (fun exn ->
             debug ~exn "dht announce to %s (%s) failed" (SHA1.to_hex_short id) (Addr.to_string addr);
             Lwt.return ())
      end
    end
  end;
  event_loop bt

let create mg =
  let chan, push = Lwt_stream.create () in
  let push x = push (Some x) in
  let trackers = List.map (fun tr -> Tracker.Tier.create [tr]) mg.Magnet.tr in
  let id = SHA1.peer_id "OCTO" in
  let ih = mg.Magnet.xt in
  let rec cl = lazy
    { id; ih; trackers; chan;
      push; peer_mgr = !!peer_mgr;
      stage = NoMeta NoMetaLength;
      listener = Listener.create
          (fun fd _ -> PeerMgr.handle_incoming_peer !!peer_mgr (IO.of_file_descr fd));
      dht = DHT.create 6881 }
  and peer_mgr =
    lazy (PeerMgr.create_no_meta id ih
        (fun sock id ext -> !!cl.push (PeerJoined (sock, id, ext)))
        (* (fun p e -> handle_peer_event !!cl p e) *)
        (fun p -> get_next_metadata_request !!cl p))
  in
  !!cl

let stats c =
  let downloaded = match c.stage with
    | HasMeta (_, Leeching (t, _, _))
    | HasMeta (_, Seeding (t, _)) ->
        Torrent.have_size t
    | _ ->
        0L
  in
  let total_size = match c.stage with
    | HasMeta (m, _) -> Metadata.total_length m
    | NoMeta _ -> 0L
  in
  let have_pieces = match c.stage with
    | HasMeta (_, Leeching (t, _, _))
    | HasMeta (_, Seeding (t, _)) -> Torrent.numgot t
    | _ -> 0
  in
  let total_pieces = match c.stage with
    | HasMeta (m, _) -> Metadata.piece_count m
    | NoMeta _ -> 0
  in
  let amount_left = match c.stage with
    | HasMeta (_, Leeching (t, _, _))
    | HasMeta (_, Seeding (t, _)) -> Torrent.amount_left t
    | _ -> 0L
  in
  { Stats.upload_speed = PeerMgr.upload_speed c.peer_mgr;
    download_speed = PeerMgr.download_speed c.peer_mgr;
    num_connected_peers = PeerMgr.num_connected_peers c.peer_mgr;
    num_total_peers = PeerMgr.num_total_peers c.peer_mgr;
    downloaded;
    total_size;
    have_pieces;
    total_pieces;
    amount_left }
