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
let (>|=) = Lwt.(>|=)

let listen_ports = [50000]

type event =
  | IncomingPeer of IO.socket * Addr.t
  | PeersReceived of Addr.t list
  | GotMetadata of Metadata.t
  | TorrentLoaded of Torrent.t
  (* | PieceVerified of int *)
  (* | TorrentCompleted *)
  | Announce of Tracker.Tier.t * Tracker.event option
  | GotMetaLength of IncompleteMetadata.t
  | GotBadMeta

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
  mutable port : int
}

exception Cant_listen

(* let create_server handle = *)
(*   let sock = Tcp.create_socket () in *)
(*   let rec loop = function *)
(*     | [] -> *)
(*       raise Cant_listen *)
(*     | p :: ports -> *)
(*       try *)
(*         let stop = Tcp.listen sock p handle in *)
(*         Log.info "listening on port %d" p; *)
(*         p, stop *)
(*       with _ -> loop ports *)
(*   in *)
(*   loop listen_ports *)

let push_incoming_peer bt sock addr =
  bt.push (IncomingPeer (sock, addr))

let push_peers_received bt xs =
  bt.push (PeersReceived xs)

let push_metadata bt info =
  bt.push (GotMetadata info)

let get_next_requests bt p n =
  match bt.stage with
  | HasMeta (_, Leeching (_, _, r)) ->
    if not (Peer.peer_choking p) then Requester.get_next_requests r p n
    else []
  | HasMeta _ -> []
  | NoMeta _ -> []

let get_next_metadata_request bt p () =
  match bt.stage with
  | NoMeta (PartialMeta m) ->
    IncompleteMetadata.get_next_metadata_request m
  | _ ->
    None

let print_info bt =
  assert false
  (* match bt.stage with *)
  (* | Leeching (_, t, _) *)
  (* | Seeding (_, t, _) -> *)
  (*   let dl, ul = 0.0, 0.0 *)
  (*     (\* Hashtbl.fold *\) *)
  (*       (\* (fun _ p (dl, ul) -> (dl +. Peer.download_rate p, ul +. Peer.upload_rate p)) *\) *)
  (*       (\* bt.peers (0.0, 0.0) *\) *)
  (*   in *)
  (*   let eta = *)
  (*     let left = Torrent.amount_left t in *)
  (*     if dl = 0.0 then "Inf" *)
  (*     else *)
  (*       let eta = Int64.to_float left /. dl in *)
  (*       let tm = Unix.gmtime eta in *)
  (*       if tm.Unix.tm_mday > 1 || tm.Unix.tm_mon > 0 || tm.Unix.tm_year > 70 then "More than a day" *)
  (*       else *)
  (*         Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec *)
  (*   in *)

  (*   Printf.eprintf "Progress: %d/%d (%d%%) Peers: %d Downloaded: %s (%s/s) Uploaded: %s (%s/s) ETA: %s\n%!" *)
  (*     (Bits.count (Torrent.have t)) *)
  (*     (Bits.length (Torrent.have t)) *)
  (*     (truncate (100.0 *. (float (Bits.count (Torrent.have t))) /. (float (Bits.length (Torrent.have t))))) *)
  (*     (\* (Hashtbl.length bt.peers) *\) 0 *)
  (*     (Util.string_of_file_size (Torrent.down t)) *)
  (*     (Util.string_of_file_size (Int64.of_float dl)) *)
  (*     (Util.string_of_file_size (Torrent.up t)) *)
  (*     (Util.string_of_file_size (Int64.of_float ul)) *)
  (*     eta *)
  (* | _ -> *)
(*   () *)

let handle_peer_event bt p e =
  match e with
  | Peer.Finished ->
    PeerMgr.peer_finished bt.peer_mgr p;
    begin match bt.stage with
    | HasMeta (_, Leeching (_, _, r)) ->
      Requester.peer_declined_all_requests r p;
      Requester.lost_bitfield r (Peer.have p)
    | _ -> ()
    end
  | Peer.AvailableMetadata len ->
    Log.success "metadata available len:%d" len;
    begin match bt.stage with
    | NoMeta NoMetaLength ->
      bt.push (GotMetaLength (IncompleteMetadata.create bt.ih len))
    | _ ->
      ()
    end
  | Peer.Choked ->
    begin match bt.stage with
    | HasMeta (_, Leeching (_, _, r)) ->
      Requester.peer_declined_all_requests r p
    | _ ->
      ()
    end
  | Peer.Have i ->
    begin match bt.stage with
    | HasMeta (_, Leeching (_, _, r)) ->
      Requester.got_have r i
    | _ -> ()
    end
  | Peer.HaveBitfield b ->
    begin match bt.stage with
    | HasMeta (_, Leeching (_, _, r)) ->
      Requester.got_bitfield r b
    | _ -> ()
    end
  | Peer.MetaRequested i ->
    begin match bt.stage with
    | NoMeta _ ->
      Peer.send_reject_meta p i
    | HasMeta (meta, _) ->
      Peer.send_meta_piece p i (Metadata.length meta, Metadata.get_piece meta i)
    end
  | Peer.GotMetaPiece (i, s) ->
    begin match bt.stage with
    | NoMeta (PartialMeta meta) ->
      Log.success "got meta piece (i=%d)" i;
      if IncompleteMetadata.add_piece meta i s then begin
        match IncompleteMetadata.verify meta with
        | Some meta ->
          Log.success "got complete metadata";
          push_metadata bt (Metadata.create (Bcode.decode meta))
        | None ->
          Log.error "metadata hash check failed";
          bt.push GotBadMeta
      end
    | _ ->
      ()
    end
  | Peer.RejectMetaPiece i ->
    Log.warning "meta piece rejected (i=%d)" i
  | Peer.BlockRequested (idx, off, len) ->
    begin match bt.stage with
    | HasMeta (_, Leeching (dl, _, _))
    | HasMeta (_, Seeding (dl, _)) ->
      if Torrent.has_piece dl idx then
        let aux _ = Torrent.get_block dl idx off len >|= Peer.send_block p idx off in
        Lwt.async aux
    | _ ->
      ()
    end
  | Peer.BlockReceived (idx, b, s) ->
    begin match bt.stage with
    | HasMeta (meta, Leeching (t, _, r)) ->
      Log.success "received block (idx=%d,blk=%d,len=%d) from %s (%s/s)"
        idx b (String.length s) (Addr.to_string (Peer.addr p))
        (Util.string_of_file_size (Int64.of_float (Peer.download_rate p)));
      (* let b = Metadata.block_number meta off in *)
      Requester.got_block r p idx b;
      Torrent.got_block t p idx b s
      (* let aux () = *)
      (*   Torrent.got_block t p idx off s >|= function *)
      (*   | `Verified -> *)
      (*     Requester.got_piece r idx; *)
      (*     (\* Torrent.got_piece r idx *\) *)
      (*     bt.push (PieceVerified idx); *)
      (*     if Torrent.is_complete t then bt.push TorrentCompleted *)
      (*   | `Failed *)
      (*   | `Continue -> () *)
      (* in *)
      (* Lwt.async aux *)
    | _ ->
      ()
    end
  | Peer.Port _ ->
    ()

let handle_torrent_event bt = function
  | Torrent.PieceVerified i ->
    Log.success "piece %d verified and written to disk" i;
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
      Log.error "piece %d failed hashcheck" i;
      Requester.got_bad_piece r i;
      PeerMgr.got_bad_piece bt.peer_mgr i
    | _ ->
      ()
    end
  | Torrent.TorrentComplete ->
    Log.success "torrent completed!";
    begin match bt.stage with
    | HasMeta (meta, Leeching (t, ch, _)) ->
      (* FIXME stop requester ? *)
      bt.stage <- HasMeta (meta, Seeding (t, ch))
    | _ ->
      ()
    end

let (!!) = Lazy.force

let handle_event bt = function
  | GotMetaLength m ->
    bt.stage <- NoMeta (PartialMeta m)
  | GotBadMeta ->
    bt.stage <- NoMeta NoMetaLength
  | IncomingPeer (sock, addr) ->
    PeerMgr.handle_incoming_peer bt.peer_mgr sock addr
  | PeersReceived addrs ->
    Log.success "received %d peers" (List.length addrs);
    List.iter (PeerMgr.handle_received_peer bt.peer_mgr) addrs
  | GotMetadata meta ->
    begin match bt.stage with
    | NoMeta _ ->
      bt.stage <- HasMeta (meta, Loading);
      PeerMgr.got_metadata bt.peer_mgr meta (get_next_requests bt);
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
      Log.success "torrent loaded (good=%d,total=%d)"
        (Torrent.numgot dl) (Metadata.piece_count meta - Torrent.numgot dl);
      let ch = Choker.create bt.peer_mgr dl in
      if Torrent.is_complete dl then
        bt.stage <- HasMeta (meta, Seeding (dl, ch))
      else begin
        let r = Requester.create meta dl in
        bt.stage <- HasMeta (meta, Leeching (dl, ch, r));
        PeerMgr.iter_peers (fun p -> Requester.got_bitfield r (Peer.have p)) bt.peer_mgr
      end;
      Choker.start ch;
      let wakeup_peer p =
        Peer.send_have_bitfield p (Torrent.have dl)
      in
      PeerMgr.iter_peers wakeup_peer bt.peer_mgr
    | _ ->
      ()
    end
  | Announce (tier, event) ->
    let doit () =
      Tracker.Tier.query tier ~ih:bt.ih ?up:None ?down:None ?left:None ?event ~port:bt.port ~id:bt.id >>= fun resp ->
      Log.success "announce on %s successful, reannouncing in %d seconds"
        (Tracker.Tier.show tier) resp.Tracker.interval;
      push_peers_received bt resp.Tracker.peers;
      Lwt_unix.sleep (float resp.Tracker.interval) >|= fun () ->
      bt.push (Announce (tier, None))
    in
    let safe_doit () =
      Lwt.catch doit (fun e -> Log.error ~exn:e "announce failure"; Lwt.return ())
    in
    Lwt.async safe_doit

let rec event_loop bt =
  Lwt_stream.next bt.chan >|= handle_event bt >>= fun () -> event_loop bt

let start bt =
  (* let port, _ = create_server (push_incoming_peer bt) in *)
  let port = 5000 in
  bt.port <- port;
  Log.info "starting";
  List.iter (fun tier -> bt.push (Announce (tier, Some Tracker.STARTED))) bt.trackers;
  PeerMgr.start bt.peer_mgr;
  event_loop bt

let create mg =
  let chan, push = Lwt_stream.create () in
  let push x = push (Some x) in
  let trackers =
    List.map (fun tr ->
        let tier = Tracker.Tier.create () in
        Tracker.Tier.add_tracker tier tr;
        tier) mg.Magnet.tr
  in
  let id = SHA1.peer_id "OCTO" in
  let ih = mg.Magnet.xt in
  let rec cl = lazy
    { id; ih; trackers; chan;
      push; peer_mgr = PeerMgr.create_no_meta id ih
                (fun p e -> handle_peer_event !!cl p e)
                (fun p () -> get_next_metadata_request !!cl p ());
      stage = NoMeta NoMetaLength;
      port = -1 }
  in
  !!cl
