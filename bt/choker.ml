let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

type t = {
  peer_mgr : PeerMgr.t;
  torrent : Torrent.t
}

let create pm tor =
  { peer_mgr = pm; torrent = tor }

let max_downloaders_unchoke = 4
let unchoking_frequency = 10
let optimistic_unchoke_iterations = 3
let rate_computation_iterations = 2

let unchoke_peers bt optimistic =
  let aux compare_peers =
    let peers = PeerMgr.fold_peers (fun p l -> p :: l) bt.peer_mgr [] in
    let peers = List.sort compare_peers peers in
    let rec loop choked downloaders = function
      | [] -> choked
      | p :: peers ->
        if downloaders < max_downloaders_unchoke then
          if Peer.am_choking p then begin
            Peer.send_unchoke p;
            if Peer.peer_interested p then loop choked (downloaders + 1) peers
            else loop choked downloaders peers
          end else
            loop choked downloaders peers
        else begin
          loop (p :: choked) downloaders peers
        end
    in
    let choked = loop [] 0 peers in
    if List.length choked > 0 then
      let r = Random.int (List.length choked) in
      let i = ref 0 in
      List.iter (fun p ->
          if optimistic && !i = r then Peer.send_unchoke p else Peer.send_choke p;
          incr i) choked
  in
  if Torrent.is_complete bt.torrent then
    aux (fun a b -> compare (Peer.upload_rate b) (Peer.upload_rate a))
  else
    (* match bt.stage with *)
    (* | Leeching _ -> *) (* FIXME *)
    aux (fun a b -> compare (Peer.download_rate b) (Peer.download_rate a))
  (* | Seeding _ -> *)
    (* aux (fun a b -> compare (Peer.upload_rate b) (Peer.upload_rate a)) *)
  (* | _ -> *)
    (* () *)

let reset_peer_rates bt =
  PeerMgr.iter_peers Peer.reset_rates bt.peer_mgr
  (* Hashtbl.iter (fun _ p -> Peer.reset_rates p) bt.peers *)

let rechoke_downloads bt =
  (* match bt.stage with *)
  (* | Leeching (_, t) -> *) (* FIXME *)
  let h = Torrent.have bt.torrent in
  let doit p =
    let rec loop i =
      if i >= Bits.length h then false
      else if not (Bits.is_set h i) && Peer.has_piece p i then true else loop (i+1)
    in
    if loop 0 then Peer.send_interested p else Peer.send_not_interested p
  in
  PeerMgr.iter_peers doit bt.peer_mgr
  (* Hashtbl.iter (fun _ p -> doit p) bt.peers *)
  (* | _ -> *)
(* () *)

let print_info bt =
  (* match bt.stage with *)
  (* | Leeching (_, t) *)
  (* | Seeding (_, t) -> *)
  let dl, ul = 0.0, 0.0
  (* Hashtbl.fold *)
  (* (fun _ p (dl, ul) -> (dl +. Peer.download_rate p, ul +. Peer.upload_rate p)) *)
  (* bt.peers (0.0, 0.0) *)
  in
  let eta =
    let left = Torrent.amount_left bt.torrent in
    if dl = 0.0 then "Inf"
    else
      let eta = Int64.to_float left /. dl in
      let tm = Unix.gmtime eta in
      if tm.Unix.tm_mday > 1 || tm.Unix.tm_mon > 0 || tm.Unix.tm_year > 70 then "More than a day"
      else
        Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
  in

  Printf.eprintf "Progress: %d/%d (%d%%) Peers: %d Downloaded: %s (%s/s) Uploaded: %s (%s/s) ETA: %s\n%!"
    (Bits.count (Torrent.have bt.torrent))
    (Bits.length (Torrent.have bt.torrent))
    (truncate (100.0 *. (float (Bits.count (Torrent.have bt.torrent))) /. (float (Bits.length (Torrent.have bt.torrent)))))
  (* (Hashtbl.length bt.peers) *) 0
    (Util.string_of_file_size (Torrent.down bt.torrent))
    (Util.string_of_file_size (Int64.of_float dl))
    (Util.string_of_file_size (Torrent.up bt.torrent))
    (Util.string_of_file_size (Int64.of_float ul))
    eta
  (* | _ -> *)
    (* () *)

let rec rechoke_pulse bt optimistic rateiter =
  Log.info "rechoking (optimistic=%d,rateiter=%d)" optimistic rateiter;
  let optimistic = if optimistic = 0 then optimistic_unchoke_iterations else optimistic - 1 in
  let rateiter = if rateiter = 0 then rate_computation_iterations else rateiter - 1 in
  unchoke_peers bt (optimistic = 0);
  rechoke_downloads bt;
  print_info bt;
  if rateiter = 0 then reset_peer_rates bt;
  Lwt_unix.sleep (float unchoking_frequency) >>= fun () ->
  rechoke_pulse bt optimistic rateiter

let start ch =
  Lwt.async (fun () -> rechoke_pulse ch 1 1)
