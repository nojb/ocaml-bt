open Messages

let _ = Lwt_log.Section.set_level Lwt_log.Section.main Lwt_log.Debug

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let debug = Supervisor.debug

let create_stream () =
  let s, w = Lwt_stream.create () in
  s, (fun x -> w (Some x))

let _ =
  Random.self_init ()

let download path =
  let peer_id = Torrent.gen_peer_id () in
  let status, msg_status = create_stream () in
  let torrent_mgr, msg_torrent_mgr = create_stream () in
  let peer_mgr, msg_peer_mgr = create_stream () in
  let mgr, msg_mgr = create_stream () in
  let msg_supervisor = Supervisor.top_level "MainSup" Supervisor.AllForOne in
  TorrentMgr.start ~msg_supervisor ~torrent_mgr ~msg_status ~peer_id ~msg_peer_mgr;
  Status.start ~msg_supervisor ~status_ch:status;
  PeerMgr.start ~msg_supervisor ~peer_mgr_ch:peer_mgr ~mgr_ch:mgr
    ~w_mgr_ch:msg_mgr ~peer_id;
  msg_torrent_mgr (AddedTorrent path);
  Lwt_main.run (fst (Lwt.wait ()))

let _ =
  download "wabi.torrent"

(* let start path = *)
(*   let torrent_info = Torrent.make (Bcode.from_file path) in *)
(*   Torrent.pp torrent_info; *)
(*   let status_ch, w_status_ch = create_stream () in *)
(*   let tracker_ch, w_tracker_ch = create_stream () in *)
(*   let peer_mgr_ch, w_peer_mgr_ch = create_stream () in *)
(*   let mgr_ch, w_mgr_ch = create_stream () in *)
(*   let peer_id = Torrent.gen_peer_id () in *)
(*   let monitor = Monitor.create Monitor.AllForOne "MySup" in *)
(*   PeerMgr.start ~monitor ~peer_mgr_ch ~mgr_ch ~w_mgr_ch ~peer_id; *)
(*   Tracker.start ~monitor ~torrent_info ~peer_id ~local_port:6881 ~w_status_ch ~tracker_ch ~w_tracker_ch ~w_peer_mgr_ch; *)
(*   Status.start ~monitor ~status_ch; *)
(*   torrent_info, w_tracker_ch, w_status_ch, monitor, msg_peer_mgr *)
(*  *)
(* let _ = *)
(*   let t, w_tracker_ch, w_status_ch, _, msg_peer_mgr = start "wabi.torrent" in *)
(*   let ih = t.Torrent.info_hash in *)
(*   let tl = { pieces = t.Torrent.pieces; msg_piece_mgr } *)
(*   msg_peer_mgr (NewTorrent (ih, ...)); *)
(*   w_status_ch (InsertTorrent (ih, t.Torrent.total_length)); *)
(*   w_tracker_ch Start; *)
(*   Lwt_main.run (fst (Lwt.wait ())) *)
