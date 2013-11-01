(* open Lwt *)
(* open Spawn *)
open Monitor

let _ = Lwt_log.Section.set_level Lwt_log.Section.main Lwt_log.Debug

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let create_stream () =
  let s, w = Lwt_stream.create () in
  s, (fun x -> w (Some x))

let _ =
  Random.self_init ()

let start path =
  let torrent_info = Torrent.make (Bcode.from_file path) in
  Torrent.pp torrent_info;
  let status_ch, w_status_ch = create_stream () in
  let tracker_ch, w_tracker_ch = create_stream () in
  let peer_mgr_ch, w_peer_mgr_ch = create_stream () in
  let mgr_ch, w_mgr_ch = create_stream () in
  let peer_id = Torrent.gen_peer_id () in
  (* let dummy_ch, _ = create_stream () in *)
  let monitor = Monitor.create Monitor.AllForOne "MySup" in
  (* let _, sup_ch = Supervisor.start ~policy:Supervisor.AllForOne *)
  (*   ~name:"MySup" *)
  (*   ~children:[ *)
  PeerMgr.start ~monitor ~peer_mgr_ch ~mgr_ch ~w_mgr_ch ~peer_id;
  Tracker.start ~monitor ~torrent_info ~peer_id ~local_port:6881
    ~w_status_ch ~tracker_ch ~w_tracker_ch ~w_peer_mgr_ch;
  Status.start ~monitor ~status_ch;
  torrent_info, w_tracker_ch, w_status_ch, monitor
(* Supervisor.Worker (PeerMgr.start ~peer_mgr_ch ~mgr_ch ~w_mgr_ch ~peer_id); *)
(* Supervisor.Worker (Tracker.start *)
(*     ~torrent_info:t *)
(*     ~peer_id *)
(*     ~local_port:6881 *)
(*     ~w_status_ch *)
(*     ~tracker_ch *)
(*     ~w_tracker_ch *)
(*     ~w_peer_mgr_ch); *)
(*  Supervisor.Worker (Status.start ~status_ch) ] *)
(*     dummy_ch *)
(*     (fun _ -> ()) in *)
(*   t, w_tracker_ch, w_status_ch, sup_ch *)

let _ =
  let t, w_tracker_ch, w_status_ch, _ = start "wabi.torrent" in
  w_status_ch (Messages.InsertTorrent
    (t.Torrent.info_hash, t.Torrent.total_length));
  w_tracker_ch Messages.Start;
  Lwt_main.run (fst (Lwt.wait ()))
