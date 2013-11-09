open Printf
open Messages

let debug = Supervisor.debug

let create_stream () =
  let s, w = Lwt_stream.create () in
  s, (fun x -> w (Some x))

let string_of_msg = function
  | AddedTorrent path ->
    sprintf "AddedTorrent: %S" path

type state = {
  msg_status : status_msg -> unit;
  msg_peer_mgr : peer_mgr_msg -> unit;
  peer_id : Torrent.peer_id
}

let add_torrent id msg_supervisor st path : unit Lwt.t =
  let torrent_info = Torrent.make (Bcode.from_file path) in
  Torrent.pp torrent_info;
  let ih = torrent_info.Torrent.info_hash in
  let tl =
    { msg_piece_mgr = (fun _ -> ()); pieces = torrent_info.Torrent.pieces }
  in
  (* FIXME check that it doesn't already exist *)
  let fs, msg_fs = create_stream () in
  let msg_supervisor = Supervisor.spawn_supervisor
    msg_supervisor (* FIXME *)
    (sprintf "TorrentSup [%s]" torrent_info.Torrent.name)
    Supervisor.AllForOne
  in
  let msg_tracker_sup =
    Supervisor.spawn_supervisor
    msg_supervisor
    (sprintf "TrackerSup [%s]" torrent_info.Torrent.name)
    Supervisor.OneForOne (* FIXME Restartable *)
  in
  let msg_trackers = List.map (fun tier ->
    Tracker.start
      ~msg_supervisor:msg_tracker_sup
      ~info_hash:ih
      ~tier
      ~peer_id:st.peer_id
      ~local_port:6881
      ~w_status_ch:st.msg_status
      ~w_peer_mgr_ch:st.msg_peer_mgr) torrent_info.Torrent.announce_list
  in
  (* let msg_tracker = Tracker.start *)
  (*   ~msg_supervisor *)
  (*   ~info_hash:ih *)
  (*   ~announce_list:torrent_info.Torrent.announce_list *)
  (*   ~peer_id:st.peer_id *)
  (*   ~local_port:6881 ~w_status_ch:st.msg_status *)
  (*   ~w_peer_mgr_ch:st.msg_peer_mgr *)
  (* in *)
  lwt handles, have = Fs.open_and_check_file id torrent_info in
  Fs.start ~msg_supervisor ~handles ~pieces:torrent_info.Torrent.pieces ~fs;
  st.msg_status (InsertTorrent (ih, torrent_info.Torrent.total_length));
  st.msg_peer_mgr (NewTorrent (ih, tl));
  List.iter (fun msg_tracker -> msg_tracker Start) msg_trackers;
  Lwt.return ()

let start ~msg_supervisor ~torrent_mgr ~msg_status ~peer_id ~msg_peer_mgr =
  let st =
    { msg_status; msg_peer_mgr; peer_id }
  in
  let handle_msg id msg =
    debug id "%s" (string_of_msg msg) >>
    match msg with
    | AddedTorrent path ->
      add_torrent id msg_supervisor st path
  in
  Supervisor.spawn_worker msg_supervisor "TorrentMgr"
    (fun id -> Lwt_stream.iter_s (handle_msg id) torrent_mgr)
