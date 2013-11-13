open Printf

let (>>=) = Lwt.(>>=)

let string_of_msg = function
  | Msg.AddedTorrent path ->
    sprintf "AddedTorrent: %S" path

type t = {
  send_status : Msg.status_msg option -> unit;
  send_peer_mgr : Msg.peer_mgr_msg option -> unit;
  send_super : Msg.super_msg option -> unit;
  peer_id : Torrent.peer_id;
  id : Proc.Id.t
}

let debug t ?exn fmt =
  Printf.ksprintf (fun msg ->
    Lwt_log.debug_f ?exn "TorrentMgr %s: %s" (Proc.Id.to_string t.id) msg) fmt

let add_torrent t path : unit Lwt.t =
  let torrent_info = Torrent.make (Bcode.from_file path) in
  Torrent.pp torrent_info;
  let ih = torrent_info.Torrent.info_hash in
  let tl =
    { Msg.send_piece_mgr = (fun _ -> ()); Msg.pieces = torrent_info.Torrent.pieces }
  in
  let msgs_sup, send_sup = Lwt_stream.create () in
  let msgs_track_super, send_track_super = Lwt_stream.create () in
  let start_trackers, send_trackers = List.split (List.map (fun tier ->
    let msgs, send = Lwt_stream.create () in
    Msg.Worker (Tracker.start
      ~info_hash:ih
      ~tier
      ~peer_id:t.peer_id
      ~local_port:6881
      ~send_status:t.send_status
      ~send_peer_mgr:t.send_peer_mgr
      ~msgs ~send), send) torrent_info.Torrent.announce_list)
  in
  let start_track_sup =
    Super.start Super.OneForOne "TrackerSup" ~children:start_trackers ~msgs:msgs_track_super
      ~send:send_track_super
  in
  (* lwt handles, have = Fs.open_and_check_file id torrent_info in *)
  let start_super = Super.start Super.AllForOne "TorrentSup"
    ~children:[ Msg.Supervisor start_track_sup ]
    ~msgs:msgs_sup ~send:send_sup
  in
  t.send_super (Some (Msg.SpawnNew (Msg.Supervisor start_super)));
  (* let _, send_fs = Fs.start ~send_super ~handles ~pieces in *)
  t.send_status (Some (Msg.InsertTorrent (ih,
    torrent_info.Torrent.total_length)));
  t.send_peer_mgr (Some (Msg.NewTorrent (ih, tl)));
  List.iter (fun send -> send (Some Msg.Start)) send_trackers;
  Lwt.return_unit

let handle_message t msg =
  debug t "%s" (string_of_msg msg) >>= fun () ->
  match msg with
  | Msg.AddedTorrent path ->
    add_torrent t path

let start ~send_super ~send_status ~peer_id ~send_peer_mgr ~msgs =
  let run id =
    let t = { send_super; send_status; send_peer_mgr; peer_id; id } in
    Lwt_stream.iter_s (handle_message t) msgs
  in
  Proc.spawn (Proc.cleanup run
    (Super.default_stop send_super) (fun _ -> Lwt.return_unit))
