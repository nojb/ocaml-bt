let _ = Lwt_log.Section.set_level Lwt_log.Section.main Lwt_log.Debug

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let debug = Proc.debug

let _ =
  Random.self_init ()

type msg =
  [ `Start
  | `Stop ]

let string_of_msg = function
  | `Start ->
    "Start"
  | `Stop ->
    "Stop"

type t = {
  info : Info.t;
  peer_id : Info.PeerId.t;
  done_mv : unit Lwt_mvar.t;
  id : Proc.Id.t
}

module H = Hashtbl.Make (Proc.Id)

let start t : unit Lwt.t =
  let info_hash = t.info.Info.info_hash in
  let pieces = t.info.Info.pieces in
  let peer_id = t.peer_id in

  let piece_mgr_ch = Lwt_pipe.create () in
  let fs_ch = Lwt_pipe.create () in
  let sup_ch = Lwt_pipe.create () in
  let status_ch = Lwt_pipe.create () in
  let peer_mgr_ch = Lwt_pipe.create () in
  let tracker_sup_ch = Lwt_pipe.create () in
  let fake_ch = Lwt_pipe.create () in
  let choke_mgr_ch = Lwt_pipe.create () in

  let start_trackers, tracker_chs = List.split (List.map (fun tier ->
    let ch = Lwt_pipe.create () in
    Msg.Worker (Tracker.start
      ~info_hash
      ~tier
      ~peer_id:t.peer_id
      ~local_port:6881
      ~status_ch:status_ch
      ~peer_mgr_ch:peer_mgr_ch
      ~ch), ch) t.info.Info.announce_list)
  in
  let start_track_sup =
    Super.start Super.OneForOne "TrackerSup" ~children:start_trackers
      ~ch:tracker_sup_ch
  in
  lwt handles, have = Fs.open_and_check_file t.id t.info in
  let left = Info.bytes_left have pieces in
  let _ = Super.start ~super_ch:fake_ch Super.AllForOne "TorrentSup"
    ~children:[
      Msg.Worker (Status.start ~ch:status_ch ~info_hash ~left);
      Msg.Worker (PeerMgr.start ~ch:peer_mgr_ch ~fs_ch ~choke_mgr_ch ~peer_id
        ~info_hash ~piece_mgr_ch ~pieces);
      Msg.Worker (ChokeMgr.start ~ch:choke_mgr_ch);
      Msg.Worker (Fs.start ~ch:fs_ch ~pieces ~handles);
      Msg.Worker (PieceMgr.start ~ch:piece_mgr_ch ~fs_ch ~choke_mgr_ch
        ~status_ch ~have ~pieces ~info_hash);
      Msg.Supervisor start_track_sup
    ]
    ~ch:sup_ch
  in
  List.iter (fun ch -> Lwt_pipe.write ch Tracker.Start) tracker_chs;
  Lwt.return_unit

let handle_message t msg : unit Lwt.t =
  match msg with
  | `Start ->
    start t
  | msg ->
    debug t.id "Unhandled: %s" (string_of_msg msg)

let download path =
  let ch = Lwt_pipe.create () in
  let done_mv = Lwt_mvar.create_empty () in
  let run id =
    let info = Info.make (Bcode.from_file path) in
    let peer_id = Info.gen_peer_id () in
    Info.pp info;
    let t =
      { info; peer_id; id; done_mv }
    in
    Lwt_pipe.iter_s (handle_message t) ch
  in
  let _ = Proc.spawn ~name:"Client" run (fun _ -> Lwt.return_unit)
  in
  Lwt_pipe.write ch `Start;
  Lwt_mvar.take done_mv
