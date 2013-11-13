let _ = Lwt_log.Section.set_level Lwt_log.Section.main Lwt_log.Debug

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let _ =
  Random.self_init ()

let download path =
  let peer_id = Torrent.gen_peer_id () in
  let status_ch = Lwt_pipe.create () in
  let torrent_mgr_ch = Lwt_pipe.create () in
  let peer_mgr_ch = Lwt_pipe.create () in
  let children = [
    Msg.Worker (TorrentMgr.start ~status_ch ~peer_id ~peer_mgr_ch
      ~ch:torrent_mgr_ch);
    Msg.Worker (Status.start ~ch:status_ch);
    Msg.Worker (PeerMgr.start ~ch:peer_mgr_ch ~peer_id)
  ]
  in
  let _ =
    let fake_ch = Lwt_pipe.create () in
    (* Lwt_pipe.close fake_ch; *)
    let ch = Lwt_pipe.create () in
    Super.start ~super_ch:fake_ch Super.AllForOne "MainSup" ~children ~ch
  in
  Lwt_pipe.write torrent_mgr_ch (Msg.AddedTorrent path);
  Lwt_main.run (fst (Lwt.wait ()))

let _ =
  if Array.length Sys.argv > 1 then
    download Sys.argv.(1)
