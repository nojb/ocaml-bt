let _ = Lwt_log.Section.set_level Lwt_log.Section.main Lwt_log.Debug

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let _ =
  Random.self_init ()

let download path =
  let peer_id = Torrent.gen_peer_id () in
  let msgs_status, send_status = Lwt_stream.create () in
  let msgs_torrent_mgr, send_torrent_mgr = Lwt_stream.create () in
  let msgs_peer_mgr, send_peer_mgr = Lwt_stream.create () in
  let children = [
    Msg.Worker (TorrentMgr.start ~send_status ~peer_id ~send_peer_mgr ~msgs:msgs_torrent_mgr);
    Msg.Worker (Status.start ~msgs:msgs_status);
    Msg.Worker (PeerMgr.start ~msgs:msgs_peer_mgr ~send:send_peer_mgr ~peer_id)
  ]
  in
  let _ =
    let send_super _ = failwith "should not happen" in
    let msgs, send = Lwt_stream.create () in
    Super.start ~send_super Super.AllForOne "MainSup" ~children ~msgs ~send
  in
  send_torrent_mgr (Some (Msg.AddedTorrent path));
  Lwt_main.run (fst (Lwt.wait ()))

let _ =
  if Array.length Sys.argv > 1 then
    download Sys.argv.(1)
