open Cmdliner

let debug =
  let doc = "Enable debug output (note: this generates a LOT of output)" in
  Arg.(value & flag & info ["d"; "debug"] ~doc)

let magnets =
  let doc = "Magnet link of the torrent(s) to download" in
  Arg.(value & pos_all string [] & info [] ~docv:"MAGNET" ~doc)

let download magnet =
  let bt = Bt.Client.create (Bt.Magnet.of_string magnet) in
  Lwt.catch
    (fun () -> Bt.Client.start bt)
    (fun e -> Bt.Log.error ~exn:e "fatal error during download"; Lwt.return ())

let download_all debug magnets =
  if debug then Bt.Log.current_level := Bt.Log.DEBUG;
  Lwt_main.run (Lwt_list.iter_p download magnets)

let download_t =
  Term.(pure download_all $ debug $ magnets)

let info =
  let doc = "download torrent(s)" in
  Term.info "otorrent" ~version:"0.1" ~doc

let _ =
  match Term.eval (download_t, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
