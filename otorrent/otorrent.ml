open Cmdliner

let magnets =
  let doc = "Magnet link of the torrent(s) to download" in
  Arg.(value & pos_all string [] & info [] ~docv:"MAGNET" ~doc)

let download magnet =
  let bt = Bt.Client.create (Bt.Magnet.of_string magnet) in
  Lwt.catch
    (fun () -> Bt.Client.start bt)
    (fun e -> Bt.Log.error ~exn:e "fatal error during download"; Lwt.return ())

let download_t =
  Term.(pure (fun xs -> Lwt_main.run (Lwt_list.iter_p download xs)) $ magnets)

let info =
  let doc = "download torrent(s)" in
  Term.info "otorrent" ~version:"0.1" ~doc

let _ =
  match Term.eval (download_t, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
