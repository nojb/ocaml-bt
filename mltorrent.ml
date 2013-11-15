let _ =
  if Array.length Sys.argv > 1 then
    Lwt_main.run (Torrent.download Sys.argv.(1))
