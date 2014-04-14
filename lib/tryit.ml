let mg = Bt.Magnet.of_string "magnet:?xt=urn:btih:76864f9ccdcfe48aea3dfd22196e2c93765377cb&dn=Wadjda.2012.1080p.Bluray.x264.anoXmous&tr=udp%3A//tracker.openbittorrent.com%3A80&tr=udp%3A//tracker.publicbt.com%3A80&tr=udp%3A//tracker.istole.it%3A6969&tr=udp%3A//tracker.ccc.de%3A80&tr=udp%3A//open.demonii.com%3A1337"

let mg2 = Bt.Magnet.of_string "magnet:?xt=urn:btih:6273026a2ee0c72fbb32820626a575c89bf10fb4&dn=Celtic+Fingerstyle+Guitar+Vol.+1+%26amp%3B+2&tr=udp%3A//tracker.openbittorrent.com%3A80&tr=udp%3A//tracker.publicbt.com%3A80&tr=udp%3A//tracker.istole.it%3A6969&tr=udp%3A//tracker.ccc.de%3A80&tr=udp%3A//open.demonii.com%3A1337"

let _ =
  let bt = Bt.Client.create mg2 in
  let t, w = Lwt.wait () in
  let _ = Bt.Client.start bt in
  Lwt_main.run t
