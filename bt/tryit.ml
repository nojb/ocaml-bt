let mg = Bt.Magnet.of_string "magnet:?xt=urn:btih:76864f9ccdcfe48aea3dfd22196e2c93765377cb&dn=Wadjda.2012.1080p.Bluray.x264.anoXmous&tr=udp%3A//tracker.openbittorrent.com%3A80&tr=udp%3A//tracker.publicbt.com%3A80&tr=udp%3A//tracker.istole.it%3A6969&tr=udp%3A//tracker.ccc.de%3A80&tr=udp%3A//open.demonii.com%3A1337"

let mg2 = Bt.Magnet.of_string "magnet:?xt=urn:btih:6273026a2ee0c72fbb32820626a575c89bf10fb4&dn=Celtic+Fingerstyle+Guitar+Vol.+1+%26amp%3B+2&tr=udp%3A//tracker.openbittorrent.com%3A80&tr=udp%3A//tracker.publicbt.com%3A80&tr=udp%3A//tracker.istole.it%3A6969&tr=udp%3A//tracker.ccc.de%3A80&tr=udp%3A//open.demonii.com%3A1337"

let mg3 = Bt.Magnet.of_string "magnet:?xt=urn:btih:e60f9d6471e6b4a5e7285be6bc18c3136ca0c429&dn=28+Weeks+Later%5B2007%5DDvDrip%5BEng%5D-aXXo&tr=udp%3A//tracker.openbittorrent.com%3A80&tr=udp%3A//tracker.publicbt.com%3A80&tr=udp%3A//tracker.istole.it%3A6969&tr=udp%3A//tracker.ccc.de%3A80&tr=udp%3A//open.demonii.com%3A1337"

let mg4 = Bt.Magnet.of_string "magnet:?xt=urn:btih:d564996fb48a28f29ee1284c5f969936c79b353d&dn=I+Am+Soldier+%5B2014%5D+BRRip+XViD+juggs%5BETRG%5D&tr=udp%3A//tracker.openbittorrent.com%3A80&tr=udp%3A//tracker.publicbt.com%3A80&tr=udp%3A//tracker.istole.it%3A6969&tr=udp%3A//tracker.ccc.de%3A80&tr=udp%3A//open.demonii.com%3A1337"

let _ =
  let bt = Bt.Client.create (Bt.Magnet.of_string Sys.argv.(1)) in
  let t, w = Lwt.wait () in
  let _ = Bt.Client.start bt in
  Lwt_main.run t
