(* The MIT License (MIT)

   Copyright (c) 2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
   FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
   COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

let () =
  Nocrypto.Rng.reseed (Cstruct.of_string "fadsfadsfadsfdasF");
  Log.set_log_level Log.DEBUG;
  Log.color_on ()

module Log = Log.Make (struct let section = "[Driver]" end)

open Cmdliner

let default =
  "magnet:?xt=urn:btih:2cfcb66cac39ad302adb06075511ac151636b19e&dn=Last.Knights.2015.HDRip.XViD-ETRG+&tr=udp%3A%2F%2Fopen.demonii.com%3A1337&tr=udp%3A%2F%2Ftracker.coppersurfer.tk%3A6969&tr=udp%3A%2F%2Ftracker.leechers-paradise.org%3A6969&tr=udp%3A%2F%2Fexodus.desync.com%3A6969"

let magnet =
  let doc = "Magnet link of the torrent(s) to download" in
  Arg.(value & pos 0 (some string) (Some default) & info [] ~docv:"MAGNET" ~doc)

open Lwt.Infix

let download magnet =
  match Bt.Magnet.parse magnet with
  | `Ok magnet ->
      let bt = Bt.Client.create magnet in
      Lwt.catch
        (fun () ->
           Bt.Client.start bt)
        (fun exn ->
           Log.error "exn:%S" (Printexc.to_string exn);
           Lwt.return_unit)
  | `Error ->
      Lwt.return_unit

let download = function
  | Some magnet ->
      Lwt_main.run (download magnet)
  | None ->
      ()

let () =
  let doc = "download torrent" in
  let version = "0.1" in
  ignore Term.(eval ~catch:true (pure download $ magnet, info ~version ~doc "otorrent"))
