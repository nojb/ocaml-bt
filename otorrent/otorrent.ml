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

(* open Cmdliner *)

(* let debug_ = *)
(*   let doc = "Enable debug output (note: this generates a LOT of output)" in *)
(*   Arg.(value & flag & info ["d"; "debug"] ~doc) *)

(* let magnets = *)
(*   let doc = "Magnet link of the torrent(s) to download" in *)
(*   Arg.(value & pos_all string [] & info [] ~docv:"MAGNET" ~doc) *)

(* let (>>=) = Lwt.(>>=) *)

(* let download magnet = *)
(*   match Bt.Magnet.parse magnet with *)
(*   | `Ok m -> *)
(*       let bt = Bt.Client.create m in *)
(*       Lwt.catch *)
(*         (fun () -> Bt.Client.start bt) *)
(*         (fun exn -> (\* debug ~exn "fatal error during download"; *\) Lwt.return_unit) *)
(*   | `Error -> *)
(*       Lwt.return_unit *)

(* let download_all debug_ magnets = *)
  (* Bt.Log.active := debug_; *)
  (* Lwt_main.run (Lwt_list.iter_p download magnets) *)

(* let download_t = *)
(*   Term.(pure download_all $ debug_ $ magnets) *)

(* let info = *)
(*   let doc = "download torrent(s)" in *)
(*   Term.info "otorrent" ~version:"0.1" ~doc *)

let _ = Nocrypto.Rng.reseed (Cstruct.of_string "fadsfadsfadsfdasF")

let _ = Log.set_log_level Log.DEBUG; Log.color_on ()

let () =
  let t, u = Lwt.wait () in
  Bt.Client.LPD.start ~port:1234 ~info_hash:(Bt.SHA1.generate ()) (fun _ -> ());
  Lwt_main.run t
