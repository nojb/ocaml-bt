(* The MIT License (MIT)

   Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

let section = Bt.Log.make_section "otorrent"

let error ?exn fmt = Bt.Log.error section ?exn fmt
let info ?exn fmt = Bt.Log.info section ?exn fmt

open Cmdliner

let print_stats_header () =
  Printf.printf "%%   Total   Download   Upload   Time Left   Peers   Pieces\n%!"

let print_stats stats =
  let module Stats = Bt.Stats in
  let module Util = Bt.Util in
  let progress = float stats.Stats.have_pieces /. float stats.Stats.total_pieces in
  let eta =
    let left = stats.Stats.amount_left in
    let dl = stats.Stats.download_speed in
    if dl = 0.0 then "Inf"
    else
      let eta = Int64.to_float left /. dl in
      let tm = Unix.gmtime eta in
      if tm.Unix.tm_mday > 1 || tm.Unix.tm_mon > 0 || tm.Unix.tm_year > 70 then ">1d"
      else
        Printf.sprintf "%02d:%02d:%02d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
  in
  Printf.printf "\027[G\027[2K%d  %s      %s/s       %s/s       %s         %d/%d   %d/%d%!"
    (truncate (100.0 *. progress))
    (Util.string_of_file_size stats.Stats.total_size)
    (Util.string_of_file_size (Int64.of_float stats.Stats.download_speed))
    (Util.string_of_file_size (Int64.of_float stats.Stats.upload_speed))
    eta
    stats.Stats.num_connected_peers stats.Stats.num_total_peers
    stats.Stats.have_pieces stats.Stats.total_pieces

let debug =
  let doc = "Enable debug output (note: this generates a LOT of output)" in
  Arg.(value & flag & info ["d"; "debug"] ~doc)

let magnets =
  let doc = "Magnet link of the torrent(s) to download" in
  Arg.(value & pos_all string [] & info [] ~docv:"MAGNET" ~doc)

let (>>=) = Lwt.(>>=)

let download magnet =
  let bt = Bt.Client.create (Bt.Magnet.of_string magnet) in
  let rec print_stats_loop bt =
    print_stats (Bt.Client.stats bt);
    Lwt_unix.sleep 1.0 >>= fun () -> print_stats_loop bt
  in
  Lwt.catch
    (fun () ->
       let t = Bt.Client.start bt in
       print_stats_header ();
       Lwt.pick [print_stats_loop bt; t])
    (fun exn -> error ~exn "fatal error during download"; Lwt.return ())

let download_all debug magnets =
  if debug then Bt.Log.log_level := Bt.Log.Debug;
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
