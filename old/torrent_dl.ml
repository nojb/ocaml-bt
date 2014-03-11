(* let (>>=) = Lwt.(>>=) *)

(* let handle_client_info .... = *)
(*   Printf.printf "" *)

(*
validating ... 9% 10% 100% done.
downloading:
name                              peers   available pieces   download speed upload speed complete
ubuntu-13.10-desktop-amd64.iso      23 56 652 13242/12342     45KiB/s 3.0MiB/s 90%
*)

(* let bittorrent_id_prefix = "-OC0001-" *)

let download magnet =
  let open Magnet in
  let m = Magnet.of_string magnet in
  (* let bc = Get.run_file Bcode.bdecode path in *)
  (* let server = Server.create m.xt in *)
  let c = Client.create m.xt m.tr (fun _ -> ()) in
  Lwt.wait () |> fst |> Lwt_main.run
  (* let id = Word160.peer_id bittorrent_id_prefix in *)
  (* let ctr = Connector.create id m.xt in *)
  (* let svr = Server.create (Connector.accept ctr) in *)
  (* let _ = *)
  (*   Announce.create m.xt (List.map (fun x -> [x]) m.tr) *)
  (*     (fun _ -> 0L) (fun _ -> 0L) (fun _ -> 0L) *)
  (*     (Server.port svr) id (Connector.got_peer ctr) *)
  (* in *)
  (* Lwt.return_unit *)
  (* let info = Info.create bc in *)
  (* Client.create info *)
  
let _ =
  if Array.length Sys.argv > 1 then
    download Sys.argv.(1)

(* let main path = *)
(*   let bc = Get.run_file Bcode.bdecode path in *)
(*   let server = Server.create info in *)
(*   Torrent.create info >>= fun torrent -> *)
(*   Announce.create info (Server.start_connection server) *)
