let (>>=) = Lwt.(>>=)

(* let handle_client_info .... = *)
(*   Printf.printf "" *)

(*
validating ... 9% 10% 100% done.
downloading:
name                              peers   available pieces   download speed upload speed complete
ubuntu-13.10-desktop-amd64.iso      23 56 652 13242/12342     45KiB/s 3.0MiB/s 90%
*)

let handle_client_update upd =
  assert false

let download path =
  let bc = Get.run_file Bcode.bdecode path in
  let info = Info.create bc in
  Client.create info
  
let _ =
  if Array.length Sys.argv > 1 then begin
    Lwt.async (fun () -> download Sys.argv.(1));
    Lwt.wait () |> fst |> Lwt_main.run
  end

(* let main path = *)
(*   let bc = Get.run_file Bcode.bdecode path in *)
(*   let server = Server.create info in *)
(*   Torrent.create info >>= fun torrent -> *)
(*   Announce.create info (Server.start_connection server) *)
