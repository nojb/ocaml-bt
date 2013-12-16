let (>>=) = Lwt.(>>=)

(* let handle_client_info .... = *)
(*   Printf.printf "" *)

(*

validating ... 9% 10% 100% done.
downloading:
name                              peers   available pieces   download speed upload speed complete
ubuntu-13.10-desktop-amd64.iso      23 56 652 13242/12342     45KiB/s 3.0MiB/s 90% *)

let handle_client_update upd =
  assert false

let download path =
  let t, w = Lwt.wait () in
  let bc = Bcode.from_file path in
  let info = Info.create bc in
  Store.open_and_check_file info >>= fun h ->
  let cl = Client.create h info in
  Client.add_handler cl handle_client_update;
  Client.start cl;
  t
  
let _ =
  if Array.length Sys.argv > 1 then
    Lwt_main.run (download Sys.argv.(1))
