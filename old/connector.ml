(* module H = Hashtbl.Make (Word160) *)

type event =
  [ `CONNECTED of Lwt_unix.file_descr * (Word160.t * Bits.t)
  | `FAILED of Unix.sockaddr * exn option]

type t = {
  id : Word160.t;
  info_hash : Word160.t;
  handshake : string;
  (* stop : unit -> unit; *)
  on_event : (event -> unit) Lwt_sequence.t
  (* connections : Peer.peer H.t; *)
  (* connecting : Unix.sockaddr Hashset.t; *)
  (* mutable spare : Unix.sockaddr list *)
}

let failwith_lwt fmt =
  Printf.ksprintf (fun msg -> Lwt.fail (Failure msg)) fmt

let max_initiate = 40

let string_of_sockaddr = function
  | Unix.ADDR_UNIX s ->
    s
  | Unix.ADDR_INET (addr, port) ->
    Unix.string_of_inet_addr addr ^ ":" ^ string_of_int port

(* let connection_made self sa fd (id, exts) : unit = *)
(*   Trace.infof "connection_made: %s/%a" (string_of_sockaddr sa) Word160.sprint id; *)
(*   let p = new Peer.info_peer fd self.info_hash (fun _ -> ()) (id, exts) in *)
(*   H.add self.connections id p *)

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let bittorrent_proto = "BitTorrent protocol"

let get_handshake ih : (Word160.t * Bits.t) Get.t =
  let open Get in
  char (String.length bittorrent_proto |> Char.chr) >>
  string bittorrent_proto >>
  string_of_length 8 >|= Bits.of_bin >>= fun exts ->
  string (Word160.to_bin ih) >>
  string_of_length 20 >|= Word160.from_bin >>= fun id ->
  return (id, exts)

(* let read_exactly fd n = *)
(*   let buf = String.create n in *)
(*   let rec loop o l = *)
(*     (\* Trace.infof "trace_exactly: loop: o=%d l=%d" o l; *\) *)
(*     if l <= 0 then *)
(*       Lwt.return buf *)
(*     else *)
(*       Lwt_unix.read fd buf o l >>= fun l' -> *)
(*       if l' = 0 then Lwt.fail End_of_file *)
(*       else loop (o+l') (l-l') *)
(*   in *)
(*   Lwt.catch (fun () -> loop 0 n) (fun exn -> *)
(*       Trace.infof ~exn "read_exactly"; Lwt.fail exn) *)

let read_exactly = Util.read_exactly

let read_handshake self sa fd =
  read_exactly fd (49 + String.length bittorrent_proto) >|= fun s ->
  (* Trace.infof "%s: got handshake: %S" (string_of_sockaddr sa) s; *)
  Get.run (get_handshake self.info_hash) s

let write_fully = Util.write_fully

(* let write_completely fd s = *)
(*   let rec loop o l = *)
(*     if l <= 0 then *)
(*       Lwt.return_unit *)
(*     else *)
(*       Lwt_unix.write fd s o l >>= fun l' -> *)
(*       (\* if l' = 0  *\) *)
(*       loop (o+l') (l-l') *)
(*   in *)
(*   Lwt.catch (fun () -> loop 0 (String.length s)) *)
(*     (fun exn -> Trace.infof ~exn "write_copletely"; Lwt.fail exn) *)
      
(* let rec fill_peers self = *)
(*   while H.length self.connections < max_initiate && *)
(*         List.length self.spare > 0 do *)
(*     let sa = List.hd self.spare in *)
(*     self.spare <- List.tl self.spare; *)
(*     connect self sa *)
(*   done *)

(* and connection_failed self ?exn sa = *)
(*   Trace.infof ?exn "connection_failed: %s" (string_of_sockaddr sa); *)
(*   Hashset.remove self.connecting sa; *)
(*   fill_peers self *)

(* and connection_lost self id exn = *)
(*   Trace.infof ~exn "connection_lost: %a" Word160.sprint id; *)
(*   H.remove self.connections id; *)
(*   fill_peers self *)

(* FIXME define actual exceptions instead of using Failure everywhere;
   that way, we can get the names printed using Printexc.to_string *)

let fire_event self ev =
  Lwt_sequence.iter_r (fun f -> f ev) self.on_event

let do_handshake self sa fd is_local =
  Trace.infof "Connecting to %s..." (string_of_sockaddr sa);
  let start () =
    (* Trace.infof "in start for %s" (string_of_sockaddr sa); *)
    if is_local then
      write_fully fd self.handshake >>= fun () ->
      read_handshake self sa fd
    else
      read_handshake self sa fd >>= fun id ->
      write_fully fd self.handshake >|= fun () -> id
  in
  Lwt.try_bind start
    (fun idexts ->
       (* Trace.infof "%s is connected" (string_of_sockaddr sa); *)
       `CONNECTED (fd, idexts) |> fire_event self;
       Lwt.return_unit)
    (fun exn ->
       Trace.infof ~exn "%s: handshake error" (string_of_sockaddr sa);
       `FAILED (sa, Some exn) |> fire_event self;
       Lwt.return_unit)

let connect self sa =
  (* Hashset.add self.connecting sa; *)
  Lwt.catch (fun () ->
      let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Lwt_unix.connect fd sa >>= fun () ->
      (* Trace.infof "%s: connected; proceeding to handshake" *)
      (*   (string_of_sockaddr sa); *)
      do_handshake self sa fd true)
    (fun exn ->
       Trace.infof "%s: connection failed before handshake"
         (string_of_sockaddr sa);
       `FAILED (sa, Some exn) |> fire_event self;
       Lwt.return_unit) |> ignore

let accept self (fd, sa) =
  (* if H.length self.connections >= max_initiate then begin *)
  (*   if List.length self.spare < max_initiate && *)
  (*      not (List.mem sa self.spare) then begin *)
  (*     self.spare <- sa :: self.spare; *)
  (*     Lwt_unix.close fd |> ignore *)
  (*     (\* Trace.infof "accept: saving %s in the spare set; now have [%d/%d]." *\) *)
  (*       (\* (string_of_sockaddr sa) (List.length self.spare) max_initiate *\) *)
  (*   end *)
  (* end else *)
  Lwt.async (fun () -> do_handshake self sa fd false)
 
(* let got_peer self sa = *)
(*   if not (List.mem sa self.spare) && not (Hashset.mem self.connecting sa) then *)
(*     self.spare <- sa :: self.spare; *)
(*   fill_peers self *)
    (* Trace.infof "got_peer: saving %s in the spare set; now have [%d/%d]." *)
      (* (string_of_sockaddr sa) (List.length self.spare) max_initiate *)
  (* fill_peers self *)
  (* end else *)
  (*   connect self sa *)

let handshake_message id info_hash =
  "\019BitTorrent protocol" ^ Bits.to_bin Peer.partial_exts ^
  Word160.to_bin info_hash ^ Word160.to_bin id

let create ~id ~info_hash =
  (* let stop_server, wake_stop = Lwt.wait () in *)
  (* let stop () = Lwt.wakeup wake_stop () in *)
  let self =
    { id;
      info_hash;
      handshake = handshake_message id info_hash;
      (* stop; *)
      on_event = Lwt_sequence.create () }
      (* connecting = Hashset.create 17; *)
      (* connections = H.create 17; *)
      (* spare = [] } *)
  in
  self

let on_event self f =
  Lwt_sequence.add_r f self.on_event |> ignore
