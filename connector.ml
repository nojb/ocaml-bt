module H = Hashtbl.Make (Word160)

type t = {
  id : Word160.t;
  info_hash : Word160.t;
  handshake : string;
  stop : unit -> unit;
  connections : Peer.peer H.t;
  connecting : Unix.sockaddr Hashset.t;
  mutable spare : Unix.sockaddr list
}

let failwith_lwt fmt =
  Printf.ksprintf (fun msg -> Lwt.fail (Failure msg)) fmt

let max_initiate = 40

let string_of_sockaddr = function
  | Unix.ADDR_UNIX s ->
    s
  | Unix.ADDR_INET (addr, port) ->
    Unix.string_of_inet_addr addr ^ ":" ^ string_of_int port

let connection_made self sa fd (id, exts) : unit =
  Trace.infof "connection_made: %s/%a" (string_of_sockaddr sa) Word160.sprint id;
  let p = new Peer.info_peer fd self.info_hash (fun _ -> ()) (id, exts) in
  H.add self.connections id p

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

let read_exactly fd n =
  let buf = String.create n in
  let rec loop o l =
    if l <= 0 then
      Lwt.return buf
    else
      Lwt_unix.read fd buf o l >>= fun l' ->
      loop (o+l') (l-l')
  in
  loop 0 n

let read_handshake self sa fd =
  read_exactly fd (49 + String.length bittorrent_proto) >|=
  Get.run (get_handshake self.info_hash)    

let write_completely fd s =
  let rec loop o l =
    if l <= 0 then
      Lwt.return_unit
    else
      Lwt_unix.write fd s o l >>= fun l' ->
      loop (o+l') (l-l')
  in
  loop 0 (String.length s)  
      
let rec fill_peers self =
  while H.length self.connections < max_initiate &&
        List.length self.spare > 0 do
    let sa = List.hd self.spare in
    self.spare <- List.tl self.spare;
    connect self sa
  done

and connection_failed self ?exn sa =
  Trace.infof ?exn "connection_failed: %s" (string_of_sockaddr sa);
  Hashset.remove self.connecting sa;
  fill_peers self

and connection_lost self id exn =
  Trace.infof ~exn "connection_lost: %a" Word160.sprint id;
  H.remove self.connections id;
  fill_peers self

(* FIXME define actual exceptions instead of using Failure everywhere;
   that way, we can get the names printed using Printexc.to_string *)

and do_handshake self sa fd is_local =
  Trace.infof "Trying to connect to %s..." (string_of_sockaddr sa);
  let start () =
    if is_local then
      write_completely fd self.handshake >>= fun () ->
      read_handshake self sa fd
    else
      read_handshake self sa fd >>= fun id ->
      write_completely fd self.handshake >|= fun () -> id
  in
  Lwt.try_bind start
    (connection_made self sa fd |> Lwt.wrap1)
    (fun exn ->
       Trace.infof ~exn "%s: handshake error" (string_of_sockaddr sa);
       connection_failed self sa;
       Lwt.return_unit)

and connect self sa =
  Hashset.add self.connecting sa;
  Lwt.catch (fun () ->
      let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      Lwt_unix.connect fd sa >>= fun () ->
      do_handshake self sa fd true)
    (fun exn ->
       connection_failed self ~exn sa;
       Lwt.return_unit) |> ignore

let accept self (fd, sa) =
  if H.length self.connections >= max_initiate then begin
    if List.length self.spare < max_initiate &&
       not (List.mem sa self.spare) then begin
      self.spare <- sa :: self.spare;
      Lwt_unix.close fd |> ignore
      (* Trace.infof "accept: saving %s in the spare set; now have [%d/%d]." *)
        (* (string_of_sockaddr sa) (List.length self.spare) max_initiate *)
    end
  end else
    Lwt.async (fun () -> do_handshake self sa fd false)
 
let got_peer self sa =
  if not (List.mem sa self.spare) && not (Hashset.mem self.connecting sa) then
    self.spare <- sa :: self.spare;
  fill_peers self
    (* Trace.infof "got_peer: saving %s in the spare set; now have [%d/%d]." *)
      (* (string_of_sockaddr sa) (List.length self.spare) max_initiate *)
  (* fill_peers self *)
  (* end else *)
  (*   connect self sa *)

let handshake_message id info_hash =
  "\019BitTorrent protocol" ^ Bits.to_bin Peer.partial_exts ^
  Word160.to_bin info_hash ^ Word160.to_bin id

let create id info_hash =
  let stop_server, wake_stop = Lwt.wait () in
  let stop () = Lwt.wakeup wake_stop () in
  let self =
    { id;
      info_hash;
      handshake = handshake_message id info_hash;
      stop;
      connecting = Hashset.create 17;
      connections = H.create 17;
      spare = [] }
  in
  self
