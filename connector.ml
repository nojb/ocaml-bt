module H = Hashtbl.Make (Word160)

type t = {
  id : Word160.t;
  info_hash : Word160.t;
  handshake : string;
  stop : unit -> unit;
  connections : Peer.t H.t;
  mutable spare : Unix.sockaddr list
}

let failwith_lwt fmt =
  Printf.ksprintf (fun msg -> Lwt.fail (Failure msg)) fmt

let max_initiate = 40
(* let outbound_connections_pool_size = 20 *)
(* let outbound_connections_thread_keep_alive_secs = 10 *)
(* let client_keep_alive_minutes = 3 *)

let string_of_sockaddr = function
  | Unix.ADDR_UNIX s ->
    s
  | Unix.ADDR_INET (addr, port) ->
    Unix.string_of_inet_addr addr ^ ":" ^ string_of_int port

let connection_made self sa ic oc (id, exts) : unit =
  (* FIXME if id = self.id then close connection or if id is already there *)
  Trace.infof "connection_made: %s/%a" (string_of_sockaddr sa) Word160.sprint id;
  let p = Peer.create_with_partial sa id ic oc self.info_hash (fun _ -> ()) exts in
  H.add self.connections id p

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let bittorrent_proto = "BitTorrent protocol"

let read_handshake self sa ic =
  let read_exactly n ic =
    let buf = String.create n in
    Lwt_io.read_into_exactly ic buf 0 n >|= fun () ->
    buf
  in
  Lwt_io.read_char ic >|= int_of_char >>= fun pstrlen ->
  if pstrlen <> String.length bittorrent_proto then
    failwith_lwt "%s: bad proto lenght: %d" (string_of_sockaddr sa) pstrlen
  else
    read_exactly pstrlen ic >>= fun proto ->
    if proto = bittorrent_proto then
      read_exactly 8 ic >|= Bits.of_bin >>= fun exts ->
      read_exactly 20 ic >|= Word160.from_bin >>= fun ih ->
      if Word160.equal ih self.info_hash then
        read_exactly 20 ic >|= fun id -> Word160.from_bin id, exts
      else
        failwith_lwt "%s: bad info hash %s" (string_of_sockaddr sa) (Word160.to_hex ih)
    else
      failwith_lwt "%s: unknown protocol %S" (string_of_sockaddr sa) proto
      
let rec fill_peers self =
  if H.length self.connections < max_initiate then
    if List.length self.spare > 0 then begin
      let sa = List.hd self.spare in
      self.spare <- List.tl self.spare;
      connect self sa
    end

and connection_failed self ?exn sa =
  Trace.infof ?exn "connection_failed: %s" (string_of_sockaddr sa);
  fill_peers self

and connection_lost self id exn =
  Trace.infof ~exn "connection_lost: %a" Word160.sprint id;
  H.remove self.connections id;
  fill_peers self
  
and do_handshake self sa ic oc is_local =
  let start () =
    if is_local then
      Lwt_io.write oc self.handshake >>= fun () ->
      read_handshake self sa ic
    else
      read_handshake self sa ic >>= fun id ->
      Lwt_io.write oc self.handshake >|= fun () -> id
  in
  Lwt.try_bind start
    (connection_made self sa ic oc |> Lwt.wrap1)
    (fun exn ->
       Trace.infof ~exn "%s: handshake error" (string_of_sockaddr sa);
       connection_failed self sa;
       Lwt.return_unit)
    
and connect self sa =
  (* Trace.infof "connect: %s" (string_of_sockaddr sa); *)
  Lwt.catch (fun () ->
      Lwt_io.open_connection sa >>= fun (ic, oc) ->
      do_handshake self sa ic oc true)
    (fun exn ->
       connection_failed self ~exn sa;
       Lwt.return_unit) |> ignore
       

let accept self (fd, sa) =
  if H.length self.connections >= max_initiate then begin
    if List.length self.spare < max_initiate &&
       not (List.mem sa self.spare) then begin
      self.spare <- sa :: self.spare;
      Trace.infof "accept: saving %s in the spare set; now have [%d/%d]."
        (string_of_sockaddr sa) (List.length self.spare) max_initiate
    end
  end else
    let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
    Lwt.async (fun () -> do_handshake self sa ic oc false)
 
let got_peer self sa =
  if H.length self.connections >= max_initiate then begin
    if List.length self.spare < max_initiate &&
       not (List.mem sa self.spare) then begin
      self.spare <- sa :: self.spare;
      Trace.infof "got_peer: saving %s in the spare set; now have [%d/%d]."
        (string_of_sockaddr sa) (List.length self.spare) max_initiate
    end
  end else
    connect self sa

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
      connections = H.create 17;
      spare = [] }
  in
  self
