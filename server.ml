let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

module W160H = Hashtbl.Make (Word160)

type t = {
  id : Word160.t;
  info_hash : Word160.t;
  (* info : Info.partial; *)
  handshake : string;
  mutable spare : Unix.sockaddr list;
  stop : unit -> unit;
  connections : Peer.t W160H.t;
  mutable port : int option
}

let failwith_lwt fmt =
  Printf.ksprintf (fun msg -> Lwt.fail (Failure msg)) fmt

let bittorrent_id_prefix = "-OC0001-"    
let unchoking_frequency = 3
let optimistic_unchoke_iterations = 3
let rate_computation_iterations = 2
let max_downloaders_unchoke = 4
let port_range = [6881; 6882; 6883; 6884; 6885; 6886; 6887; 6888; 6889]
let outbound_connections_pool_size = 20
let outbound_connections_thread_keep_alive_secs = 10
let client_keep_alive_minutes = 3
let max_listen_backlog = 5
let max_downloaders_unchoke = 4

let string_of_sockaddr = function
  | Unix.ADDR_UNIX s ->
    s
  | Unix.ADDR_INET (addr, port) ->
    Unix.string_of_inet_addr addr ^ ":" ^ string_of_int port    

let unchoke_peers cl optimistic =  
  let peers = W160H.fold (fun _ p l -> p :: l) cl.connections [] in
  let peers = List.sort (fun p1 p2 -> compare (Peer.dl p2) (Peer.dl p1)) peers in
  let rec loop downloaders choking = function
    | [] -> choking
    | p :: peers ->
      if downloaders >= max_downloaders_unchoke then
        loop downloaders (p :: choking) peers
      else
      if Peer.is_choked p then begin
        Peer.unchoke p;
        if Peer.is_interested p then
          loop (downloaders+1) choking peers
        else
          loop downloaders choking peers
      end else
        loop downloaders choking peers
  in
  let choking = loop 0 [] peers in
  if List.length choking > 0 then
    let n = Random.int (List.length choking) in
    let rec loop i = function
      | [] -> ()
      | p :: peers ->
        begin
          if i = n then begin
            Trace.infof "Optimistically unchoking %s" (Peer.to_string p);
            Peer.unchoke p
          end else
            Peer.choke p
        end;
        loop (i+1) peers
    in
    loop 0 choking

(* let handle_peer_event_leeching info store cl ev = *)
(*   match ev with *)
(*   | Peer.CHOKED pr -> *)
(*     begin match Peer.requested_piece pr with *)
(*     | None -> () *)
(*     | Some i -> Bits.unset cl.requested i *)
(*     end *)
(*   | Peer.READY pr -> *)
(*     let interesting = *)
(*       Bits.logand (Peer.available_pieces pr) *)
(*         (Bits.lognot (Bits.logor cl.requested cl.completed)) *)
(*     in *)
(*     if Bits.count interesting = 0 then begin *)
(*       Trace.infof "Peer %s is ready but does not have any interesting pieces" *)
(*         (Peer.to_string pr) *)
(*     end else begin *)
(*       let rarest = H.pick cl.rarest (Bits.is_set interesting) in *)
(*       let pc = List.nth rarest (Random.int (List.length rarest)) in *)
(*       Bits.set cl.requested pc; *)
(*       Peer.download_piece pr pc *)
(*     end *)
(*   | Peer.GOT_BITFIELD (pr, bits) -> *)
(*     Bits.iteri (fun i b -> if b then H.add cl.rarest i) bits; *)
(*     let interesting = *)
(*       Bits.logand bits *)
(*         (Bits.lognot (Bits.logor cl.requested cl.completed)) *)
(*     in *)
(*     if Bits.count interesting = 0 then *)
(*       Peer.not_interesting pr *)
(*     else *)
(*       Peer.interesting pr; *)
(*     Trace.infof "Received bitfield from %s count: %d interesting: %d \ *)
(*                  completed: %d available: %d total: %d" *)
(*       (Peer.to_string pr) (Bits.count bits) (Bits.count interesting) *)
(*       (Bits.count cl.completed) (-1) (Array.length info.pieces) *)
(*   | Peer.PIECE_SENT (pr, i) -> *)
(*     Trace.infof "#%d uploaded to %s" i (Peer.to_string pr); *)
(*     cl.uploaded <- *)
(*       Int64.add cl.uploaded (Int64.of_int info.pieces.(i).piece_length) *)
(*   | Peer.PIECE_COMPLETED (pr, i, s) -> *)
(*     cl.downloaded <- *)
(*       Int64.add cl.downloaded (Int64.of_int info.pieces.(i).piece_length); *)
(*     if Bits.is_set cl.completed i then begin *)
(*       Trace.infof "Received repeated #%d from %s, ignoring" *)
(*         i (Peer.to_string pr) *)
(*     end else if not (Bits.is_set cl.requested i) then begin *)
(*       Trace.infof "Received unrequested #%d from %s, ignoring" i (Peer.to_string pr) *)
(*     end else begin *)
(*       Bits.unset cl.requested i; *)
(*       if Word160.equal (Word160.digest_of_string s) info.pieces.(i).piece_digest then begin *)
(*         Bits.set cl.completed i; *)
(*         write_piece store info.pieces.(i) s; *)
(*         (\* Store.write_piece cl.store i s; *\) *)
(*         Trace.infof "Received OK #%d from %s. Now have %d/%d pieces" *)
(*           i  (Peer.to_string pr) (Bits.count cl.completed) (Array.length info.pieces) *)
(*       end else *)
(*         Trace.infof "Received BAD #%d from %s, discarding" *)
(*           i (Peer.to_string pr) *)
(*     end *)
(*     (\* check for completion FIXME FIXME *\) *)
(*   | _ -> *)
(*     () *)

let connection_made self sa ic oc (id, exts) : unit =
  Trace.infof "New peer connected with id %s" (Word160.to_hex_short id);
  let _ = Peer.create_with_partial sa id ic oc self.info_hash (fun _ -> ()) exts in
  ()
  (* FIXME save the peer *)

let connection_lost self sa exn =
  ()

let handshake_message id ih =
  "\019BitTorrent protocol" ^ Bits.to_bin Peer.partial_exts ^ Word160.to_bin ih ^ Word160.to_bin id

let read_handshake self sa ic =
  let read_exactly n ic =
    let buf = String.create n in
    Lwt_io.read_into_exactly ic buf 0 n >|= fun () ->
    buf
  in
  Lwt_io.read_char ic >|= int_of_char >>= fun pstrlen ->
  read_exactly pstrlen ic >>= fun proto ->
  if proto = "BitTorrent protocol" then
    read_exactly 8 ic >|= Bits.of_bin >>= fun exts ->
    read_exactly 20 ic >|= Word160.from_bin >>= fun ih ->
    if Word160.equal ih self.info_hash then
      read_exactly 20 ic >|= fun id -> Word160.from_bin id, exts
    else
      failwith_lwt "%s: bad info hash %s" (string_of_sockaddr sa) (Word160.to_hex ih)
  else
    failwith_lwt "%s: unknown protocol %S" (string_of_sockaddr sa) proto

let _start_connection self sa ic oc is_local =
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
    (fun exn -> Trace.infof ~exn "Handshake error"; Lwt.return_unit)

let start_connection self sa =
  let _ =
    Lwt_io.open_connection sa >>= fun (ic, oc) ->
    _start_connection self sa ic oc true
  in
  ()

let start_external_connection self (fd, sa) =
  let ic = Lwt_io.of_fd ~mode:Lwt_io.input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output fd in
  let _ = _start_connection self sa ic oc false in
  ()
    
(* let bytes_left minfo have = *)
(*   Bits.fold_left_i (fun acc i has -> *)
(*     if has then Int64.add acc (Int64.of_int minfo.Info.pieces.(i).Info.piece_length) *)
(*     else acc) 0L have *)

(* let check_data store pieces : Bits.t Lwt.t = *)
(*   let n = Array.length pieces in *)
(*   let completed = Bits.create n in *)
(*   let rec loop i = *)
(*     if i >= n then *)
(*       Lwt.return_unit *)
(*     else begin *)
(*       Store.read store pieces.(i).Info.piece_offset pieces.(i).Info.piece_length >>= fun s -> *)
(*       let digest = Word160.digest_of_string s in *)
(*       if Word160.equal digest pieces.(i).Info.piece_digest then Bits.set completed i; *)
(*       loop (i+1) *)
(*     end *)
(*   in *)
(*   loop 0 >|= fun () -> completed *)
    
let start_server connectfunc stop_thread : int option =
  let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  let rec accept_loop () =
    Lwt.pick [stop_thread >|= (fun () -> `Stop);
              Lwt_unix.accept fd >|= (fun x -> `Accept x)] >>= function
    | `Stop ->
      Lwt.return_unit
    | `Accept x ->
      connectfunc x;
      accept_loop ()
  in
  let rec loop = function
    | [] ->
      Trace.infof "No available port for the BitTorrent client!";
      None
    | port :: rest ->
      let sa = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
      try
        Lwt_unix.bind fd sa;
        Lwt_unix.listen fd max_listen_backlog;
        Trace.infof "Starting peer server at %s" (string_of_sockaddr sa);
        Lwt.async (fun () ->
            Lwt.catch accept_loop
              (fun exn ->
                 Trace.infof ~exn "Server error";
                 Lwt.return_unit));
        Some port
      with
      | exn ->
        Trace.infof ~exn "Server could not bind to %s, trying next port" (string_of_sockaddr sa);
        loop rest
  in
  loop port_range

let create info_hash =
  let id = Word160.peer_id bittorrent_id_prefix in
  let stop_server, wake_stop = Lwt.wait () in
  let stop () = Lwt.wakeup wake_stop () in
  let self =
    { id;
      info_hash;
      (* info = Info.of_info_hash info_hash (fun _ -> exit 0); *)
      handshake = handshake_message id info_hash;
      spare = [];
      stop;
      connections = W160H.create 17;
      port = None }
  in
  self.port <- start_server (start_external_connection self) stop_server;
  self

let port self =
  match self.port with
  | None -> raise Not_found
  | Some port -> port

let id self =
  self.id
