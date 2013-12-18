open Info

type update = {
  name : string;
  completed : int;
  total : int;
  total_length : int64;
  left : int64;
  available : int;
  requested : int;
  connected : int;
  known : int;
  ul : float;
  dl : float
}
  
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

type client_state =
  | WAITING
  | VALIDATING
  | LEECHING
  | SEEDING
  | DONE
  | ERROR

module W160H = Hashtbl.Make (Word160)
module H = Histo.MakeImp (Histo.Int) (Histo.Int)

(* type t = { *)
(*   info : Info.t; *)
(*   announce : Announce.t; *)
(*   mutable state : client_state; *)
(*   mutable pending : Unix.sockaddr list; *)
(*   mutable server : unit Lwt.t; *)
(*   connected : Peer.t W160H.t *)
(* } *)
    
type t = {
  info : Info.t;
  mutable state : client_state;
  info_hash : Word160.t;
  announce : Announce.t;
  connected : Peer.t W160H.t;
  mutable pending : Lwt_unix.sockaddr list;
  mutable server : unit Lwt.t;
  stats : Info.stats;
  rarest : H.t;
  (* completed : Bits.t; *)
  requested : Bits.t;
  store : Store.t;
  mutable handlers : (update -> unit) list
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
  let peers = W160H.fold (fun _ p l -> p :: l) cl.connected [] in
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

let handle_peer_event cl ev =
  match ev with
  | Peer.CHOKED pr ->
    begin match Peer.requested_piece pr with
    | None -> ()
    | Some i -> Bits.unset cl.requested i
    end
  | Peer.READY pr ->
    let interesting =
      Bits.logand (Peer.available_pieces pr)
        (Bits.lognot (Bits.logor cl.requested cl.stats.completed))
    in
    if Bits.count interesting = 0 then begin
      Trace.infof "Peer %s is ready but does not have any interesting pieces"
        (Peer.to_string pr)
    end else begin
      let rarest = H.pick cl.rarest (Bits.is_set interesting) in
      let pc = List.nth rarest (Random.int (List.length rarest)) in
      Bits.set cl.requested pc;
      Peer.download_piece pr pc
    end
  | Peer.GOT_BITFIELD (pr, bits) ->
    Bits.iteri (fun i b -> if b then H.add cl.rarest i) bits;
    let interesting =
      Bits.logand bits
        (Bits.lognot (Bits.logor cl.requested cl.stats.completed))
    in
    if Bits.count interesting = 0 then
      Peer.not_interesting pr
    else
      Peer.interesting pr;
    Trace.infof "Received bitfield from %s count: %d interesting: %d \
                 completed: %d available: %d total: %d"
      (Peer.to_string pr) (Bits.count bits) (Bits.count interesting)
      (Bits.count cl.stats.completed) (-1) (Array.length cl.info.pieces)
  | Peer.PIECE_SENT (pr, i) ->
    Trace.infof "#%d uploaded to %s" i (Peer.to_string pr);
    cl.stats.uploaded <-
      Int64.add cl.stats.uploaded (Int64.of_int cl.info.pieces.(i).piece_length)
  | Peer.PIECE_COMPLETED (pr, i, s) ->
    cl.stats.downloaded <-
      Int64.add cl.stats.downloaded (Int64.of_int cl.info.pieces.(i).piece_length);
    if Bits.is_set cl.stats.completed i then begin
      Trace.infof "Received repeated #%d from %s, ignoring"
        i (Peer.to_string pr)
    end else if not (Bits.is_set cl.requested i) then begin
      Trace.infof "Received unrequested #%d from %s, ignoring" i (Peer.to_string pr)
    end else begin
      Bits.unset cl.requested i;
      if Word160.equal (Word160.digest_of_string s) cl.info.pieces.(i).piece_digest then begin
        Bits.set cl.stats.completed i;
        Store.write_piece cl.store i s;
        Trace.infof "Received OK #%d from %s. Now have %d/%d pieces"
          i  (Peer.to_string pr) (Bits.count cl.stats.completed) (Array.length cl.info.pieces)
      end else
        Trace.infof "Received BAD #%d from %s, discarding"
          i (Peer.to_string pr)
    end
    (* check for completion FIXME FIXME *)
  | _ ->
    ()
      
let peer_connected cl sa ic oc id : unit =
  Trace.infof "New peer connected with id %s" (Word160.to_hex_short id);
  let pr = Peer.create cl.stats sa id ic oc cl.info cl.store in
  Peer.add_handler pr (handle_peer_event cl)

let peer_connect_failed cl sa exn =
  ()

let handshake id ih =
  "\019BitTorrent protocol" ^ String.make 8 '\000' ^ Word160.to_bin ih ^ Word160.to_bin id

let read_and_check_handshake cl sa ic =
  let junk n ic =
    let buf = String.create n in
    Lwt_io.read_into_exactly ic buf 0 n
  in
  let read_exactly n ic =
    let buf = String.create n in
    Lwt_io.read_into_exactly ic buf 0 n >>= fun () ->
    Lwt.return buf
  in
  Lwt_io.read_char ic >|= int_of_char >>= fun pstrlen ->
  read_exactly pstrlen ic >>= fun proto ->
  if proto = "BitTorrent protocol" then
    junk 8 ic >>= fun () ->
    read_exactly 20 ic >|= Word160.from_bin >>= fun ih' ->
    if Word160.equal ih' cl.info_hash then
      read_exactly 20 ic >|= Word160.from_bin
    else
      failwith_lwt "handshake: unknown info hash %s from %s"
        (Word160.to_hex ih') (string_of_sockaddr sa)
  else
    failwith_lwt "handshake: unknown protocol %S" proto

let handle_incoming_peer cl (newfd, sa) : unit =
  let ic = Lwt_io.of_fd ~mode:Lwt_io.input newfd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output newfd in
  let connector () =
    Trace.infof "Incoming connection: %s" (string_of_sockaddr sa);
    Lwt.catch
      (fun () ->
         read_and_check_handshake cl sa ic >>= fun id ->
         Lwt_io.write oc (handshake cl.stats.id cl.info_hash) >|= fun () ->
         Trace.infof "HANDSHAKE successfull peer:%s id:%s"
           (string_of_sockaddr sa) (Word160.to_hex id);
         peer_connected cl sa ic oc id)
      (fun exn ->
         Lwt_io.abort ic >>= fun () ->
         Lwt_io.abort oc >>= fun () ->
         Trace.infof ~exn "HANDSHAKE failure peer:%s" (string_of_sockaddr sa);
         Lwt.return_unit)
  in
  Lwt.async connector
  
let create_server cl : unit Lwt.t =
  let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  let rec loop = function
    | port :: rest ->
      let sa = Unix.ADDR_INET (Unix.inet_addr_any, port) in
      begin
        try
          Lwt_unix.bind fd sa;
          Lwt_unix.listen fd max_listen_backlog;
          Trace.infof "Starting peer server at %s:%d"
            (Unix.string_of_inet_addr Unix.inet_addr_any) port;
          cl.stats.local_port <- port
        with
        | exn ->
          Trace.infof ~exn "Server error: could not bind to %s, trying next port"
            (string_of_sockaddr sa);
          loop rest
      end;
    | [] ->
      failwith "No available port for the BitTorrent client!"
  in
  loop port_range;
  let rec loop () =
    Lwt_unix.accept fd >>= fun pr ->
    handle_incoming_peer cl pr;
    loop ()
  in
  Lwt.catch loop
    (fun exn ->
       Trace.infof ~exn "Server error";
       Lwt.fail exn)  

let connect_peer cl addr port =
  let sa = Unix.ADDR_INET (addr, port) in
  let connector () =
    Lwt_io.open_connection sa >>= fun (ic, oc) ->
    Lwt_io.write oc (handshake cl.stats.id cl.info_hash) >>= fun () ->
    Trace.infof "Handshake sent to %s:%d" (Unix.string_of_inet_addr addr) port;
    Lwt.try_bind
      (fun () -> read_and_check_handshake cl sa ic)
      (fun id ->
         Trace.infof "Handshake success addr:%s port:%d id:%s"
           (Unix.string_of_inet_addr addr) port (Word160.to_hex id);
         peer_connected cl sa ic oc id;
         Lwt.return_unit)
      (fun exn ->
         peer_connect_failed cl sa exn;
         Lwt.return_unit)
  in
  Lwt.catch connector
    (fun exn ->
       Trace.infof ~exn "Handshake failure with %s" (string_of_sockaddr sa);
       Lwt.return_unit)
    
let bytes_left minfo have =
  Bits.fold_left_i (fun acc i has ->
    if has then Int64.add acc (Int64.of_int minfo.pieces.(i).piece_length)
    else acc) 0L have

let create (handles, have) metainfo =
  let stats =
    { downloaded = 0L; uploaded = 0L; left = bytes_left metainfo have; local_port = 0;
      id = Word160.peer_id bittorrent_id_prefix;
      completed = have }
  in
  let cl =
    { info = metainfo; state = WAITING;
      info_hash = metainfo.info_hash;
      announce = Announce.create metainfo stats;
      connected = W160H.create 17;
      server = Lwt.return_unit;
      stats;
      pending = [];
      (* completed = Bits.create (Array.length metainfo.pieces); *)
      requested = Bits.create (Array.length metainfo.pieces);
      rarest = H.create ();
      store = Store.create handles metainfo;
      handlers = [] }
  in
  cl.server <- create_server cl;
  Announce.add_handler cl.announce
    (function
      | Announce.ANN_RESPONSE (Some seeders, Some leechers) ->
        Trace.infof "Received response from tracker: seeders: %d leechers: %d" seeders leechers
      | Announce.ANN_RESPONSE (None, Some leechers) ->
        Trace.infof "Received from tracker: leechers: %d" leechers
      | Announce.ANN_RESPONSE (Some seeders, None) ->
        Trace.infof "Received from tracker: seeders: %d" seeders
      | Announce.ANN_RESPONSE (None, None) ->
        ()
      | Announce.ANN_PEERS peers ->
        Trace.infof "Received %d peers from tracker" (List.length peers);
        List.iter (fun (addr, port) ->
            Lwt.async (fun () -> connect_peer cl addr port)) peers);
  cl

let start cl =
  Announce.start cl.announce

let add_handler cl h =
  cl.handlers <- h :: cl.handlers
