type event =
  | NOTHING

let max_connections = 5
    
type client_status =
  | WAITING
  | INITIALIZING of Info.t
  | LEECHING of Info.t * Torrent.t * Picker.t
  | SEEDING of Info.t * Torrent.t
  | DONE
  | ERROR of exn

module PeerS = Set.Make (struct type t = Unix.sockaddr let compare = compare end)
module H = Hashtbl
    
type t = {
  id : Word160.t;
  info_hash : Word160.t;
  conn : Connector.t;
  server : Server.t;
  mutable status : client_status;
  mutable peers : PeerS.t;
  mutable connecting : PeerS.t;
  connected : (Unix.sockaddr, Peer.peer) H.t;
  on_event : event -> unit
}

let string_of_sockaddr = function
  | Unix.ADDR_INET (a, p) ->
    Printf.sprintf "%s:%d"
      (Unix.string_of_inet_addr a) p
  | Unix.ADDR_UNIX s ->
    s

let bittorrent_id_prefix = "-OC0001-"

let down self =
  match self.status with
  | LEECHING (_, t, _)
  | SEEDING (_, t) ->
    Torrent.down t
  | _ ->
    0L

let up self =
  match self.status with
  | LEECHING (_, t, _)
  | SEEDING (_, t) ->
    Torrent.up t
  | _ ->
    0L

let amount_left self =
  match self.status with
  | LEECHING (_, t, _)
  | SEEDING (_, t) ->
    Torrent.amount_left t
  | _ ->
    0L

let disconnect_all_peers self =
  H.iter (fun _ p -> p#stop) self.connected;
  H.clear self.connected

let connect self sa =
  if not (H.mem self.connected sa || PeerS.mem sa self.connecting) then begin
    self.connecting <- PeerS.add sa self.connecting;
    Connector.connect self.conn sa
  end

let fill_peers self =
  let rec loop s =
    if H.length self.connected >= max_connections || PeerS.is_empty s then ()
    else begin
      let sa = PeerS.choose s in
      connect self sa;
      loop (PeerS.remove sa s)
    end
  in
  loop self.peers

let got_info self info =
  (* let (>>=) = Lwt.(>>=) in *)
  let (>|=) = Lwt.(>|=) in
  (* Trace.infof "got_info"; *)
  match self.status with
  | WAITING ->
    disconnect_all_peers self;
    self.status <- INITIALIZING info;
    Lwt.catch (fun () ->
        Torrent.create info >|= fun t ->
        (* Trace.infof "Torrent initialization complete: have %d/%d pieces" *)
        (*   (Torrent.numgot t) (Array.length info.Info.pieces); *)
        let p = Picker.create (Array.length info.Info.pieces) in
        for i = 0 to Array.length info.Info.pieces - 1 do
          if Torrent.has_piece t i then Picker.got_piece p i
        done;
        self.status <- LEECHING (info, t, p);
        fill_peers self)
      (fun exn ->
         Trace.infof ~exn "Torrent initialisation error";
         self.status <- ERROR exn;
         Lwt.return_unit) |> ignore
  | _ ->
    ()

let on_dead self sa =
  Trace.infof "Connection to %s closed" (string_of_sockaddr sa);
  self.peers <- PeerS.remove sa self.peers;
  fill_peers self

let on_connector_event self = function
  | `CONNECTED (fd, (id, exts)) ->
    Trace.infof "%s: %s: connected" (string_of_sockaddr (Lwt_unix.getpeername fd))
      (Word160.to_hex id);
    self.connecting <- PeerS.remove (Lwt_unix.getpeername fd) self.connecting;
    begin match self.status with
      | WAITING ->
        new Peer.info_peer fd self.info_hash (got_info self) (id, exts) (on_dead self) |>
        H.add self.connected (Lwt_unix.getpeername fd)
      | LEECHING (i, t, p) ->
        (new Peer.sharing fd self.info_hash i p t (id, exts) (on_dead self) :> Peer.peer) |>
        H.add self.connected (Lwt_unix.getpeername fd)
      | SEEDING (i, t) ->
        assert false
      | DONE
      | INITIALIZING _
      | ERROR _ ->
        ()
    end
  | `FAILED (sa, exn) ->
    Trace.infof ?exn "%s: connection failed" (string_of_sockaddr sa);
    self.connecting <- PeerS.remove sa self.connecting;
    self.peers <- PeerS.remove sa self.peers;
    fill_peers self
      (* FIXME should leave them with a CANTCONNECT flag so that
I do not re-add them from the tracker/dht, etc. *)

(* FIXME should not connect if torrent isnot initialised *)
let got_peer self sa =
  (* Trace.infof "got_peer: %s" (string_of_sockaddr sa); *)
  if PeerS.mem sa self.peers then
    Trace.infof "%s: discarding repeated peer" (string_of_sockaddr sa)
  else begin
    self.peers <- PeerS.add sa self.peers;
    (* if H.mem self.connected sa then () *)
    match self.status with
    | WAITING
    | LEECHING _ ->
      if H.length self.connected < max_connections then begin
        connect self sa
        (* self.connecting <- PeerS.add sa self.connecting; *)
        (* Connector.connect self.conn sa *)
      end
    | _ ->
      ()
  end

let create info_hash trs on_event =
  let id = Word160.peer_id bittorrent_id_prefix in
  let conn = Connector.create ~id ~info_hash in
  let server = Server.create (Connector.accept conn) in
  let self =
    { id;
      conn;
      server;
      info_hash;
      peers = PeerS.empty;
      connected = H.create 17;
      connecting = PeerS.empty;
      status = WAITING;
      on_event }
  in
  Connector.on_event self.conn (on_connector_event self);
  let a =
    Announce.create info_hash (List.map (fun x -> [x]) trs)
      (fun () -> up self) (fun () -> down self) (fun () -> amount_left self)
      (Server.port server) id (got_peer self)
  in
  (* Announce.on_got_peer a (got_peer self); *)
  self

(* let download magnet = *)
(*   let id = new_id () in *)
(*   let mgn = Magnet.of_string s in *)
(*   let chk = Choker.create ... in *)
(*   let pkr = Picker.create ... in *)
(*   let ctr = Connector.create id info_hash in *)
(*   let dht = Dht.create (Connector.got_peer ctr) in *)
(*   let svr = Server.create id info_hash (Connector.got_incoming_connection ctr) in *)
(*   let str = Store.create info_hash (Connector.store_initialised ctr) in *)
(*   let ann = *)
(*     Announcer.create id (Torrent.up sto) (Torrent.down sto) *)
(*       (Torrent.amount_left sto) (Server.port sto) (Connector.got_peer ctr) *)
(*   in *)
