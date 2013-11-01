open Printf
open Messages
open Monitor

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

module M = Map.Make (ThreadId)

let string_of_peer_msg = function
  | PeersFromTracker (ih, peers) ->
    sprintf "PeersFromTracker: info_hash: %s count: %d"
      (Torrent.Digest.to_string ih)
      (List.length peers)
  | NewIncoming _ ->
    "NewIncoming"
  | NewTorrent ih ->
    sprintf "NewTorrent: info_hash: %s" (Torrent.Digest.to_string ih)
  | StopTorrent ih ->
    sprintf "StopTorrent: info_hash: %s" (Torrent.Digest.to_string ih)

let string_of_mgr_msg = function
  | Connect (ih, tid) ->
    sprintf "Connect: info_hash: %s id: %s"
      (Torrent.Digest.to_string ih)
      (ThreadId.to_string tid)
  | Disconnect tid ->
    sprintf "Disconnect: id: %s" (ThreadId.to_string tid)

type msg_source =
  | FromPeer of mgr_msg
  | PeerMgrMsg of peer_mgr_msg

let string_of_msg = function
  | FromPeer msg ->
    sprintf "FromPeer: %s" (string_of_mgr_msg msg)
  | PeerMgrMsg msg ->
    string_of_peer_msg msg

type state = {
  pending_peers : (Torrent.digest * peer) list;
  peers : unit M.t;
  peer_id : Torrent.peer_id;
  (* chan : msg_source Lwt_stream.t; *)
  w_mgr_ch : mgr_msg -> unit;
  pool : Monitor.t
}

let max_peers = ref 40

let junk n ic =
  let buf = String.create n in
  Lwt_io.read_into_exactly ic buf 0 n

exception BadHandShake of string

let recv_handshake id st ic ih : Torrent.peer_id Lwt.t =
  try_lwt
    lwt pstrlen = Lwt_io.read_char ic in
    let pstrlen = int_of_char pstrlen in
    let buf = String.create pstrlen in
    debug id "received pstrlen: %d" pstrlen >>
    Lwt_io.read_into_exactly ic buf 0 pstrlen >>
    debug id "received pstr: %S" buf >>
    if buf = "BitTorrent protocol" then
      junk 8 ic >>
      lwt ih' = Torrent.Digest.of_input_channel ic in
      if Torrent.Digest.equal ih' ih then
        Torrent.PeerId.of_input_channel ic
      else
        raise_lwt (BadHandShake "unknown info hash")
    else
      raise_lwt (BadHandShake "wrong protocol")
  with
  | End_of_file -> raise_lwt (BadHandShake "unexpected end of file")

let handshake_message peer_id ih =
  "\019BitTorrent protocol" ^ String.make 8 '\000' ^
  (Torrent.Digest.to_bin ih) ^ (Torrent.PeerId.to_string peer_id)

let handshake id st ic oc ih =
  debug id "Sending handshake message" >>
  lwt () = Lwt_io.write oc (handshake_message st.peer_id ih) in
  debug id "Waiting to hear back from the other side" >>
  recv_handshake id st ic ih

let handle_good_handshake id st ic oc ih peer_id : unit Lwt.t =
  debug id "good handshake received from: %S"
    (Torrent.PeerId.to_string peer_id) >>= fun () ->
  Peer.start st.pool ic oc st.w_mgr_ch ih;
  (* let children = Peer.start ic oc st.w_mgr_ch ih in *)
  (* st.w_pool (SpawnNew (Supervisor *)
  (*   (Supervisor.start ~policy:AllForOne ~name:"PeerSup" ~children))); *)
  Lwt.return ()

let connect id st (addr, port) ih =
  let connector id =
    debug id "Connecting to %s:%d" (Unix.string_of_inet_addr addr) port >>
    lwt ic, oc = Lwt_io.open_connection (Unix.ADDR_INET (addr, port)) in
    debug id "Connected to %s:%d, initiating handshake"
      (Unix.string_of_inet_addr addr) port >>
    handshake id st ic oc ih >>= handle_good_handshake id st ic oc ih
  in
  Monitor.spawn ~name:"Connector" connector

let add_peer id st (ih, paddr) : unit =
  connect id st paddr ih

let list_split n xs =
  let n' = min (List.length xs) n in
  let rec loop i xs =
    if i >= n' then [], xs
    else
      let ys, xs' = loop (i+1) (List.tl xs)
      in List.hd xs :: ys, xs'
  in loop 0 xs

let fill_peers id st : state Lwt.t =
  let no_peers = M.cardinal st.peers in
  if no_peers < !max_peers then
    let to_add, rest =
      list_split (!max_peers - no_peers) st.pending_peers in
    List.iter (add_peer id st) to_add;
    Lwt.return {st with pending_peers = rest}
  else
    Lwt.return st

let handle_message id msg st : state Lwt.t =
  (* lwt msg = Lwt_stream.next st.chan in *)
  debug id "%s" (string_of_msg msg) >>
  match msg with
  | PeerMgrMsg (PeersFromTracker (ih, peers)) ->
    let st = {st with pending_peers = st.pending_peers @
      List.map (fun p -> (ih, p)) peers} in
    fill_peers id st
  | _ ->
    fill_peers id st

let start ~monitor ~peer_mgr_ch ~mgr_ch ~w_mgr_ch ~peer_id =
  (* supervisor_ch w_supervisor_ch = *)
  let chan = Lwt_stream.choose [
    Lwt_stream.map (fun msg -> PeerMgrMsg msg) peer_mgr_ch;
    Lwt_stream.map (fun msg -> FromPeer msg) mgr_ch
  ]
  in
  (* FIXME register pool thread with my supervisor so that
   * it gets automatically finished when the supervisor goes away *)
  let pool = Monitor.create ~parent:monitor Monitor.OneForOne "PeerMgrPool"
  in
  let st = {
    pending_peers = [];
    peers = M.empty;
    peer_id;
    (* chan; *)
    w_mgr_ch;
    pool }
  in
  let event_loop id =
    Lwt_stream.fold_s (handle_message id) chan st >>= fun _ ->
    Lwt.return ()
  in
  Monitor.spawn ~parent:monitor ~name:"PeerMgr" event_loop
