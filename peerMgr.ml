open Printf
open Msg

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let debug id ?exn fmt =
  Printf.ksprintf (fun msg ->
    Lwt_log.debug_f ?exn "PeerMgr %s: %s" (Proc.Id.to_string id) msg) fmt

module M = Map.Make (Proc.Id)

let string_of_msg = function
  | PeersFromTracker (ih, peers) ->
    sprintf "PeersFromTracker: info_hash: %s count: %d"
      (Torrent.Digest.to_string ih)
      (List.length peers)
  | NewIncoming _ ->
    "NewIncoming"
  | NewTorrent (ih, _) ->
    sprintf "NewTorrent: info_hash: %s" (Torrent.Digest.to_string ih)
  | StopTorrent ih ->
    sprintf "StopTorrent: info_hash: %s" (Torrent.Digest.to_string ih)
  | Connect (ih, tid) ->
    sprintf "Connect: info_hash: %s id: %s"
      (Torrent.Digest.to_string ih)
      (Proc.Id.to_string tid)
  | Disconnect tid ->
    sprintf "Disconnect: id: %s" (Proc.Id.to_string tid)

module TorrentTbl = Hashtbl.Make (Torrent.Digest)

type t = {
  torrents : Msg.torrent_local TorrentTbl.t;
  mutable pending_peers : (Torrent.digest * peer) list;
  peers : unit M.t;
  peer_id : Torrent.peer_id;
  ch : Msg.peer_mgr_msg Lwt_pipe.t;
  pool_ch : Msg.super_msg Lwt_pipe.t;
  id : Proc.Id.t
}

let max_peers = ref 40

let junk n ic =
  let buf = String.create n in
  Lwt_io.read_into_exactly ic buf 0 n

exception BadHandShake of string
exception InfoHashNotFound of Torrent.Digest.t

let recv_handshake t ic ih : Torrent.peer_id Lwt.t =
  try_lwt
    lwt pstrlen = Lwt_io.read_char ic in
    let pstrlen = int_of_char pstrlen in
    let buf = String.create pstrlen in
    (* debug t.id "received pstrlen: %d" pstrlen >> *)
    Lwt_io.read_into_exactly ic buf 0 pstrlen >>= fun () ->
    (* debug t.id "received pstr: %S" buf >> *)
    if buf = "BitTorrent protocol" then
      junk 8 ic >>
      lwt ih' = Torrent.Digest.of_input_channel ic in
      if Torrent.Digest.equal ih' ih then
        Torrent.PeerId.of_input_channel ic
      else
        raise_lwt (BadHandShake "bad info hash")
    else
      raise_lwt (BadHandShake "wrong protocol")
  with
  | End_of_file -> raise_lwt (BadHandShake "unexpected end of file")

let handshake_message peer_id ih =
  "\019BitTorrent protocol" ^ String.make 8 '\000' ^
  (Torrent.Digest.to_bin ih) ^ (Torrent.PeerId.to_string peer_id)

let handshake t ic oc ih =
  debug t.id "Sending handshake message" >>= fun () ->
  Lwt_io.write oc (handshake_message t.peer_id ih) >>= fun () ->
  (* debug t.id "Waiting to hear back from the other side" >>= fun () -> *)
  recv_handshake t ic ih

let handle_good_handshake t ic oc ih peer_id : unit Lwt.t =
  debug t.id "good handshake received from: %S"
    (Torrent.PeerId.to_string peer_id) >>= fun () ->
  try_lwt
    let tl = TorrentTbl.find t.torrents ih in
    let pieces = tl.Msg.pieces in
    let piece_mgr_ch = tl.Msg.piece_mgr_ch in
    let children = Peer.start ic oc ~peer_mgr_ch:t.ch ih ~pieces ~piece_mgr_ch in
    let start_peer_sup =
      let ch = Lwt_pipe.create () in
      Super.start Super.AllForOne "PeerSup" ~children ~ch
    in
    Lwt_pipe.write t.pool_ch (SpawnNew (Supervisor start_peer_sup));
    Lwt.return_unit
  with
  | Not_found ->
    raise_lwt (InfoHashNotFound ih)

let connect t (addr, port) ih =
  let connector id =
    try_lwt
    debug id "Connecting to %s:%d" (Unix.string_of_inet_addr addr) port >>
    lwt ic, oc = Lwt_io.open_connection (Unix.ADDR_INET (addr, port)) in
    debug id "Connected to %s:%d, initiating handshake"
      (Unix.string_of_inet_addr addr) port >>
    handshake t ic oc ih >>= handle_good_handshake t ic oc ih
    with
    | exn ->
      debug t.id ~exn "Connector failed on exception"
  in
  ignore (Proc.spawn connector)

let add_peer t (ih, paddr) : unit =
  connect t paddr ih

let list_split n xs =
  let n' = min (List.length xs) n in
  let rec loop i xs =
    if i >= n' then [], xs
    else
      let ys, xs' = loop (i+1) (List.tl xs)
      in List.hd xs :: ys, xs'
  in loop 0 xs

let fill_peers t : unit Lwt.t =
  let no_peers = M.cardinal t.peers in
  if no_peers < !max_peers then
    let to_add, rest =
      list_split (!max_peers - no_peers) t.pending_peers in
    List.iter (add_peer t) to_add;
    t.pending_peers <- rest;
    Lwt.return_unit
  else
    Lwt.return_unit

let handle_message t msg : unit Lwt.t =
  debug t.id "%s" (string_of_msg msg) >>= fun () ->
  match msg with
  | PeersFromTracker (ih, peers) ->
    t.pending_peers <- t.pending_peers @ List.map (fun p -> (ih, p)) peers;
    fill_peers t
  | NewTorrent (ih, tl) ->
    TorrentTbl.add t.torrents ih tl;
    Lwt.return_unit
  | msg ->
    debug t.id "Unhandled: %s" (string_of_msg msg)

let start ~super_ch ~ch ~peer_id =
  (* FIXME register pool thread with my supervisor so that
   * it gets automatically finished when the supervisor goes away *)
  let run id =
    let pool_ch = Lwt_pipe.create () in
    let start_pool =
      Super.start Super.OneForOne "PeerMgrPool" ~children:[] ~ch:pool_ch
    in
    Lwt_pipe.write super_ch (SpawnNew (Supervisor start_pool));
    let t =
      { torrents = TorrentTbl.create 17;
        pending_peers = [];
        peers = M.empty;
        peer_id;
        ch;
        pool_ch;
        id }
    in
    Lwt_pipe.iter_s (handle_message t) ch
  in
  Proc.spawn (Proc.cleanup run
    (Super.default_stop super_ch) (fun _ -> Lwt.return_unit))
