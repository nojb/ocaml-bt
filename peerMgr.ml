open Printf
open Msg

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let debug = Proc.debug

let failwith_lwt msg = raise_lwt (Failure msg)

module M = Map.Make (Proc.Id)

let string_of_msg = function
  | PeersFromTracker peers ->
    sprintf "PeersFromTracker: count: %d" (List.length peers)
  | NewIncoming _ ->
    "NewIncoming"
  | Connect tid ->
    sprintf "Connect: id: %s" (Proc.Id.to_string tid)
  | Disconnect tid ->
    sprintf "Disconnect: id: %s" (Proc.Id.to_string tid)

type t = {
  mutable pending_peers : peer list;
  pieces : Info.piece_info array;
  peers : unit M.t;
  peer_id : Info.peer_id;
  info_hash : Info.Digest.t;
  piece_mgr_ch : PieceMgr.msg Lwt_pipe.t;
  ch : Msg.peer_mgr_msg Lwt_pipe.t;
  pool_ch : Msg.super_msg Lwt_pipe.t;
  id : Proc.Id.t
}

let max_peers = 40

let junk n ic =
  let buf = String.create n in
  Lwt_io.read_into_exactly ic buf 0 n

let recv_handshake ic ih : Info.peer_id Lwt.t =
  lwt pstrlen = Lwt_io.read_char ic in
  let pstrlen = int_of_char pstrlen in
  let buf = String.create pstrlen in
  (* debug t.id "received pstrlen: %d" pstrlen >> *)
  Lwt_io.read_into_exactly ic buf 0 pstrlen >>= fun () ->
  (* debug t.id "received pstr: %S" buf >> *)
  if buf = "BitTorrent protocol" then
    junk 8 ic >>
    lwt ih' = Info.Digest.of_input_channel ic in
    if Info.Digest.equal ih' ih then
      Info.PeerId.of_input_channel ic
    else
      failwith_lwt "bad info hash"
  else
    failwith_lwt "bad protocol"

let handshake_message peer_id ih =
  "\019BitTorrent protocol" ^ String.make 8 '\000' ^
  (Info.Digest.to_bin ih) ^ (Info.PeerId.to_string peer_id)

let handshake id peer_id ic oc ih =
  debug id "Sending handshake message" >>= fun () ->
  Lwt_io.write oc (handshake_message peer_id ih) >>= fun () ->
  (* debug t.id "Waiting to hear back from the other side" >>= fun () -> *)
  recv_handshake ic ih

let handle_good_handshake t id ic oc peer_id : unit Lwt.t =
  debug id "good handshake received from: %S"
    (Info.PeerId.to_string peer_id) >>= fun () ->
  let pieces = t.pieces in
  let piece_mgr_ch = t.piece_mgr_ch in
  let children =
    Peer.start ic oc ~peer_mgr_ch:t.ch t.info_hash ~pieces ~piece_mgr_ch
  in
  let start_peer_sup =
    let ch = Lwt_pipe.create () in
    Super.start Super.AllForOne "PeerSup" ~children ~ch
  in
  Lwt_pipe.write t.pool_ch (SpawnNew (Supervisor start_peer_sup));
  Lwt.return_unit

let connect t (addr, port) =
  let connector id =
    debug id "Connecting to %s:%d" (Unix.string_of_inet_addr addr) port >>
    lwt ic, oc = Lwt_io.open_connection (Unix.ADDR_INET (addr, port)) in
    debug id "Connected to %s:%d, initiating handshake"
      (Unix.string_of_inet_addr addr) port >>
    handshake id t.peer_id ic oc t.info_hash >>=
    handle_good_handshake t id ic oc
  in
  Proc.async ~name:"Connector" connector

let add_peer t paddr : unit =
  connect t paddr

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
  if no_peers < max_peers then
    let to_add, rest =
      list_split (max_peers-no_peers) t.pending_peers in
    List.iter (add_peer t) to_add;
    t.pending_peers <- rest;
    Lwt.return_unit
  else
    Lwt.return_unit

let handle_message t msg : unit Lwt.t =
  debug t.id "%s" (string_of_msg msg) >>= fun () ->
  match msg with
  | PeersFromTracker peers ->
    t.pending_peers <- t.pending_peers @ peers;
    fill_peers t
  | msg ->
    debug t.id "Unhandled: %s" (string_of_msg msg)

let start ~super_ch ~ch ~peer_id ~info_hash ~piece_mgr_ch ~pieces =
  (* FIXME register pool thread with my supervisor so that
   * it gets automatically finished when the supervisor goes away *)
  let run id =
    let pool_ch = Lwt_pipe.create () in
    let start_pool =
      Super.start Super.OneForOne "PeerMgrPool" ~children:[] ~ch:pool_ch
    in
    Lwt_pipe.write super_ch (SpawnNew (Supervisor start_pool));
    let t =
      { pending_peers = [];
        peers = M.empty;
        peer_id;
        pieces;
        info_hash;
        piece_mgr_ch;
        ch;
        pool_ch;
        id }
    in
    Lwt_pipe.iter_s (handle_message t) ch
  in
  Proc.spawn ~name:"PeerMgr" run (Super.default_stop super_ch)
    (fun _ -> Lwt.return_unit)
