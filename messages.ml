open Printf

type torrent_mgr_msg =
  | AddedTorrent of string

type tracker_msg =
  | Stop
  | TrackerTick of int
  | Start
  | Complete

type state = {
  uploaded        : int64;
  downloaded      : int64;
  left            : int64;
  incomplete      : int option;
  complete        : int option;
  state           : Torrent.state
}

type tracker_stats = {
  track_info_hash   : Torrent.digest;
  track_incomplete  : int option;
  track_complete    : int option
}

type status_msg =
  | TrackerStat         of tracker_stats
  | CompletedPiece      of Torrent.digest * int
  | InsertTorrent       of Torrent.digest * int64
  | RemoveTorrent       of Torrent.digest
  | TorrentCompleted    of Torrent.digest
  | RequestStatus       of Torrent.digest * state Lwt_mvar.t
  | RequestAllTorrents  of (Torrent.digest * state) list Lwt_mvar.t
  | StatusTimerTick

type peer =
  Unix.inet_addr * int

type block =
  | Block of int * int

type peer_msg =
  | KeepAlive
  | Choke
  | Unchoke
  | Interested
  | NotInterested
  | Have of int
  | BitField of Bits.t
  | Request of int * block
  | Piece of int * int * string
  | Cancel of int * block
  | Port of int

let string_of_peer_msg = function
  | KeepAlive ->
    "KeepAlive"
  | Choke ->
    "Choke"
  | Unchoke ->
    "Unchoke"
  | Interested ->
    "Interested"
  | NotInterested ->
    "NotInterested"
  | Have index ->
    sprintf "Have: index: %d" index
  | BitField bits ->
    sprintf "BitField: length: %d" (Bits.length bits)
  | Request (index, Block (offset, length)) ->
    sprintf "Request: index: %d offset: %d length: %d" index offset length
  | Piece (index, offset, _) ->
    sprintf "Piece: index: %d offset: %d" index offset
  | Cancel (index, Block (offset, length)) ->
    sprintf "Cancel: index: %d offset: %d length: %d" index offset length
  | Port port ->
    sprintf "Port: %d" port

(* let peer_msg_size = function *)
(*   | KeepAlive -> 4 *)
(*   | Choke *)
(*   | Unchoke *)
(*   | Interested *)
(*   | NotInterested -> 5 *)
(*   | Have _ -> 9 *)
(*   | BitField bits -> 5 + Bits.length (Bits.pad 8 bits) *)
(*   | Request _ -> 17 *)
(*   | Piece (_, _, block) -> String.length block + 13 *)
(*   | Cancel _ -> 17 *)
(*   | Port -> 7 *)

type piece_mgr_msg =
  | GrabBlocks of int * Bits.t * (int * block) list Lwt_mvar.t
  | PutbackBlocks of (int * block) list

type torrent_local = {
  msg_piece_mgr : piece_mgr_msg -> unit;
  pieces : Torrent.piece_info array
}

type peer_mgr_msg =
  | PeersFromTracker    of Torrent.digest * peer list
  | NewIncoming         of Lwt_unix.file_descr
  | NewTorrent          of Torrent.digest * torrent_local
  | StopTorrent         of Torrent.digest

type mgr_msg =
  | Connect     of Torrent.digest * Monitor.id
  | Disconnect  of Monitor.id

type msg_ty =
  | FromPeer    of peer_msg
  | FromSender  of int
  | FromTimer

type sender_msg =
  | SendMsg     of peer_msg
  | SendPiece   of int * block
  | SendCancel  of int * block
