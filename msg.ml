open Printf

type child =
  | Worker of (super_ch:super_msg Lwt_pipe.t -> Proc.Id.t)
  | Supervisor of (super_ch:super_msg Lwt_pipe.t -> (Proc.Id.t * super_msg Lwt_pipe.t))

and super_msg =
  | IAmDying of Proc.Id.t
  | PleaseDie
  | SpawnNew of child

type tracker_msg =
  | Stop
  | TrackerTick of int
  | Start
  | Complete

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
  (** Ask for grabbing some blocks *)
  | GrabBlocks of int * Bits.t * (int * block) list Lwt_mvar.t
  (** Put these blocks back for retrieval *)
  | PutbackBlocks of (int * block) list

type peer_mgr_msg =
  | PeersFromTracker    of peer list
  | NewIncoming         of Lwt_unix.file_descr
  (* | NewTorrent          of Torrent.digest * torrent_local *)
  (* | StopTorrent         of Torrent.digest *)
  | Connect     of Proc.Id.t
  | Disconnect  of Proc.Id.t

type msg_ty =
  | FromPeer    of peer_msg
  | FromSender  of int
  | FromTimer

type sender_msg =
  | SendMsg     of peer_msg
  | SendPiece   of int * block
  | SendCancel  of int * block

type fs_msg =
  | CheckPiece of int * bool Lwt_mvar.t
  | ReadBlock of int * block * string Lwt_mvar.t
  | WriteBlock of int * block * string
