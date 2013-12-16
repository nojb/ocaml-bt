open Printf

(* type child = *)
(*   | Worker of (super_ch:super_msg Lwt_pipe.t -> Proc.Id.t) *)
(*   | Supervisor of (super_ch:super_msg Lwt_pipe.t -> (Proc.Id.t * super_msg Lwt_pipe.t)) *)

(* and super_msg = *)
(*   | IAmDying of Proc.Id.t *)
(*   | PleaseDie *)
(*   | SpawnNew of child *)

(* type peer = *)
(*   Unix.inet_addr * int *)

type msg_ty =
  | PeerMsg of Wire.msg
  | BytesSent of int
  | PieceCompleted of int
  | ReceiverAborted of exn
  | SenderAborted of exn
  | Tick
  | Choke
  | UnChoke

(* type peer_mgr_msg = *)
(*   | PeersFromTracker    of peer list *)
(*   | NewIncoming         of Lwt_unix.file_descr *)
(*   (\* | NewTorrent          of Torrent.digest * torrent_local *\) *)
(*   (\* | StopTorrent         of Torrent.digest *\) *)
(*   | Connect     of Proc.Id.t * msg_ty Lwt_pipe.t *)
(*   | Disconnect  of Proc.Id.t *)

type sender_msg =
  | SendMsg     of Wire.msg
  | SendPiece   of int * Wire.block
  | SendCancel  of int * Wire.block
