type state = {
  uploaded        : int64;
  downloaded      : int64;
  left            : int64;
  incomplete      : int option;
  complete        : int option;
  state           : Info.state
}

type msg =
  [ `UpdateStats         of int option * int option
  | `CompletedPiece      of int
  | `TorrentCompleted
  | `RequestStatus       of state Lwt_mvar.t
  | `Tick ]

val start :
  ch: msg Lwt_pipe.t ->
  info_hash: Info.Digest.t ->
  left: int64 ->
  Proc.Id.t
