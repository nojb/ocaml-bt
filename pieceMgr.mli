type msg =
  (** Ask for grabbing some blocks *)
  [ `GrabPiece of Bits.t * int option Lwt_mvar.t
  (** Put these blocks back for retrieval *)
  | `PutbackPiece of int
  (** A peer has announce that it has a set of pieces *)
  | `PeerHave of int list * bool Lwt_mvar.t
  (** Get the bitset of pieces which are done *)
  | `GetDone of Bits.t Lwt_mvar.t
  (** A complete piece has been received from a peer *)
  | `PieceReceived of int * string ]

val start :
  super_ch: Msg.super_msg Lwt_pipe.t ->
  ch: msg Lwt_pipe.t ->
  fs_ch: Fs.msg Lwt_pipe.t ->
  choke_mgr_ch: ChokeMgr.msg Lwt_pipe.t ->
  status_ch: Status.msg Lwt_pipe.t ->
  have: Bits.t ->
  pieces: Info.piece_info array ->
  info_hash: Info.Digest.t ->
  Proc.Id.t
