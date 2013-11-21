type msg =
  | Tick
  | AddPeer of Proc.Id.t * Msg.msg_ty Lwt_pipe.t
  | RemovePeer of Proc.Id.t
  | PieceCompleted of int
  | PeerChoked of Proc.Id.t
  | PeerUnChoked of Proc.Id.t
  | PeerInterested of Proc.Id.t
  | PeerNotInterested of Proc.Id.t
  | PeerRateUpdate of Proc.Id.t * float * float

val start :
  super_ch: Msg.super_msg Lwt_pipe.t ->
  ch: msg Lwt_pipe.t ->
  Proc.Id.t
