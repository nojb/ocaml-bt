type msg =
  | Tick
  | AddPeer of Proc.Id.t * Msg.msg_ty Lwt_pipe.t
  | RemovePeer of Proc.Id.t
  | PieceCompleted of int

type peer_rate_info = {
  up: float;
  down: float;
  interested: bool;
  choking: bool
}

(* module H : Hashtbl.S with type key = Proc.Id.t *)

val start :
  super_ch: Msg.super_msg Lwt_pipe.t ->
  ch: msg Lwt_pipe.t ->
  peer_rates: peer_rate_info Hashtbl.Make(Proc.Id).t ->
  Proc.Id.t
