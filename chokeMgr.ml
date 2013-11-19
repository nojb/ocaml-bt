let (>>=) = Lwt.(>>=)

let debug = Proc.debug

type msg =
  | Tick
  | AddPeer of Proc.Id.t * Msg.msg_ty Lwt_pipe.t
  | RemovePeer of Proc.Id.t
  | PieceCompleted of int

let string_of_msg = function
  | Tick ->
    "Tick"
  | AddPeer (id, _) ->
    Printf.sprintf "AddPeer: id: %s" (Proc.Id.to_string id)
  | RemovePeer id ->
    Printf.sprintf "RemovePeer: id: %s" (Proc.Id.to_string id)
  | PieceCompleted index ->
    Printf.sprintf "PieceCompleted: index: %d" index

type peer_rate_info = {
  up: float;
  down: float;
  interested: bool;
  choking: bool
}

module H = Hashtbl.Make (Proc.Id)

type peer_info = {
  peer_ch : Msg.msg_ty Lwt_pipe.t;
}

type t = {
  peers : peer_info H.t;
  peer_rates : peer_rate_info H.t;
  id : Proc.Id.t
}

(* let unchoke_peers t optimistic = *)
(*   let donwnloaders = ref 0 in *)
(*   let *)
(*     elected, nonelected = *)
(*       M.partition (fun _ pi -> *)
(*         if !downlaoders < max_downloaders_unchoke then *)
(*           if pi.choking then begin *)
(*             if pi.interested then incr downloaders; *)
(*             true *)
(*           end else false *)
(*         else *)
(*           false) p.peers *)
(*   in *)
(*   M.iter (fun _ pi -> Lwt_pipe.write pi.pch (Unchoke)) elected; *)
(*  *)
(*  *)
(*   try *)
(*     M.iter (fun id pi -> *)
(*       if !downloaders < max_downloaders_unchoke then *)
(*         if pi.choking then begin *)
(*           if pi.is_interested then incr downloaders; *)
(*           Lwt_pipe.write pi.pch (UnchokePeer) *)
(*         else *)
(*           ...) t.peers *)
(*  *)

let handle_message t msg =
  debug t.id "%s" (string_of_msg msg) >>= fun () ->
  match msg with
  (* | Tick -> *)
  (*   unchoke_peers t *)
  | AddPeer (id, ch) ->
    H.add t.peers id { peer_ch = ch };
    Lwt.return_unit
  | RemovePeer (id) ->
    H.remove t.peers id;
    H.remove t.peer_rates id;
    Lwt.return_unit
  | PieceCompleted index ->
    H.iter (fun _ pi -> Lwt_pipe.write pi.peer_ch (Msg.PieceCompleted index))
      t.peers;
    Lwt.return_unit
  | msg ->
    debug t.id "Unhandled: %s" (string_of_msg msg)

let start ~super_ch ~ch ~peer_rates =
  let run id =
    let t =
      { peer_rates; peers = H.create 17; id }
    in
    Lwt_pipe.iter_s (handle_message t) ch
  in
  Proc.spawn ~name:"ChokeMgr" run (Super.default_stop super_ch)
