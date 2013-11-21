let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let debug = Proc.debug

type msg =
  | Tick
  | AddPeer of Proc.Id.t * Msg.msg_ty Lwt_pipe.t
  (** Peer is connected *)
  | RemovePeer of Proc.Id.t
  (** Peer has disconnected *)
  | PieceCompleted of int
  (** Piece was verified and comitted to disk *)
  | PeerChoked of Proc.Id.t
  (** Peer was choked by us *)
  | PeerUnChoked of Proc.Id.t
  (** Peer was unchoked by us *)
  | PeerInterested of Proc.Id.t
  (** Peer is interestd in us *)
  | PeerNotInterested of Proc.Id.t
  (** Peer is not interested in us *)
  | PeerRateUpdate of Proc.Id.t * float * float
  (** Peer is reporting UL/DL rates *)

let string_of_msg = function
  | Tick ->
    "Tick"
  | AddPeer (id, _) ->
    Printf.sprintf "AddPeer: id: %s" (Proc.Id.to_string id)
  | RemovePeer id ->
    Printf.sprintf "RemovePeer: id: %s" (Proc.Id.to_string id)
  | PieceCompleted index ->
    Printf.sprintf "PieceCompleted: index: %d" index
  | PeerChoked id ->
    Printf.sprintf "PeerChoked: %s" (Proc.Id.to_string id)
  | PeerUnChoked id ->
    Printf.sprintf "PeerUnChoked: %s" (Proc.Id.to_string id)
  | PeerInterested id ->
    Printf.sprintf "PeerInterested: %s" (Proc.Id.to_string id)
  | PeerNotInterested id ->
    Printf.sprintf "PeerNotInterested: %s" (Proc.Id.to_string id)
  | PeerRateUpdate (id, ul, dl) ->
    Printf.sprintf "PeerRateUpdate: id: %s up: %f down: %f"
      (Proc.Id.to_string id) ul dl

module H = Hashtbl.Make (Proc.Id)

type peer_info = {
  peer_ch : Msg.msg_ty Lwt_pipe.t;
  peer_id : Proc.Id.t;
  mutable ul_rate : float;
  mutable dl_rate : float;
  mutable we_choking : bool;
  mutable interested_in_us : bool
}

type t = {
  peers : peer_info H.t;
  id : Proc.Id.t;
  ticker : unit Lwt.t
}

let max_downloaders_unchoke = 4

let unchoke_peers t optimistic =
  let peers = H.fold (fun _ p l -> p :: l) t.peers [] in
  let peers = List.sort (fun p1 p2 -> compare p2.dl_rate p1.dl_rate) peers in
  let rec loop downloaders choking = function
    | [] -> choking
    | p :: peers ->
      if downloaders >= max_downloaders_unchoke then
        loop downloaders (p :: choking) peers
      else
        if p.we_choking then begin
          Lwt_pipe.write p.peer_ch Msg.UnChoke;
          if p.interested_in_us then
            loop (downloaders+1) choking peers
          else
            loop downloaders choking peers
        end else
          loop downloaders choking peers
  in
  let choking = loop 0 [] peers in
  if List.length choking > 0 then
    let n = Random.int (List.length choking) in
    let rec loop i = function
      | [] -> ()
      | p :: peers ->
        begin if i = n then begin
          ignore (debug t.id "Optimistically UnChoking %s"
            (Proc.Id.to_string p.peer_id));
          Lwt_pipe.write p.peer_ch Msg.UnChoke
        end else
          Lwt_pipe.write p.peer_ch Msg.Choke
        end;
        loop (i+1) peers
    in
    loop 0 choking

let adjust_peer t id f =
  try
    let p = H.find t.peers id in
    f p
  with
  | Not_found ->
    ignore (debug t.id "Trying to adjust a non-existent peer: %s"
      (Proc.Id.to_string id))

let handle_message t msg =
  debug t.id "%s" (string_of_msg msg) >|= fun () ->
  match msg with
  | Tick ->
    unchoke_peers t true
  | AddPeer (id, ch) ->
    H.add t.peers id
      { peer_ch = ch;
        peer_id = id;
        ul_rate = 0.0;
        dl_rate = 0.0;
        we_choking = true;
        interested_in_us = false }
  | RemovePeer (id) ->
    H.remove t.peers id
  | PieceCompleted index ->
    H.iter (fun _ pi -> Lwt_pipe.write pi.peer_ch (Msg.PieceCompleted index))
      t.peers
  | PeerChoked id ->
    adjust_peer t id (fun p -> p.we_choking <- true)
  | PeerUnChoked id ->
    adjust_peer t id (fun p -> p.we_choking <- false)
  | PeerInterested id ->
    adjust_peer t id (fun p -> p.interested_in_us <- true)
  | PeerNotInterested id ->
    adjust_peer t id (fun p -> p.interested_in_us <- false)
  | PeerRateUpdate (id, ul, dl) ->
    adjust_peer t id (fun p -> p.ul_rate <- ul; p.dl_rate <- dl)
  (* | msg -> *)
  (*   ignore (debug t.id "Unhandled: %s" (string_of_msg msg)) *)

let choking_frequency = 5

let start ~super_ch ~ch =
  let rec ticker () =
    Lwt_unix.sleep (float choking_frequency) >>= fun () ->
    Lwt_pipe.write ch Tick;
    ticker ()
  in
  let run id =
    let t = { peers = H.create 17; id; ticker = ticker () } in
    Lwt_pipe.iter_s (handle_message t) ch
  in
  Proc.spawn ~name:"ChokeMgr" run (Super.default_stop super_ch)
