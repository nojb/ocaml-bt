let (>>=) = Lwt.(>>=)

let debug = Proc.debug

type msg =
  | GrabPiece of Bits.t * int option Lwt_mvar.t
  (** Ask for grabbing some blocks *)
  | PutbackPiece of int
  (** Put these blocks back for retrieval *)
  | PeerHave of int list * bool Lwt_mvar.t
  (** A peer has announce that it has a set of pieces *)
  | PeerUnHave of int list
  | GetDone of Bits.t Lwt_mvar.t
  (** Get the bitset of pieces which are done *)
  | PieceReceived of int * string
  (** A complete piece has been received from a peer *)

let string_of_msg = function
  | GrabPiece (have, _) ->
    Printf.sprintf "GrabPiece: have: %d" (Bits.count have)
  | PutbackPiece n ->
    Printf.sprintf "PutbackPiece: n: %d" n
  | PeerHave (pns, _) ->
    Printf.sprintf "PeerHave: %d pieces" (List.length pns)
      (* (String.concat ", " (List.map string_of_int pns)) *)
  | PeerUnHave (pns) ->
    Printf.sprintf "PeerUnHave: %d pieces" (List.length pns)
  | GetDone _ ->
    "GetDone"
  | PieceReceived (index, s) ->
    Printf.sprintf "PieceReceived: index: %d length: %d"
      index (String.length s)

let rarest_piece_jitter = 42

module H = Histo.MakeImp (Histo.Int) (Histo.Int)

type t = {
  histo : H.t;
  (** Piece histogram *)
  completed : Bits.t;
  (** Pieces we have completed *)
  requested : Bits.t;
  (** Pieces we have requested but not yet completed *)
  pieces : Info.piece_info array;
  (** Info about all the pieces of the torrent *)
  ch : msg Lwt_pipe.t;
  (** Channel used to talk to this Piece Manager *)
  fs_ch : Fs.msg Lwt_pipe.t;
  (** Channel used to talk to the File system *)
  choke_mgr_ch : ChokeMgr.msg Lwt_pipe.t;
  (** Channel to talk to the Choke manager *)
  status_ch : Status.msg Lwt_pipe.t;
  (** Channel to send messages to Status *)
  info_hash : Info.digest;
  (** The torrent's info hash *)
  id : Proc.Id.t
  (** The PieceMgr's thread id *)
}

let grab_piece self (eligible : Bits.t) : int option Lwt.t =
  let interesting =
    Bits.logand eligible (Bits.lognot
      (Bits.logor self.requested self.completed))
  in
  if Bits.count interesting = 0 then
    debug self.id "no interesting pieces - maybe end game mode?" >>= fun () ->
    Lwt.return None
  else begin
    let rarest = H.pick self.histo (Bits.is_set interesting) in
    let pc = List.nth rarest (Random.int (List.length rarest)) in
    Bits.set self.requested pc;
    debug self.id "Requesting piece#%4d" pc >>= fun () ->
    Lwt.return (Some pc)
  end

let peer_have self pns mv =
  List.iter (H.add self.histo) pns;
  let interesting =
    List.exists (fun n ->
      not (Bits.is_set self.requested n || Bits.is_set self.completed n)) pns
  in
  Lwt_mvar.put mv interesting

let peer_unhave self pns =
  List.iter (H.remove self.histo) pns;
  Lwt.return_unit

let piece_received self i s =
  if Bits.is_set self.completed i then
    debug self.id "Received a piece that we already have, ignoring"
  else if not (Bits.is_set self.requested i) then
    debug self.id "Received a piece that has not been requested, ignoring"
  else begin
    Bits.unset self.requested i;
    if Info.Digest.string s = self.pieces.(i).Info.piece_digest then begin
      Bits.set self.completed i;
      debug self.id "Received a valid piece #%d, comitting to disk" i >>= fun () ->
      Lwt_pipe.write self.fs_ch (`WritePiece (i, s));
      Lwt_pipe.write self.choke_mgr_ch (ChokeMgr.PieceCompleted i);
      debug self.id "Now have %d/%d pieces"
        (Bits.count self.completed) (Array.length self.pieces)
      (** FIXME check if we are done *)
    end else
      debug self.id "Received a piece, but SHA-1 hash does not check out,
      ignoring"
  end

let handle_message self msg : unit Lwt.t =
  debug self.id "%s" (string_of_msg msg) >>= fun () ->
  match msg with
  | GrabPiece (eligible, mv)  -> grab_piece self eligible >>= Lwt_mvar.put mv
  | PeerHave (pns, mv)        -> peer_have self pns mv
  | PeerUnHave (pns)          -> peer_unhave self pns
  | GetDone (mv)              -> Lwt_mvar.put mv (Bits.copy self.completed)
  | PieceReceived (i, s)      -> piece_received self i s
  | msg ->
    debug self.id "Unhandled: %s" (string_of_msg msg)

let start ~ch ~fs_ch ~choke_mgr_ch ~status_ch ~have
  ~pieces ~info_hash =
  let run id =
    let self =
      { histo = H.create (); completed = have;
        requested = Bits.create (Bits.length have);
        pieces; ch; fs_ch; choke_mgr_ch; status_ch; info_hash; id }
    in
    Lwt_pipe.iter_s (handle_message self) ch
  in
  Proc.spawn ~name:"PieceMgr" run
