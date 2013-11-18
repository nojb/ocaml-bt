let (>>=) = Lwt.(>>=)

let debug = Proc.debug

type msg =
  [ `GrabPiece of Bits.t * int option Lwt_mvar.t
  (** Ask for grabbing some blocks *)
  | `PutbackPiece of int
  (** Put these blocks back for retrieval *)
  | `PeerHave of int list * bool Lwt_mvar.t
  (** A peer has announce that it has a set of pieces *)
  (* | `GetDone of Bits.t Lwt_mvar.t *)
  (** Get the bitset of pieces which are done *)
  | `PieceReceived of int * string ]
  (** A complete piece has been received from a peer *)

let string_of_msg = function
  | `GrabPiece (have, _) ->
    Printf.sprintf "GrabPiece: have: %d" (Bits.count have)
  | `PutbackPiece n ->
    Printf.sprintf "PutbackPiece: n: %d" n
  | `PeerHave (pns, _) ->
    Printf.sprintf "PeerHave: %d pieces" (List.length pns)
      (* (String.concat ", " (List.map string_of_int pns)) *)
  | `PieceReceived (index, s) ->
    Printf.sprintf "PieceReceived: index: %d length: %d"
      index (String.length s)

let rarest_piece_jitter = 42

module H = Histo.Make (Histo.Int) (Histo.Int)

type t = {
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
  status_ch : Status.msg Lwt_pipe.t;
  (** Channel to send messages to Status *)
  info_hash : Info.digest;
  (** The torrent's info hash *)
  id : Proc.Id.t
  (** The PieceMgr's thread id *)
}

let grab_piece t (eligible : Bits.t) histo : int option Lwt.t =
  let interesting =
    Bits.logand eligible (Bits.lognot (Bits.logor t.requested t.completed))
  in
  if Bits.count interesting = 0 then
    debug t.id "no interesting pieces - maybe end game mode?" >>= fun () ->
    Lwt.return None
  else begin
    let rarest = H.pick (Bits.is_set interesting) histo in
    let pc = List.nth rarest (Random.int (List.length rarest)) in
    Bits.set t.requested pc;
    debug t.id "Requesting piece#%4d" pc >>= fun () ->
    Lwt.return (Some pc)
  end

let handle_message t msg histo : H.t Lwt.t =
  debug t.id "%s" (string_of_msg msg) >>= fun () ->
  match msg with
  | `GrabPiece (eligible, mv) ->
    grab_piece t eligible histo >>= Lwt_mvar.put mv >>= fun () ->
    Lwt.return histo
  | `PeerHave (pns, mv) ->
    let histo = List.fold_left (fun h n -> H.add n h) histo pns in
    let interesting =
      List.exists (fun n ->
        not (Bits.is_set t.requested n || Bits.is_set t.completed n)) pns
    in
    Lwt_mvar.put mv interesting >>= fun () ->
    Lwt.return histo
  | `PieceReceived (i, s) ->
    if Bits.is_set t.completed i then
      debug t.id "Received a piece that we already have, ignoring" >>= fun () ->
      Lwt.return histo
    else if not (Bits.is_set t.requested i) then
      debug t.id "Received a piece that has not been requested, ignoring" >>=
        fun () ->
      Lwt.return histo
    else begin
      Bits.unset t.requested i;
      if Info.Digest.string s = t.pieces.(i).Info.piece_digest then begin
        Bits.set t.completed i;
        debug t.id "Received a valid piece #%d, comitting to disk" i >>= fun () ->
        Lwt_pipe.write t.fs_ch (`WritePiece (i, s));
        (** FIXME check if we are done *)
        Lwt.return histo
      end else
        debug t.id "Received a piece, but SHA-1 hash does not check out,
        ignoring" >>= fun () ->
        Lwt.return histo
    end
  | msg ->
    debug t.id "Unhandled: %s" (string_of_msg msg) >>= fun () ->
    Lwt.return histo

let start ~super_ch ~ch ~fs_ch ~status_ch ~have ~pieces ~info_hash =
  let run id =
    let t =
      { completed = have; requested = Bits.create (Bits.length have);
        pieces; ch; fs_ch; status_ch; info_hash; id }
    in
    Lwt_pipe.fold_s (handle_message t) ch H.empty >>= fun _ ->
    Lwt.return_unit
  in
  Proc.spawn ~name:"PieceMgr" run (Super.default_stop super_ch)
    (fun _ -> Lwt.return_unit)
