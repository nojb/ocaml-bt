let (>>=) = Lwt.(>>=)

let debug = Proc.debug

let string_of_msg = function
  | Msg.GrabBlocks (n, have, _) ->
    Printf.sprintf "GrabBlocks: n: %d have: %d" n (Bits.count have)
  | Msg.PutbackBlocks _ ->
    "PutbackBlocks"

module Block = struct
  type t = Msg.block
  let compare = compare
end

module PieceBlock = struct
  type t = int * Msg.block
  let compare = compare
end

module Int = struct
  type t = int
  let compare = compare
end

module MakeSet(Ord : Set.OrderedType) = struct
  include Set.Make(Ord)
  let take n s : t * t =
    let rec loop acc n s =
      if is_empty s || n <= 0 then acc, s
      else
        let x = choose s in
        loop (add x acc) (n-1) (remove x s)
    in loop empty n s
  let union_map f s1 s2 =
    fold (fun x1 s2 -> add (f x1) s2) s1 s2
  let map f s =
    fold (fun x s -> add (f x) s) s empty
end

module IntSet = Set.Make (Int)
module BlockSet = MakeSet (Block)
module PieceBlockSet = MakeSet (PieceBlock)
module IntMap = Map.Make (Int)

let default_block_size = 16384 (* in bytes *)

type progress_info = {
  no_blocks : int;
  have_blocks : BlockSet.t;
  pending_blocks : BlockSet.t
}

type db = {
  in_progress : progress_info IntMap.t;
  have : IntSet.t;
  pending : IntSet.t;
  downloading : PieceBlockSet.t;
  all_pieces : Torrent.piece_info array
  (** info about all the pieces in the torrent *)
}

type t = {
  ch : Msg.piece_mgr_msg Lwt_pipe.t;
  (** channel used to talk to this Piece Manager *)
  status_ch : Msg.status_msg Lwt_pipe.t;
  (** channel to send messages to Status *)
  info_hash : Torrent.digest;
  (** The torrent's info hash *)
  id : Proc.Id.t
  (** The PieceMgr's thread id *)
}

(** Utility functions *)
let blocks_of_piece bsz psz : BlockSet.t =
  let rec loop rem off acc =
    if rem = 0 then acc
    else if rem <= bsz then (BlockSet.add (Msg.Block (off, rem)) acc)
    else loop (rem - bsz) (off + bsz) (BlockSet.add (Msg.Block (off, bsz)) acc)
  in loop psz 0 BlockSet.empty

let rec grab_in_progress db n eligible (captured : PieceBlockSet.t) (ipp :
  progress_info IntMap.t) : db * PieceBlockSet.t =
  if n = 0 then db, captured
  else if IntMap.is_empty ipp then
    grab_pending db n eligible captured db.pending
  else
    let i, p = IntMap.choose ipp in
    if Bits.is_set eligible i then
      let (grabbed : BlockSet.t), rest = BlockSet.take n p.pending_blocks in
      grab_in_progress db (n-BlockSet.cardinal grabbed) eligible
        (* (PieceBlockSet.union_map (fun x -> (i, x)) grabbed captured) *)
        PieceBlockSet.empty
        (IntMap.remove i ipp)
    else
      grab_in_progress db n eligible captured (IntMap.remove i ipp)

and grab_pending db n eligible captured pending : db * PieceBlockSet.t =
  if IntSet.is_empty pending || n = 0 then
    db, captured
  else
    let p = IntSet.choose pending in
    if Bits.is_set eligible p then
      let bl = blocks_of_piece default_block_size db.all_pieces.(p).Torrent.piece_length in
      let ip =
        { no_blocks = BlockSet.cardinal bl;
          have_blocks = BlockSet.empty;
          pending_blocks = bl }
      in
      let db =
        { db with
          in_progress = IntMap.add p ip db.in_progress;
          pending = IntSet.remove p db.pending }
      in
      grab_in_progress db n eligible captured (IntMap.singleton p ip)
    else
      grab_pending db n eligible captured (IntSet.remove p pending)

let grab_blocks db n (eligible : Bits.t) : db * PieceBlockSet.t =
  let db, blocks = grab_in_progress db n eligible PieceBlockSet.empty db.in_progress in
  if PieceBlockSet.is_empty blocks && not (IntSet.is_empty db.pending) then
    failwith "PieceMgr.grab_blocks: entering end game mode: not implemented"
  else begin
    let db =
      { db with downloading = PieceBlockSet.union blocks db.downloading }
    in
    (* List.iter (fun blk -> Hashset.add t.downloading blk) blocks; *)
    (* t.downloading <- *)
    (*   List.fold_left (fun s blk -> DownloadSet.add blk s) *)
    (*     t.downloading blocks; *)
    db, blocks
  end

let handle_message t msg db : db Lwt.t =
  debug t.id "%s" (string_of_msg msg) >>= fun () ->
  match msg with
  | Msg.GrabBlocks (n, eligible, mv) ->
    let db, blocks = grab_blocks db n eligible in
    Lwt_mvar.put mv (PieceBlockSet.elements blocks) >>= fun () ->
    Lwt.return db
    (* Lwt_mvar.put mv (grab_blocks t n eligible) *)
  | msg ->
    debug t.id "Unhandled: %s" (string_of_msg msg) >>= fun () ->
    Lwt.return db

let start ~super_ch ~ch ~status_ch db ~info_hash =
  let run id =
    let t = { ch; status_ch; info_hash; id } in
    Lwt_pipe.fold_s (handle_message t) ch db >>= fun _ ->
    Lwt.return ()
  in
  Proc.spawn ~name:"PieceMgr" run (Super.default_stop super_ch)
    (fun _ -> Lwt.return_unit)

let create_piece_db have pieces =
  let pieces' =
    let i = ref (-1) in
    Array.fold_right (fun _ s -> incr i; IntSet.add !i s) pieces IntSet.empty
  in
  let have, pending = IntSet.partition (Bits.is_set have) pieces' in
  { in_progress = IntMap.empty;
    have;
    pending;
    downloading = PieceBlockSet.empty;
    all_pieces = pieces }
    
