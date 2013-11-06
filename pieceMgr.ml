open Messages

module BlockSet = Set.Make (struct type t = block let compare = compare end)
module DownloadSet = Set.Make (struct type t = int * block let compare =
  compare end)

type piece_status =
  | Pending
  (** Piece is missing *)
  | Done
  (** Piece is downloaded and its SHA1 hash has been verified *)
  | InProgress of int * BlockSet.t * block list
  (** Piece is currently being downloaded *)

type state = {
  piece_status : piece_status array;
  (** info about the pieces we have/are missing/are downloading *)
  mutable downloading : DownloadSet.t;
  (** blocks in progress *)
  piece_info : piece_info array;
  (** info about all the pieces in the torrent *)
  piece_mgr : piece_mgr_msg Lwt_stream.t;
  (** stream used to talk to this Piece Manager *)
  msg_status : statatus_msg -> unit;
  (** callback to send messages to Status *)
  info_hash : Torrent.digest
  (** The torrent's info hash *)
}

let array_fold_left_i f x a =
  let rec loop acc i =
    if i >= Array.length a then acc
    else loop (f acc i a.(i)) (i+1)
  in loop x 0

let rec try_grab_in_progress id st n eligible captured ipp =
  let rec loop n captured = function
    | 0, _ -> Lwt.return captured
    | _, [] -> try_grab_pending id st n eligible captured
    | _, i :: ipp ->
      begin match st.piece_status.(i) with
      | InProgress (pn, have, bs) ->
        let blks, rest = split_at n bs in
        st.downloading.(pn) <- InProgress (pn, have, rest);
        try_grab_in_progress id st (n - List.length blks)
          (List.map (fun b -> (pn, b)) blks @ captured) ipp in
      | _ -> 
        failwith "..."
      end
  in loop n captured ipp

and try_grab_pending id st n eligible captured =
  ...

let try_grab id st n eligible : (int * block) list Lwt.t =
  let ipp = array_fold_left_i (fun ipp i x ->
    match x with
    | InProgress _ -> i :: ipp
    | _ -> ipp) [] st.piece_status
  in
  try_grab_in_progress id st n eligible [] ipp

let grab_blocks id st n eligible : (int * block) list Lwt.t =
  lwt blocks = try_grab id st n eligible in
  let pending =
    let rec loop i =
      if i >= Array.length st.piece_status then false
      else match st.piece_status.(i) with
      | Pending -> true
      | _ -> loop (i+1)
    in
    loop 0
  in
  if List.length blocks = 0 && not pending then
    debug id "Entered EndGame Mode" >> failwith "Not Implemented"
  else begin
    st.downloading <-
      List.fold_left (fun s blk -> DownloadSet.add blk s)
        st.downloading blocks;
    Lwt.return blocks
  end

let handle_message id st =
  lwt msg = Lwt_stream.next st.piece_mgr in
  debug id "%s" (string_of_msg msg) >>
  match msg with
  | GrabBlocks (n, eligible, mv) ->
    grab_blocks id st n eligible
  | PutbackBlocks blks ->
    ...
  | msg ->
    debug id "Unhandled: %s" (string_of_msg msg)

let start ~monitor ~piece_mgr ~msg_status ~st ~info_hash =
  ...
