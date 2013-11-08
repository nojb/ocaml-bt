open Messages
open Printf

let (>>=) = Lwt.(>>=)

let debug = Supervisor.debug

let safe_to_int n =
  let n' = Int64.to_int n in
  assert (Int64.(compare (of_int n') n) = 0);
  n'

let rec get_chunks handles ~offset ~size =
  if size < 0 then
    invalid_arg "Fs.get_chunks"
  else
    match handles with
    | [] ->
      failwith "get_chunks: tring to read beyond torrent length"
    | (ic, oc, size1) :: handles ->
      let open Int64 in
      if compare offset size1 >= 0 then
        get_chunks handles ~offset:(sub offset size1) ~size
      else begin
        let size1 = sub size1 offset in
        if compare size1 (of_int size) >= 0 then
          [ic, oc, offset, size]
        else
          let size1 = safe_to_int size1 in
          (ic, oc, offset, size1) :: get_chunks handles ~offset:0L ~size:(size - size1)
      end

let string_of_msg = function
  | CheckPiece (pn, _) ->
    sprintf "CheckPiece: %d" pn
  | ReadBlock (pn, Block (boff, blen), _) ->
    sprintf "ReadBlock: piece: %d offset: %d length: %d" pn boff blen
  | WriteBlock (pn, Block (boff, blen), _) ->
    sprintf "WriteBlock: piece: %d offset: %d length: %d" pn boff blen

let read_block id handles pi (Block (boff, blen)) : string Lwt.t =
  let data = String.create blen in
  let read_chunk doff (ic, _, off, len) =
    Lwt_io.set_position ic off >>
    Lwt_io.read_into_exactly ic data doff len >>
    Lwt.return (doff + len)
  in
  let start = Int64.(add pi.Torrent.piece_offset (of_int boff)) in
  lwt n = Lwt_list.fold_left_s read_chunk 0
    (get_chunks handles ~offset:start ~size:blen)
  in
  assert (n = blen);
  debug id "read_block: offset: %d length: %d" boff blen >>
  Lwt.return data

let write_block id handles pi (Block (boff, blen)) data : unit Lwt.t =
  assert (String.length data = blen);
  let write_chunk doff (_, oc, off, len) =
    Lwt_io.set_position oc off >>
    Lwt_io.write_from_exactly oc data doff len >>
    Lwt.return (doff + len)
  in
  let start = Int64.(add pi.Torrent.piece_offset (of_int boff)) in
  lwt n = Lwt_list.fold_left_s write_chunk 0 
    (get_chunks handles ~offset:start ~size:blen) in
  assert (n = blen);
  debug id "write_block: offset: %d length: %d" boff blen >>
  Lwt.return ()

(** raises End_of_file if [pi] refers to a piece beyond the
 * end of file *)
let check_piece id handles (pi : Torrent.piece_info) : bool Lwt.t =
  let data = String.create pi.Torrent.piece_length in
  let read_chunk doff (ic, _, off, len) =
    Lwt_io.set_position ic off >>
    Lwt_io.read_into_exactly ic data doff len >>
    Lwt.return (doff + len)
  in
  lwt n = Lwt_list.fold_left_s read_chunk 0
    (get_chunks handles ~offset:pi.Torrent.piece_offset ~size:pi.Torrent.piece_length)
  in
  assert (n = pi.Torrent.piece_length);
  let digest = Torrent.Digest.string data in
  Lwt.return (Torrent.Digest.equal digest pi.Torrent.piece_digest)

let check_file id handles pieces : Bits.t Lwt.t =
  let n = Array.length pieces in
  let have = Bits.create n in
  try_lwt
    let rec loop i =
      if i >= n then Lwt.return have
      else
        lwt valid = check_piece id handles pieces.(i) in
        if valid then Bits.set have i;
        loop (i+1)
    in loop 0
  with
  | End_of_file -> Lwt.return have

let handle_message id handles pieces msg : unit Lwt.t =
  debug id "%s" (string_of_msg msg) >>
  match msg with
  | CheckPiece (pn, mv) ->
    check_piece id handles pieces.(pn) >>= Lwt_mvar.put mv
  | ReadBlock (n, bl, mv) ->
    read_block id handles pieces.(n) bl >>= Lwt_mvar.put mv
  | WriteBlock (n, bl, data) ->
    write_block id handles pieces.(n) bl data

let start ~msg_supervisor ~handles ~pieces ~fs =
  let event_loop id =
    Lwt_stream.iter_s (handle_message id handles pieces) fs
  in
  Supervisor.spawn_worker msg_supervisor "FS" event_loop

(* let safe_map path f = *)
(*   lwt fd = Lwt_unix.openfile path [...] in *)
(*   try_lwt *)
(*     f (Lwt_bytes.map_file fd ...) *)
(*   with *)
(*   | exn -> *)
(*     Lwt_unix.close fd; *)
(*     raise_lwt exn *)

let open_and_check_file id info : 'a Lwt.t =
  lwt handles = Lwt_list.map_s (fun fi ->
    let path = String.concat "/" fi.Torrent.file_path in
    lwt oc = Lwt_io.open_file ~mode:Lwt_io.Output path in
    lwt ic = Lwt_io.open_file ~mode:Lwt_io.Input path in
    Lwt.return (ic, oc, fi.Torrent.file_size)) info.Torrent.files in
  lwt have = check_file id handles info.Torrent.pieces in
  debug id "Torrent data check successful" >>
  Lwt.return (handles, have)

