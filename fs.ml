(* open Messages *)
open Printf

let (>>=) = Lwt.(>>=)

let debug id fmt =
  Printf.ksprintf (fun msg ->
    Lwt_log.debug_f ~exn "FS %s: %s" (Proc.Id.to_string id) msg) fmt

let safe_to_int n =
  let n' = Int64.to_int n in
  assert (Int64.(compare (of_int n') n) = 0);
  n'

type t = {
  handles : (Lwt_io.input_channel * Lwt_io.output_channel * int64) list;
  pieces : Torrent.piece_info array;
  id : Proc.Id.t
}

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

let read_block t i (Block (boff, blen)) : string Lwt.t =
  let pi = t.pieces.(i) in
  let data = String.create blen in
  let read_chunk doff (ic, _, off, len) =
    Lwt_io.set_position ic off >>= fun () ->
    Lwt_io.read_into_exactly ic data doff len >>= fun () ->
    Lwt.return (doff + len)
  in
  let start = Int64.(add pi.Torrent.piece_offset (of_int boff)) in
  lwt n = Lwt_list.fold_left_s read_chunk 0
    (get_chunks t.handles ~offset:start ~size:blen)
  in
  assert (n = blen);
  debug id "read_block: offset: %d length: %d" boff blen >>= fun () ->
  Lwt.return data

let write_block t i (Block (boff, blen)) data : unit Lwt.t =
  let pi = t.pieces.(i) in
  assert (String.length data = blen);
  let write_chunk doff (_, oc, off, len) =
    Lwt_io.set_position oc off >>= fun () ->
    Lwt_io.write_from_exactly oc data doff len >>= fun () ->
    Lwt.return (doff + len)
  in
  let start = Int64.(add pi.Torrent.piece_offset (of_int boff)) in
  lwt n = Lwt_list.fold_left_s write_chunk 0 
    (get_chunks t.handles ~offset:start ~size:blen) in
  assert (n = blen);
  debug id "write_block: offset: %d length: %d" boff blen

(** raises End_of_file if [pi] refers to a piece beyond the
 * end of file *)
let check_piece t i : bool Lwt.t =
  let pi = t.pieces.(i) in
  let data = String.create pi.Torrent.piece_length in
  let read_chunk doff (ic, _, off, len) =
    Lwt_io.set_position ic off >>= fun () ->
    Lwt_io.read_into_exactly ic data doff len >>= fun () ->
    Lwt.return (doff + len)
  in
  lwt n = Lwt_list.fold_left_s read_chunk 0
    (get_chunks t.handles ~offset:pi.Torrent.piece_offset ~size:pi.Torrent.piece_length)
  in
  assert (n = pi.Torrent.piece_length);
  let digest = Torrent.Digest.string data in
  Lwt.return (Torrent.Digest.equal digest pi.Torrent.piece_digest)

let check_file handles pieces : Bits.t Lwt.t =
  let n = Array.length pieces in
  let have = Bits.create n in
  try_lwt
    let rec loop i =
      if i >= n then Lwt.return have
      else
        lwt valid = check_piece t i in
        if valid then Bits.set have i;
        loop (i+1)
    in loop 0
  with
  | End_of_file -> Lwt.return have

let handle_message t msg : unit Lwt.t =
  debug t.id "%s" (string_of_msg msg) >>= fun () ->
  match msg with
  | CheckPiece (pn, mv) ->
    check_piece t pn >>= Lwt_mvar.put mv
  | ReadBlock (pn, bl, mv) ->
    read_block t pn bl >>= Lwt_mvar.put mv
  | WriteBlock (pn, bl, data) ->
    write_block t pn bl data

let start ~send_super ~handles ~pieces ~fs =
  let msgs, send = Lwt_stream.create () in
  let run id =
    let t = { handles; pieces; id } in
    Lwt_stream.iter_s (handle_message t) msgs
  in
  let id = Proc.spawn (Proc.cleanup run
    (Super.default_stop send_super) (fun _ -> Lwt.return_unit) in
  id, send

let open_and_check_file info id : 'a Lwt.t =
  lwt handles = Lwt_list.map_s (fun fi ->
    let path = String.concat "/" fi.Torrent.file_path in
    lwt oc = Lwt_io.open_file ~mode:Lwt_io.Output path in
    lwt ic = Lwt_io.open_file ~mode:Lwt_io.Input path in
    Lwt.return (ic, oc, fi.Torrent.file_size)) info.Torrent.files in
  lwt have = check_file id handles info.Torrent.pieces in
  debug id "Torrent data check successful" >>
  Lwt.return (handles, have)

