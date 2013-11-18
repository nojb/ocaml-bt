open Printf

let (>>=) = Lwt.(>>=)

let debug = Proc.debug

let failwith_f fmt = Printf.ksprintf failwith fmt

let safe_to_int n =
  let n' = Int64.to_int n in
  assert (Int64.(compare (of_int n') n) = 0);
  n'

type t = {
  handles : (Lwt_io.input_channel * Lwt_io.output_channel * int64) list;
  pieces : Info.piece_info array;
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

type msg =
  [ `CheckPiece of int * bool Lwt_mvar.t
  | `ReadBlock of int * Msg.block * string Lwt_mvar.t
  | `WritePiece of int * string ]

let string_of_msg = function
  | `CheckPiece (pn, _) ->
    sprintf "CheckPiece: %d" pn
  | `ReadBlock (pn, Msg.Block (boff, blen), _) ->
    sprintf "ReadBlock: index: %d offset: %d length: %d" pn boff blen
  (* | Msg.WriteBlock (pn, Msg.Block (boff, blen), _) -> *)
  (*   sprintf "WriteBlock: piece: %d offset: %d length: %d" pn boff blen *)
  | `WritePiece (index, s) ->
    sprintf "WritePiece: index: %d length: %d" index (String.length s)

let read_block t i (Msg.Block (boff, blen)) : string Lwt.t =
  let pi = t.pieces.(i) in
  let data = String.create blen in
  let read_chunk doff (ic, _, off, len) =
    Lwt_io.set_position ic off >>= fun () ->
    Lwt_io.read_into_exactly ic data doff len >>= fun () ->
    Lwt.return (doff + len)
  in
  let start = Int64.(add pi.Info.piece_offset (of_int boff)) in
  lwt n = Lwt_list.fold_left_s read_chunk 0
    (get_chunks t.handles ~offset:start ~size:blen)
  in
  assert (n = blen);
  debug t.id "read_block: offset: %d length: %d" boff blen >>= fun () ->
  Lwt.return data

(* let write_block t i (Msg.Block (boff, blen)) data : unit Lwt.t = *)
(*   let pi = t.pieces.(i) in *)
(*   assert (String.length data = blen); *)
(*   let write_chunk doff (_, oc, off, len) = *)
(*     Lwt_io.set_position oc off >>= fun () -> *)
(*     Lwt_io.write_from_exactly oc data doff len >>= fun () -> *)
(*     Lwt.return (doff + len) *)
(*   in *)
(*   let start = Int64.(add pi.Info.piece_offset (of_int boff)) in *)
(*   lwt n = Lwt_list.fold_left_s write_chunk 0  *)
(*     (get_chunks t.handles ~offset:start ~size:blen) in *)
(*   assert (n = blen); *)
(*   debug t.id "write_block: offset: %d length: %d" boff blen *)

let write_piece t index s : unit Lwt.t =
  let pi = t.pieces.(index) in
  assert (String.length s = pi.Info.piece_length);
  let write_chunk doff (_, oc, off, len) =
    Lwt_io.set_position oc off >>= fun () ->
    Lwt_io.write_from_exactly oc s doff len >>= fun () ->
    Lwt.return (doff + len)
  in
  Lwt_list.fold_left_s write_chunk 0
    (get_chunks t.handles ~offset:pi.Info.piece_offset
      ~size:pi.Info.piece_length) >>= fun n ->
  assert (n = pi.Info.piece_length);
  debug t.id "write_piece: index: %d length: %d successful"
    index (String.length s)

(** raises End_of_file if [pi] refers to a piece beyond the
 * end of file *)
let check_piece handles (pi : Info.piece_info) : bool Lwt.t =
  let data = String.create pi.Info.piece_length in
  let read_chunk doff (ic, _, off, len) =
    Lwt_io.set_position ic off >>= fun () ->
    Lwt_io.read_into_exactly ic data doff len >>= fun () ->
    Lwt.return (doff + len)
  in
  lwt n = Lwt_list.fold_left_s read_chunk 0
    (get_chunks handles ~offset:pi.Info.piece_offset ~size:pi.Info.piece_length)
  in
  assert (n = pi.Info.piece_length);
  let digest = Info.Digest.string data in
  Lwt.return (Info.Digest.equal digest pi.Info.piece_digest)

let check_file id handles pieces : Bits.t Lwt.t =
  let n = Array.length pieces in
  let have = Bits.create n in
  try_lwt
    let rec loop i =
      if i >= n then
        Lwt.return have
      else
        check_piece handles pieces.(i) >>= fun valid ->
        if valid then Bits.set have i;
        loop (i+1)
    in loop 0
  with
  | End_of_file -> Lwt.return have

let handle_message t msg : unit Lwt.t =
  debug t.id "%s" (string_of_msg msg) >>= fun () ->
  match msg with
  | `CheckPiece (pn, mv) ->
    check_piece t.handles t.pieces.(pn) >>= Lwt_mvar.put mv
  | `ReadBlock (pn, bl, mv) ->
    read_block t pn bl >>= Lwt_mvar.put mv
  | `WritePiece (index, s) ->
    write_piece t index s
  (* | `WriteBlock (pn, bl, data) -> *)
  (*   write_block t pn bl data *)

let start ~super_ch ~handles ~pieces ~ch =
  let run id =
    let t = { handles; pieces; id } in
    Lwt_pipe.iter_s (handle_message t) ch
  in
  Proc.spawn ~name:"Fs" run (Super.default_stop super_ch)
    (fun _ -> Lwt.return_unit)

let with_cwd path f =
  let cwd = Sys.getcwd () in
  try
    Sys.chdir path; f () >>= fun x -> Sys.chdir cwd; Lwt.return x
  with
  | exn -> Sys.chdir cwd; raise_lwt exn

let open_file path =
  let rec loop path =
    match path with
    | [] ->
      assert false
    | [path] ->
      Lwt_io.open_file ~mode:Lwt_io.output path >>= fun oc ->
      Lwt_io.open_file ~mode:Lwt_io.input path >>= fun ic ->
      Lwt.return (ic, oc)
    | dir :: path ->
      if Sys.file_exists dir then
        if Sys.is_directory dir then
          with_cwd dir (fun () -> loop path)
        else
          failwith_f "Fs.open_file: '%s/%s' exists and is not a directory"
            (Sys.getcwd ()) dir
      else
        let n = Sys.command ("mkdir " ^ dir) in
        if n <> 0 then
          failwith_f "Fs.open_file: 'mkdir %s' returned error %d" dir n
        else
          with_cwd dir (fun () -> loop path)
  in
  loop path

let open_and_check_file id info : 'a Lwt.t =
  lwt handles = Lwt_list.map_s (fun fi ->
    open_file fi.Info.file_path >>= fun (ic, oc) ->
    Lwt.return (ic, oc, fi.Info.file_size)) info.Info.files in
  check_file id handles info.Info.pieces >>= fun have ->
  debug id "Torrent data check successful" >>= fun () ->
  Lwt.return (handles, have)

