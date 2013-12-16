let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let failwith fmt = Printf.ksprintf failwith fmt

type msg =
  | CheckPiece of int * bool Lwt_mvar.t
  | ReadBlock of int * Wire.block * string Lwt_mvar.t
  | WritePiece of int * string

let string_of_msg = function
  | CheckPiece (pn, _) ->
    Printf.sprintf "CheckPiece: %d" pn
  | ReadBlock (pn, Wire.Block (boff, blen), _) ->
    Printf.sprintf "ReadBlock: index: %d offset: %d length: %d" pn boff blen
  (* | Msg.WriteBlock (pn, Msg.Block (boff, blen), _) -> *)
  (*   sprintf "WriteBlock: piece: %d offset: %d length: %d" pn boff blen *)
  | WritePiece (index, s) ->
    Printf.sprintf "WritePiece: index: %d length: %d" index (String.length s)

let log_prefix = "[store]"

(* let debug ?exn fmt = *)
(*   Log.trace ?exn fmt *)
  (* Printf.ksprintf (fun msg -> *)
  (*     match exn with *)
  (*     | None -> *)
  (*       Printf.eprintf "%s %s\n%!" log_prefix msg *)
  (*     | Some exn -> *)
  (*       Printf.eprintf "%s %s: %s\n%!" log_prefix msg (Printexc.to_string exn)) fmt *)

type t = {
  handles : (Lwt_io.input_channel * Lwt_io.output_channel * int64) list;
  pieces : Info.piece_info array;
  send : msg option -> unit
}

let safe_to_int n =
  let n' = Int64.to_int n in
  assert (Int64.(compare (of_int n') n) = 0);
  n'

let rec get_chunks handles ~offset ~size =
  if size < 0 then
    invalid_arg "Store.get_chunks"
  else
    match handles with
    | [] ->
      failwith "Store.get_chunks: tring to read beyond torrent length"
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

let _read_block store i (Wire.Block (boff, blen)) : string Lwt.t =
  let pi = store.pieces.(i) in
  let data = String.create blen in
  let read_chunk doff (ic, _, off, len) =
    Lwt_io.set_position ic off >>= fun () ->
    Lwt_io.read_into_exactly ic data doff len >>= fun () ->
    Lwt.return (doff + len)
  in
  let start = Int64.(add pi.Info.piece_offset (of_int boff)) in
  Lwt_list.fold_left_s read_chunk 0
    (get_chunks store.handles ~offset:start ~size:blen) >>= fun n ->
  assert (n = blen);
  Trace.infof "read_block: offset:%d length:%d" boff blen;
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

let _write_piece store index s : unit Lwt.t =
  let pi = store.pieces.(index) in
  assert (String.length s = pi.Info.piece_length);
  let write_chunk doff (_, oc, off, len) =
    Lwt_io.set_position oc off >>= fun () ->
    Lwt_io.write_from_exactly oc s doff len >>= fun () ->
    Lwt.return (doff + len)
  in
  Lwt_list.fold_left_s write_chunk 0
    (get_chunks store.handles ~offset:pi.Info.piece_offset
      ~size:pi.Info.piece_length) >>= fun n ->
  assert (n = pi.Info.piece_length);
  Trace.infof "write_piece: index:%d length:%d"
    index (String.length s);
  Lwt.return_unit

(** raises End_of_file if [pi] refers to a piece beyond the
 * end of file *)
let check_piece handles (pi : Info.piece_info) : bool Lwt.t =
  let data = String.create pi.Info.piece_length in
  let read_chunk doff (ic, _, off, len) =
    Lwt_io.set_position ic off >>= fun () ->
    Lwt_io.read_into_exactly ic data doff len >>= fun () ->
    Lwt.return (doff + len)
  in
  Lwt_list.fold_left_s read_chunk 0
    (get_chunks handles ~offset:pi.Info.piece_offset ~size:pi.Info.piece_length) >>= fun n ->
  assert (n = pi.Info.piece_length);
  let digest = Word160.digest_of_string data in
  Lwt.return (Word160.equal digest pi.Info.piece_digest)

let check_file handles pieces : Bits.t Lwt.t =
  let n = Array.length pieces in
  let have = Bits.create n in
  let rec loop i =
    if i >= n then
      Lwt.return have
    else begin
      check_piece handles pieces.(i) >>= fun valid ->
      if valid then Bits.set have i;
      loop (i+1)
    end
  in
  Lwt.catch (fun () -> loop 0)
    (function
      | End_of_file -> Lwt.return have
      | _ as exn -> Lwt.fail exn)
  (* try_lwt *)
  (*   let rec loop i = *)
  (*     if i >= n then *)
  (*       Lwt.return have *)
  (*     else *)
  (*       check_piece handles pieces.(i) >>= fun valid -> *)
  (*       if valid then Bits.set have i; *)
  (*       loop (i+1) *)
  (*   in loop 0 *)
  (* with *)
  (* | End_of_file -> Lwt.return have *)

let handle_message store msg : unit Lwt.t =
  Trace.recv "STORE" (string_of_msg msg);
  match msg with
  | CheckPiece (pn, mv) ->
    check_piece store.handles store.pieces.(pn) >>= Lwt_mvar.put mv
  | ReadBlock (pn, bl, mv) ->
    _read_block store pn bl >>= Lwt_mvar.put mv
  | WritePiece (index, s) ->
    _write_piece store index s
  (* | `WriteBlock (pn, bl, data) -> *)
  (*   write_block t pn bl data *)

let read_block store i b =
  let mv = Lwt_mvar.create_empty () in
  store.send (Some (ReadBlock (i, b, mv)));
  Lwt_mvar.take mv

let write_piece store i s =
  store.send (Some (WritePiece (i, s)))

let create handles (minfo : Info.t) =
  let strm, send = Lwt_stream.create () in
  let store = { handles; pieces = minfo.Info.pieces; send } in
  let run () =
    Lwt_stream.iter_s (handle_message store) strm
  in
  Lwt.async run;
  store

let with_cwd path f =
  let cwd = Sys.getcwd () in
  try
    Sys.chdir path;
    f () >>= fun x ->
    Sys.chdir cwd;
    Lwt.return x
  with
  | exn ->
    Sys.chdir cwd;
    Lwt.fail exn

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
          failwith "Store.open_file: '%s/%s' exists and is not a directory"
            (Sys.getcwd ()) dir
      else
        let n = Sys.command (Printf.sprintf "mkdir \"%s\"" dir) in
        if n <> 0 then
          failwith "Store.open_file: 'mkdir %s' returned error %d" dir n
        else
          with_cwd dir (fun () -> loop path)
  in
  loop path

let open_and_check_file info : 'a Lwt.t =
  Lwt_list.map_s (fun fi ->
    open_file fi.Info.file_path >|= fun (ic, oc) ->
    (ic, oc, fi.Info.file_size)) info.Info.files >>= fun handles ->
  check_file handles info.Info.pieces >>= fun have ->
  Trace.infof "Store initialised"; (* add more info: how much data already on disk, etc.. FIXME *)
  Lwt.return (handles, have)
