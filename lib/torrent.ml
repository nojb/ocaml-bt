module Piece = struct
  type t = { i : int; hash : string; len : int }
end

let check_integriy piece buf = piece.Piece.hash = Sha1.to_bin (Sha1.string buf)
let max_block_size = 16384
let max_backlog = 5

let attempt_download_piece ~clock ~peer ~piece =
  let downloaded = ref 0 in
  let requested = ref 0 in
  let backlog = ref 0 in
  let buf = Bytes.create piece.Piece.len in
  Eio.Time.with_timeout clock 30.0 @@ fun () ->
  while !downloaded < piece.Piece.len do
    if not (Peer.choked peer) then
      while !backlog < max_backlog && !requested < piece.Piece.len do
        let block_size = min max_block_size (piece.Piece.len - !requested) in
        Peer.send_request peer ~i:piece.Piece.i ~ofs:!requested ~len:block_size;
        requested := !requested + block_size;
        backlog := !backlog + 1
      done;
    match Peer.read_message peer with
    | Peer.Message.Piece { i = _; ofs; data } ->
        let len = String.length data in
        Bytes.blit_string data 0 buf ofs len;
        downloaded := !downloaded + len;
        backlog := !backlog - 1
    | _ -> ()
  done;
  let buf = Bytes.unsafe_to_string buf in
  if check_integriy piece buf then Error `Hash_failed else Ok buf

let start_download_worker ~sw ~net ~clock ~info_hash ~peer_id ~peer ~work_queue
    ~results =
  let on_error exn = raise exn in
  (* FIXME *)
  Eio.Fiber.fork_sub ~sw ~on_error @@ fun sw ->
  match peer.Tracker.Response.Peer.ip with
  | `Ipaddr addr -> (
      let peer =
        Peer.run ~net ~clock ~sw ~info_hash ~peer_id addr
          peer.Tracker.Response.Peer.port
      in
      match peer with
      | Error err ->
          Logs.debug (fun f ->
              f "Connection to %s failed: %s"
                (Unix.string_of_inet_addr addr)
                (Peer.string_of_error err))
      | Ok peer ->
          Logs.debug (fun f ->
              f "Completed handshake with %s" (Unix.string_of_inet_addr addr));
          Peer.send_unchoke peer;
          Peer.send_interested peer;
          let rec loop () =
            let piece = Eio.Stream.take work_queue in
            if not (Peer.has_piece peer piece.Piece.i) then (
              Eio.Stream.add work_queue piece;
              loop ())
            else (
              (match attempt_download_piece ~clock ~peer ~piece with
              | Ok buf -> Eio.Stream.add results (piece, buf)
              | Error `Hash_failed ->
                  Logs.warn (fun f ->
                      f "Piece #%d failed integrity check" piece.Piece.i);
                  Eio.Stream.add work_queue piece
              | Error `Timeout ->
                  Logs.warn (fun f -> f "Piece #%d timed out" piece.Piece.i);
                  Eio.Stream.add work_queue piece);
              loop ())
          in
          loop ())
  | `Name _ -> ()

let piece_bounds ~piece_offset ~piece_length files =
  let rec loop piece_ofs buf_ofs buf_len = function
    | [] -> assert false
    | (file, file_length) :: files ->
        let file_rem = file_length - piece_ofs in
        if file_rem <= 0 then loop (-file_rem) buf_ofs buf_len files
        else if buf_len <= file_rem then [ (file, piece_ofs, buf_ofs, buf_len) ]
        else
          (file, piece_ofs, buf_ofs, file_rem)
          :: loop 0 (buf_ofs + file_rem) (buf_len - file_rem) files
  in
  loop piece_offset 0 piece_length files

let download ~net ~clock ~cwd ~info_hash ~peer_id ~meta ~peers =
  let num_pieces = Array.length meta.Meta.pieces in
  let work_queue = Eio.Stream.create num_pieces in
  let results = Eio.Stream.create 100 in
  Eio.Switch.run @@ fun sw ->
  let files =
    List.map
      (fun file ->
        Eio.Path.open_out ~sw ~create:(`If_missing 0o600)
          Eio.Path.(cwd / file.Meta.path))
      meta.Meta.files
  in
  for i = 0 to num_pieces - 1 do
    let hash = meta.Meta.pieces.(i) in
    let len = Meta.piece_length meta i in
    Eio.Stream.add work_queue { Piece.i; hash; len }
  done;
  Eio.Fiber.first
    (fun () ->
      Eio.Fiber.iter
        (fun peer ->
          start_download_worker ~sw ~net ~clock ~info_hash ~peer_id ~peer
            ~work_queue ~results)
        peers)
    (fun () ->
      let completed = ref 0 in
      while !completed < num_pieces do
        let piece, buf = Eio.Stream.take results in
        let bounds =
          let piece_offset = Meta.piece_offset meta piece.Piece.i in
          let piece_length = Meta.piece_length meta piece.Piece.i in
          let files =
            List.map2
              (fun file { Meta.length; _ } -> (file, length))
              files meta.Meta.files
          in
          piece_bounds ~piece_offset ~piece_length files
        in
        let buf = Cstruct.of_string buf in
        List.iter
          (fun (file, file_offset, ofs, len) ->
            let file_offset = Optint.Int63.of_int file_offset in
            Eio.Fs.pwrite_exact file ~file_offset (Cstruct.sub buf ofs len))
          bounds;
        completed := !completed + 1;
        let percent = float !completed /. float num_pieces *. 100.0 in
        Logs.app (fun f ->
            f "(%0.2f%%) Downloaded piece #%d" percent piece.Piece.i)
      done)
