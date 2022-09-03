module Piece = struct
  type t = { index : int; hash : string; length : int }
end

let check_integriy piece data =
  piece.Piece.hash = Sha1.to_bin (Sha1.string data)

let max_block_size = 16384
let max_backlog = 5

let attempt_download_piece ~clock ~peer ~piece =
  let downloaded = ref 0 in
  let requested = ref 0 in
  let backlog = ref 0 in
  let buf = Bytes.create piece.Piece.length in
  Eio.Time.with_timeout_exn clock 30.0 @@ fun () ->
  while !downloaded < piece.Piece.length do
    if not (Peer.choked peer) then
      while !backlog < max_backlog && !requested < piece.Piece.length do
        let block_size = min max_block_size (piece.Piece.length - !requested) in
        Peer.send_request peer ~i:piece.Piece.index ~ofs:!requested
          ~len:block_size;
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
  Bytes.unsafe_to_string buf

let start_download_worker ~net ~clock ~info_hash ~peer_id ~peer ~work_queue
    ~results =
  Eio.Switch.run @@ fun sw ->
  match peer.Tracker.Response.Peer.ip with
  | `Ipaddr addr -> (
      let peer =
        Peer.run ~net ~clock ~sw ~info_hash ~peer_id addr
          peer.Tracker.Response.Peer.port
      in
      match peer with
      | Error err ->
          Eio.traceln "Connection to %s failed: %s"
            (Unix.string_of_inet_addr addr)
            (Peer.string_of_error err)
      | Ok peer ->
          Eio.traceln "Completed handshake with %s"
            (Unix.string_of_inet_addr addr);
          Peer.send_unchoke peer;
          Peer.send_interested peer;
          let rec loop () =
            let piece = Eio.Stream.take work_queue in
            if not (Peer.has_piece peer piece.Piece.index) then (
              Eio.Stream.add work_queue piece;
              loop ())
            else
              let buf = attempt_download_piece ~clock ~peer ~piece in
              if check_integriy piece buf then (
                Eio.traceln "Downloaded piece #%d" piece.Piece.index;
                Eio.Stream.add results (piece, buf))
              else (
                Eio.traceln "Piece #%d failed integrity check" piece.Piece.index;
                Eio.Stream.add work_queue piece);
              loop ()
          in
          loop ())
  | `Name _ -> ()

let download ~net ~clock ~info_hash ~peer_id ~meta ~peers =
  let num_pieces = Array.length meta.Metainfo.pieces in
  let work_queue = Eio.Stream.create num_pieces in
  let results = Eio.Stream.create 100 in
  for index = 0 to num_pieces - 1 do
    let hash = meta.Metainfo.pieces.(index) in
    let length = Metainfo.piece_length meta index in
    Eio.Stream.add work_queue { Piece.index; hash; length }
  done;
  Eio.Fiber.both
    (fun () ->
       Eio.Fiber.iter
         (fun peer ->
            start_download_worker ~net ~clock ~info_hash ~peer_id ~peer ~work_queue
              ~results)
         peers
    )
    (fun () ->
       let completed = ref 0 in
       while !completed < num_pieces do
         let piece, _buf = Eio.Stream.take results in
         (* let ofs = Metainfo.piece_offset meta piece.Piece.index in *)
         completed := !completed + 1;
         let percent = float !completed /. float num_pieces in
         Eio.traceln "(%0.2f%%) Downloaded piece #%d" percent piece.Piece.index
       done
    )
