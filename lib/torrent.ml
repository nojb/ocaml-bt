module Piece = struct
  type t =
    {
      index: int;
      hash: string;
      length: int;
    }
end

(* let download_from_peer ~peer ~work_queue ~results = *)
(*   let peer = Peer.run ~net ~clock ~info_hash ~peer_id peer in *)
(*   Peer.send_unchoke peer; *)
(*   Peer.send_interested peer; *)
(*   let rec loop () = *)
(*     let piece = Eio.Stream.take work_queue in *)
(*     if not (Peer.has_piece peer index) then Eio.Stream.add work_queue piece *)
(*     else *)
(*       let res = try_download_from_peer ~peer ~piece ~results in *)

let download_from_peer ~net ~clock ~info_hash ~peer_id ~peer ~work_queue:_ ~results:_ =
  Eio.Switch.run @@ fun sw ->
  match peer.Tracker.Response.Peer.ip with
  | `Ipaddr addr ->
      let peer = Peer.run ~net ~clock ~sw ~info_hash ~peer_id addr peer.Tracker.Response.Peer.port in
      begin match peer with
      | Error err ->
          Format.printf "Connection to %s failed: %s.@." (Unix.string_of_inet_addr addr)
            (Peer.string_of_error err)
      | Ok _peer ->
          Format.printf "Connection to %s OK.@." (Unix.string_of_inet_addr addr)
      end
  | `Name _ ->
      ()

let download ~net ~clock ~info_hash ~peer_id ~meta ~peers =
  let num_pieces = Array.length meta.Metainfo.pieces in
  let work_queue = Eio.Stream.create num_pieces in
  let results = Eio.Stream.create 20 in
  for index = 0 to num_pieces - 1 do
    let hash = meta.Metainfo.pieces.(index) in
    let length = Metainfo.piece_length meta index in
    Eio.Stream.add work_queue {Piece.index; hash; length}
  done;
  Eio.Fiber.iter (fun peer -> download_from_peer ~net ~clock ~info_hash ~peer_id ~peer ~work_queue ~results) peers
