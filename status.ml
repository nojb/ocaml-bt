open Printf

let (>>=) = Lwt.(>>=)

module H = Hashtbl.Make (Torrent.Digest)

type t = {
  torrents : Msg.state H.t;
  id : Proc.Id.t
}

let debug t fmt =
  Printf.ksprintf (fun msg ->
    Lwt_log.debug_f "Status %s: %s" (Proc.Id.to_string t.id) msg) fmt

let string_of_msg = function
  | Msg.TrackerStat (ih, complete, incomplete) ->
    sprintf "TrackerStat: info_hash: %s%s%s"
      (Torrent.Digest.to_string ih)
      (Util.map_some "" (fun ic -> " incomplete: " ^ string_of_int ic)
        incomplete)
      (Util.map_some "" (fun co -> " complete: " ^ string_of_int co)
        complete)
  | Msg.CompletedPiece _ -> "CompletedPiece"
  | Msg.InsertTorrent (ih, left) ->
    sprintf "InsertTorrent: info_hash: %s left: %Ld B"
      (Torrent.Digest.to_string ih) left
  | Msg.RemoveTorrent _ -> "RemoveTorrent"
  | Msg.TorrentCompleted _ -> "TorrentCompleted"
  | Msg.RequestStatus (ih, _) ->
    sprintf "RequestStatus: info_hash: %s"
      (Torrent.Digest.to_string ih)
  | Msg.RequestAllTorrents _ -> "RequestAllTorrents"
  | Msg.StatusTimerTick -> "StatusTimerTick"

exception UnknownInfoHash of Torrent.Digest.t

let handle_message t msg : unit Lwt.t =
  debug t "%s" (string_of_msg msg) >>= fun () ->
  match msg with
  | Msg.TrackerStat (ih, complete, incomplete) ->
    let st = H.find t.torrents ih in
    H.replace t.torrents ih {st with Msg.complete; Msg.incomplete};
    Lwt.return_unit
  | Msg.InsertTorrent (ih, left) ->
    (* FIXME what if it is duplicate? *)
    H.add t.torrents ih
      { Msg.uploaded = 0L; Msg.downloaded = 0L; Msg.left;
        Msg.incomplete = None; Msg.complete = None;
        Msg.state =
          if left = 0L then
            Torrent.Seeding
          else
            Torrent.Leeching };
    Lwt.return_unit
  | Msg.RequestStatus (ih, mv) ->
    begin try
      let st = H.find t.torrents ih in
      Lwt_mvar.put mv st >>= fun () ->
      Lwt.return_unit
    with
    | Not_found -> raise_lwt (UnknownInfoHash ih)
    end
  | _ ->
    failwith "unhandled message"

let start ~super_ch ~ch =
  let run id =
    let t = { torrents = H.create 17; id } in
    Lwt_pipe.iter_s (handle_message t) ch
  in
  Proc.spawn
    (Proc.cleanup run (Super.default_stop super_ch) (fun _ ->
      Lwt.return_unit))
