open Printf
open Messages
open Monitor
(* open Spawn *)
(* open Lwt *)

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

module M = Map.Make (Torrent.Digest)

let string_of_msg = function
  | TrackerStat stats ->
    sprintf "TrackerStat: info_hash: %s%s%s"
      (Torrent.Digest.to_string stats.track_info_hash)
      (Util.map_some "" (fun ic -> " incomplete: " ^ string_of_int ic)
        stats.track_incomplete)
      (Util.map_some "" (fun co -> " complete: " ^ string_of_int co)
        stats.track_complete)
  | CompletedPiece _ -> "CompletedPiece"
  | InsertTorrent (ih, left) ->
    sprintf "InsertTorrent: info_hash: %s left: %Ld B"
      (Torrent.Digest.to_string ih) left
  | RemoveTorrent _ -> "RemoveTorrent"
  | TorrentCompleted _ -> "TorrentCompleted"
  | RequestStatus (ih, _) ->
    sprintf "RequestStatus: info_hash: %s"
      (Torrent.Digest.to_string ih)
  | RequestAllTorrents _ -> "RequestAllTorrents"
  | StatusTimerTick -> "StatusTimerTick"

exception UnknownInfoHash of Torrent.Digest.t

let adjust f k m =
  (* FIXME throw UnknownInfoHash if info_hash does not
   * exist *)
  M.add k (f (M.find k m)) m

let handle_message id msg m : Messages.state M.t Lwt.t =
  debug id "%s" (string_of_msg msg) >>
  match msg with
  | TrackerStat stats ->
    Lwt.return (adjust (fun st ->
      {st with
        incomplete = stats.track_incomplete;
        complete = stats.track_complete}) stats.track_info_hash m)
  | InsertTorrent (ih, l) ->
    (* FIXME what if it is duplicate? *)
    Lwt.return (M.add ih
      { uploaded = 0L; downloaded = 0L; left = l;
        incomplete = None; complete = None;
        state =
          if l = 0L then
            Torrent.Seeding
          else
            Torrent.Leeching } m)
  | RequestStatus (ih, v) ->
    begin try
      let st = M.find ih m in
      Lwt_mvar.put v st >> Lwt.return m
    with
    | Not_found -> raise_lwt (UnknownInfoHash ih)
    end
  | _ ->
    failwith "unhandled message"

let start ~monitor ~status_ch =
  let event_loop id =
    Lwt_stream.fold_s (handle_message id) status_ch M.empty >>= fun _ ->
    Lwt.return ()
  in
  Monitor.spawn ~parent:monitor ~name:"Status" event_loop
  (* Supervisor.spawn "Status" w_supervisor_ch event_loop *)
