open Printf

let (>>=) = Lwt.(>>=)

let debug = Proc.debug

type state = {
  uploaded        : int64;
  downloaded      : int64;
  left            : int64;
  incomplete      : int option;
  complete        : int option;
  state           : Info.state
}

type msg =
  [ `UpdateStats         of int option * int option
  | `CompletedPiece      of int
  | `TorrentCompleted
  | `RequestStatus       of state Lwt_mvar.t
  | `Tick ]

type t = {
  info_hash : Info.Digest.t;
  id : Proc.Id.t
}

let string_of_msg = function
  | `UpdateStats (Some complete, Some incomplete) ->
    sprintf "UpdateStat seeders: %d leechers: %d" complete incomplete
  | `UpdateStats (None, Some leechers) ->
    sprintf "UpdateStats leechers: %d" leechers
  | `UpdateStats (Some seeders, None) ->
    sprintf "UpdateStats seeders %d" seeders
  | `UpdateStats (None, None) ->
    "UpdateStats"
  | `CompletedPiece i ->
    sprintf "CompletedPiece index: %d" i
  | `TorrentCompleted ->
    "TorrentCompleted"
  | `RequestStatus _ ->
    "RequestStatus"
  | `Tick ->
    "Tick"

let handle_message t msg st : state Lwt.t =
  debug t.id "%s" (string_of_msg msg) >>= fun () ->
  match msg with
  | `UpdateStats (complete, incomplete) ->
    Lwt.return {st with complete; incomplete};
  | `RequestStatus mv ->
    Lwt_mvar.put mv st >>= fun () ->
    Lwt.return st
  | _ ->
    debug t.id "Unhandled: %s" (string_of_msg msg) >>= fun () ->
    Lwt.return st

let start ~ch ~info_hash ~left =
  let run id =
    let st =
      { uploaded = 0L;
        downloaded = 0L;
        left;
        incomplete = None;
        complete = None;
        state =
          if Int64.(compare left 0L) = 0 then Info.Seeding
          else Info.Leeching }
    in
    let t = { info_hash; id } in
    Lwt_pipe.fold_s (handle_message t) ch st >>= fun _ ->
    Lwt.return_unit
  in
  Proc.spawn ~name:"Status" run
