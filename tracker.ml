open Printf
(* open Lwt *)
(* open Spawn *)
open Monitor
open Messages

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let string_of_msg = function
  | Stop -> "Stop"
  | TrackerTick n -> sprintf "TrackerTick: %d" n
  | Start -> "Start"
  | Complete -> "Complete"

type status =
  | Started
  | Stopped
  | Completed
  | Running

type peer =
  Unix.inet_addr * int

type reply = {
  new_peers     : peer list;
  complete      : int option;
  incomplete    : int option;
  interval      : int;
  min_interval  : int option
}

exception HTTPError of string
exception DecodeError
exception EmptyAnnounceList

type response =
  | Success of reply
  | Warning of string
  | Error of string

let fail_timer_interval = 15 * 60

type state = {
  w_status_ch   : status_msg -> unit;
  (* tracker_ch    : tracker_msg Lwt_stream.t; *)
  w_tracker_ch  : tracker_msg -> unit;
  w_peer_mgr_ch : peer_mgr_msg -> unit;
  (* peer_mgr_ch   : peer_mgr_msg Lwt_tream.t; *)
  torrent_info  : Torrent.info;
  peer_id       : Torrent.peer_id;
  announce_list : Uri.t list list;
  status        : status;
  local_port    : int;
  next_tick     : int
}

let decode_http_response (d : Bcode.t) =
  let co = Bcode.search_int' "complete" d in
  let ic = Bcode.search_int' "incomplete" d in
  let iv = Bcode.search_int' "interval" d in
  let mi = Bcode.search_maybe_int' "min interval" d in
  let peers =
    try
      match Bcode.search "peers" d with
      | Bcode.BList pr ->
        List.map (fun d ->
          let ip = Bcode.search_string "ip" d in
          let port = Bcode.search_int' "port" d in
          let addr = Unix.inet_addr_of_string ip in
          (addr, port)) pr
      | Bcode.BString pr ->
        let rec loop i =
          if i >= String.length pr then []
          else
            (Unix.inet_addr_of_string (sprintf "%03d.%03d.%03d.%03d"
              (int_of_char pr.[i+0]) (int_of_char pr.[i+1])
              (int_of_char pr.[i+2]) (int_of_char pr.[i+3])),
              (int_of_char pr.[i+4] lsl 8 + int_of_char pr.[i+5])) :: loop (i+6)
        in loop 0
      | _ ->
        raise DecodeError
    with
    | _ -> raise DecodeError
  in
  { new_peers = peers; complete = Some co; incomplete = Some ic;
    interval = iv; min_interval = mi }

(* let process_result_dict (d : Bcode.t) : response = *)
(*   match Bcode.tracker_error d with *)
(*   | None -> *)
(*       begin match Bcode.tracker_warning d with *)
(*       | None -> *)
(*           begin try ResponseOk (decode_ok d) *)
(*           with _ -> DecodeError "Could not decode response properly" *)
(*           end *)
(*       | Some warn -> *)
(*           Warning warn *)
(*       end *)
(*   | Some err -> *)
(*       Error err *)

(** [timer_update id c st] is a Lwt thread that updates the [tick] counter
of [st] and launches a asynchronous thread to ping the tracker thread
after [st.interval] seconds.
@returns [st] updated with the new [tick] counter *)
let timer_update id (st, interval, _) : state Lwt.t = (* (interval, min_interval) = *)
  match st.status with
  | Running ->
    let t = st.next_tick in
    let st = {st with next_tick = t+1} in
    (* run *)
    spawn ~name:"TrackerTimer"
      (fun _ ->
        Lwt_unix.sleep (float interval) >>
        (st.w_tracker_ch (TrackerTick t); Lwt.return ()));
    debug id "Set Timer to: %d" interval >>
    Lwt.return st
  | _ ->
    Lwt.return st

(** [build_request_uri c st ss url] builds the HTTP request URI.
It uses information from a variety of sources, the peer id, info_hash,
information gained from [Status], the tracker URL *)
let build_request_uri st ss url : Uri.t =
  let params =
    ("info_hash",   (Torrent.Digest.to_bin st.torrent_info.Torrent.info_hash)) ::
    ("peer_id",     (Torrent.PeerId.to_string st.peer_id)) ::
    ("uploaded",    Int64.to_string ss.uploaded) ::
    ("downloaded",  Int64.to_string ss.downloaded) ::
    ("left",        Int64.to_string ss.left) ::
    ("port",        string_of_int st.local_port) ::
    match st.status with
    | Running -> []
    | Completed -> ("event", "completed") :: []
    | Started -> ("event", "started") :: []
    | Stopped -> ("event", "stopped") :: []
  in
  Uri.add_query_params' url params

let request_http_tracker id _ (uri : Uri.t) : response Lwt.t =
  match_lwt Cohttp_lwt_unix.Client.get uri with
  | None -> raise_lwt (HTTPError "no response")
  | Some (resp, body) ->
    lwt body = Cohttp_lwt_body.string_of_body body in
    debug id "Got tracker response" >>
    let dict = Bcode.from_string body in
    Lwt.return (Success (decode_http_response dict))
    (* if not (Cohttp_lwt_unix.Response.has_body resp) then *)
    (*   let err = "Response has no body" in *)
    (*   Log.debug err >> fail (HTTPError err) (* return (Left err) *) *)
    (* else *)
    (*   Log.debug "Got good response!" >> (* decode answer *) assert false *)

let tracker_request id st uri =
  debug id "Querying URI: %s" (Uri.to_string uri) >>
  match Uri.scheme uri with
  | None
  | Some "http" -> request_http_tracker id st uri
  | Some scheme -> failwith (sprintf "Scheme \"%s\" Unsupported" scheme)

let bubble_up_url st url tier =
  (* FIXME *)
  st
  (* let rec loop = function *)
  (* | [] -> [] *)
  (* | x :: xs -> *)
  (*     if x == url then xs *)
  (*     else x :: loop xs *)
  (* in url :: loop tier *)

let rec try_this_tier' id st ss = function
  | [] ->
    raise_lwt EmptyAnnounceList
  | x :: [] ->
    let uri = build_request_uri st ss x in
    lwt resp = tracker_request id st uri in
    Lwt.return (uri, resp)
  | x :: xs ->
    let uri = build_request_uri st ss x in
    try_lwt
      lwt resp = tracker_request id st uri in
      Lwt.return (uri, resp)
    with
    | _ -> try_this_tier' id st ss xs

let try_this_tier id st ss p =
  lwt url, resp = try_this_tier' id st ss p in
  let st = bubble_up_url st url p in
  Lwt.return (st, resp)
  (* try_this_tier' ss p >>= fun (url, resp) -> *)
  (* FIXME *)
  (* put (fun st -> {st with announce_list = bubble_up_url url ...}) >| *)
  (* return resp *)

(**
@returns the new state (with the tier rearranged to reflect
success) and the tracker response.
*)
let query_trackers id st ss : (state * response) Lwt.t =
  (* TODO have to reload the state in each loop iteration? *)
  let rec loop = function
  | [] -> raise_lwt EmptyAnnounceList
  | x :: [] -> try_this_tier id st ss x
  | x :: xs -> try_lwt try_this_tier id st ss x with _ -> loop xs
  in loop st.announce_list

(** [poke_tracker id c st] tries to contact the tracker and
asks for peers for the Torrent specified by [c.torrent_info].
Before doing this, it asks the [Status] thread to know how many
bytes are left to download. If the tracker request is successful,
a new timer is setup for the interval time requested by the tracker,
otherwise a default is used.
@returns st the state updated with new interval times and [status] *)
let poke_tracker id st : (state * int * int option) Lwt.t =
  let v = Lwt_mvar.create_empty () in
  let ih = st.torrent_info.Torrent.info_hash in
  st.w_status_ch (RequestStatus (ih, v));
  lwt ss = Lwt_mvar.take v in
  try_lwt
    lwt st, msg = query_trackers id st ss in
    match msg with
    | Warning warn ->
      debug id "Tracker Warning Response: %s" warn >>
      Lwt.return (st, fail_timer_interval, None)
    | Error err ->
      debug id "Tracker Error Response: %s" err >>
      Lwt.return (st, fail_timer_interval, None)
    | Success ok ->
      lwt () = debug id "Received %d peers" (List.length ok.new_peers) in
      st.w_peer_mgr_ch (PeersFromTracker (ih, ok.new_peers));
      let stats = {
        track_info_hash = ih;
        track_complete = ok.complete;
        track_incomplete = ok.incomplete
      } in
      st.w_status_ch (TrackerStat stats);
      let new_status = match st.status with
        | Running -> Running
        | Stopped -> Stopped
        | Completed -> Running
        | Started -> Running
      in
      Lwt.return ({st with status = new_status}, ok.interval, ok.min_interval)
  with
  | HTTPError err ->
    debug id "Tracker HTTP Error: %s" err >>
    Lwt.return (st, fail_timer_interval, None)
  | DecodeError ->
    debug id "Response Decode Error" >>
    Lwt.return (st, fail_timer_interval, None)

let handle_message id msg st : state Lwt.t =
  (* lwt msg = Lwt_stream.next st.tracker_ch in *)
  debug id "%s" (string_of_msg msg) >>
  match msg with
  | TrackerTick x ->
    debug id "ticking: %d" x >>
    if x+1 = st.next_tick then
      poke_tracker id st >>= timer_update id
    else
      Lwt.return st
  | Stop ->
    let st = {st with status = Stopped} in
    poke_tracker id st >>= timer_update id
  | Start ->
    let st = {st with status = Started} in
    poke_tracker id st >>= timer_update id
  | Complete ->
    let st = {st with status = Completed} in
    poke_tracker id st >>= timer_update id

let start ~monitor ~torrent_info ~peer_id ~local_port ~w_status_ch
  ~tracker_ch ~w_tracker_ch ~w_peer_mgr_ch =
  let st = {
    w_status_ch;
    (* tracker_ch; *)
    w_tracker_ch;
    w_peer_mgr_ch;
    torrent_info;
    peer_id;
    (* w_status_ch; *)
    (* tracker_ch; *)
    (* w_tracker_ch; *)
    (* w_peer_mgr_ch; *)
    (* torrent_info; *)
    announce_list = torrent_info.Torrent.announce_list;
    (* peer_id; *)
    status        = Stopped;
    local_port;
    next_tick     = 0
  }
  in
  let event_loop id =
    Lwt_stream.fold_s (handle_message id) tracker_ch st >>= fun _ ->
    Lwt.return ()
  in
  (* handle_message id st >>= event_loop id *)
  Monitor.spawn ~parent:monitor ~name:"Tracker" event_loop
  (* Supervisor.spawn "Tracker" w_supervisor_ch event_loop *)
  (* (fun id -> event_loop id) *)
  (* spawn ~name:"Tracker" st (loop ()) *)
  (* FIXME cleanup *)
  (* Supervisor.spawn ~name:"Tracker" w_supervisor_ch st (loop ()) *)

