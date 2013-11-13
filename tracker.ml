open Printf
(* open Messages *)

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let failwith_lwt msg =
  raise_lwt (Failure msg)

let string_of_msg = function
  | Msg.Stop -> "Stop"
  | Msg.TrackerTick n -> sprintf "TrackerTick: %d" n
  | Msg.Start -> "Start"
  | Msg.Complete -> "Complete"

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

type t = {
  send_status           : Msg.status_msg option -> unit;
  send          : Msg.tracker_msg option -> unit;
  send_peer_mgr         : Msg.peer_mgr_msg option -> unit;
  info_hash             : Torrent.Digest.t;
  peer_id               : Torrent.peer_id;
  tier                  : Uri.t array;
  local_port            : int;
  mutable status        : status;
  mutable next_tick     : int;
  id : Proc.Id.t
}

let debug t ?exn fmt =
  Printf.ksprintf (fun msg ->
    Lwt_log.debug_f ?exn "Tracker %s: %s" (Proc.Id.to_string t.id) msg) fmt

(** [timer_update id c st] is a Lwt thread that updates the [tick] counter
    of [st] and launches a asynchronous thread to ping the tracker thread
    after [st.interval] seconds.
    @returns [st] updated with the new [tick] counter *)
let timer_update t (interval, _) : unit Lwt.t = (* (interval, min_interval) = *)
  match t.status with
  | Running ->
    let nt = t.next_tick in
    t.next_tick <- nt+1;
    (* run *)
    let _ = Proc.spawn
      (fun _ ->
        Lwt_unix.sleep (float interval) >|= fun () ->
        t.send (Some (Msg.TrackerTick nt))) in
    debug t "Set Timer to: %d" interval >>
    Lwt.return_unit
  | _ ->
    Lwt.return_unit

let try_udp_server t ss url =
  let fresh_transaction_id () = Random.int32 Int32.max_int in
  let socket = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0 in
  let host = match Uri.host url with
    | None -> failwith "Empty Hostname"
    | Some host -> host
  in
  let port = match Uri.port url with
    | None -> failwith "Empty Port"
    | Some port -> port
  in
  lwt he = Lwt_unix.gethostbyname host in
  let iaddr = he.Lwt_unix.h_addr_list.(0) in
  let saddr = Lwt_unix.ADDR_INET (iaddr, port) in
  Lwt_unix.connect socket saddr >>
  let ic = Lwt_io.of_fd ~mode:Lwt_io.input socket in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.output socket in
  let rec connect_response trans_id () =
    lwt n = Lwt_io.BE.read_int32 ic in
    assert (n = 0l || n = 3l);
    if n = 3l then (* error *)
      lwt msg = Lwt_io.read ic in
      Lwt.return (Error msg)
    else begin
      lwt trans_id' = Lwt_io.BE.read_int32 ic in
      assert (Int32.compare trans_id trans_id' = 0);
      lwt conn_id = Lwt_io.BE.read_int64 ic in
      request_announce conn_id 0
    end
  and announce_response trans_id () =
    lwt n = Lwt_io.BE.read_int32 ic in
    assert (n = 1l || n = 3l);
    if n = 3l then (* error *)
      lwt msg = Lwt_io.read ic in
      Lwt.return (Error msg)
    else begin
      lwt trans_id' = Lwt_io.BE.read_int32 ic in
      assert (Int32.compare trans_id trans_id' = 0);
      lwt interval = Lwt_io.read_int32 ic in
      lwt leechers = Lwt_io.read_int32 ic in
      lwt seeders = Lwt_io.read_int32 ic in
      let rec loop () = (* FIXME tail-recursive *)
        try_lwt
          let ip = String.create 4 in
          Lwt_io.read_into_exactly ic ip 0 4 >>
          (* lwt ip = Util.inet_addr_of_int64 (Lwt_io.read_int64 ic) in *)
          let ip = Unix.inet_addr_of_string (sprintf "%03d.%03d.%03d.%03d"
            (int_of_char ip.[0]) (int_of_char ip.[1]) (int_of_char ip.[2])
            (int_of_char ip.[3]))
          in
          lwt port = Lwt_io.read_int16 ic in
          lwt peers = loop () in
          Lwt.return ((ip, port) :: peers)
        with
        | End_of_file -> Lwt.return_nil
      in
      lwt peers = loop () in
      Lwt.return (Success
        { new_peers = peers;
          complete = Some (Int32.to_int seeders); (* safe in 64-bit *)
          incomplete = Some (Int32.to_int leechers); (* safe in 64-bit *)
          interval = Int32.to_int interval; (* safe in 64-bit *)
          min_interval = None })
    end
  and request_announce conn_id n =
      let trans_id = fresh_transaction_id () in
      Lwt_io.BE.write_int64 oc conn_id >>
      Lwt_io.BE.write_int32 oc 1l >> (* announce *)
      Lwt_io.BE.write_int32 oc trans_id >>
      Lwt_io.write oc (Torrent.Digest.to_bin t.info_hash) >>
      Lwt_io.write oc (Torrent.PeerId.to_string t.peer_id) >>
      Lwt_io.BE.write_int64 oc ss.Msg.downloaded >>
      Lwt_io.BE.write_int64 oc ss.Msg.left >>
      Lwt_io.BE.write_int64 oc ss.Msg.uploaded >>
      Lwt_io.BE.write_int32 oc
        begin match t.status with
        | Running -> 0l
        | Completed -> 1l
        | Started -> 2l
        | Stopped -> 3l
        end >>
      Lwt_io.BE.write_int32 oc 0l >>
      Lwt_io.BE.write_int32 oc 0l >>
      Lwt_io.BE.write_int32 oc (Int32.of_int (-1)) >>
      Lwt_io.BE.write_int16 oc t.local_port >>
      try_lwt
        Lwt_unix.with_timeout (15.0 *. 2.0 ** float n)
          (announce_response trans_id)
      with
      | Lwt_unix.Timeout ->
        debug t "UDP Announce Request Timeout after %d s; Retrying..."
          (truncate (15.0 *. 2.0 ** float n)) >>
        if n >= 2 then request_connect (n+1)
        else request_announce conn_id (n+1)
  and request_connect n =
    let trans_id = fresh_transaction_id () in
    Lwt_io.BE.write_int64 oc 0x41727101980L >>
    Lwt_io.BE.write_int32 oc 0l >> (* connect *)
    Lwt_io.BE.write_int32 oc trans_id >>
    try_lwt
      Lwt_unix.with_timeout (15.0 *. 2.0 ** float n) (connect_response trans_id)
    with
    | Lwt_unix.Timeout ->
      if n >= 8 then
        failwith_lwt "Too many retries"
      else
        debug t "UDP Connect Request Timeout after %d s; Retrying..."
          (truncate (15.0 *. 2.0 ** float n)) >>
        request_connect (n+1)
  in request_connect 0

let decode_http_response (d : Bcode.t) =
  let decode_success () =
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
    Success
      { new_peers = peers;
        complete = Some co;
        incomplete = Some ic;
        interval = iv;
        min_interval = mi }
  in
  try
    Warning (Bcode.search_string "warning" d)
  with
  | Not_found ->
    try
      Error (Bcode.search_string "error" d)
    with
    | Not_found ->
      decode_success ()

let try_http_server t ss (uri : Uri.t) : response Lwt.t =
  let uri =
    let params =
      ("info_hash",   (Torrent.Digest.to_bin t.info_hash)) ::
      ("peer_id",     (Torrent.PeerId.to_string t.peer_id)) ::
      ("uploaded",    Int64.to_string ss.Msg.uploaded) ::
      ("downloaded",  Int64.to_string ss.Msg.downloaded) ::
      ("left",        Int64.to_string ss.Msg.left) ::
      ("port",        string_of_int t.local_port) ::
      match t.status with
      | Running -> []
      | Completed -> ("event", "completed") :: []
      | Started -> ("event", "started") :: []
      | Stopped -> ("event", "stopped") :: []
    in
    Uri.add_query_params' uri params
  in
  match_lwt Cohttp_lwt_unix.Client.get uri with
  | None -> raise_lwt (HTTPError "no response")
  | Some (resp, body) ->
    lwt body = Cohttp_lwt_body.string_of_body body in
    (* debug id "Got tracker response" >> *)
    let dict = Bcode.from_string body in
    Lwt.return (decode_http_response dict)
    (* if not (Cohttp_lwt_unix.Response.has_body resp) then *)
    (*   let err = "Response has no body" in *)
    (*   Log.debug err >> fail (HTTPError err) (* return (Left err) *) *)
    (* else *)
    (*   Log.debug "Got good response!" >> (* decode answer *) assert false *)

let try_server t ss url =
  debug t "Querying Tracker: %s" (Uri.to_string url) >>
  match Uri.scheme url with
  | Some "http" ->
    try_http_server t ss url
  | Some "udp" ->
    try_udp_server t ss url
  | Some other -> failwith_lwt (sprintf "Unknown Tracker Scheme: %S" other)
  | None -> failwith_lwt "No Tracker Scheme Specified"

let query_trackers t ss : response Lwt.t =
  let rec loop i =
    if i >= Array.length t.tier then
      raise_lwt EmptyAnnounceList
    else
      try_lwt
        lwt resp = try_server t ss t.tier.(i) in
        let tmp = t.tier.(0) in
        t.tier.(0) <- t.tier.(i);
        t.tier.(i) <- tmp;
        Lwt.return resp
      with
      | Lwt.Canceled ->
        raise_lwt Lwt.Canceled
      | exn ->
        debug t ~exn "Failed" >>= fun () ->
        loop (i+1)
  in
  loop 0

(** [poke_tracker id c st] tries to contact the tracker and
    asks for peers for the Torrent specified by [c.torrent_info].
    Before doing this, it asks the [Status] thread to know how many
    bytes are left to download. If the tracker request is successful,
    a new timer is setup for the interval time requested by the tracker,
    otherwise a default is used.
    @returns st the state updated with new interval times and [status] *)
let poke_tracker t : (int * int option) Lwt.t =
  let mv = Lwt_mvar.create_empty () in
  let ih = t.info_hash in
  t.send_status (Some (Msg.RequestStatus (ih, mv)));
  lwt ss = Lwt_mvar.take mv in
  try_lwt
    match_lwt query_trackers t ss with
    | Warning warn ->
      debug t "Tracker Warning Response: %s" warn >>
      Lwt.return (fail_timer_interval, None)
    | Error err ->
      debug t "Tracker Error Response: %s" err >>
      Lwt.return (fail_timer_interval, None)
    | Success ok ->
      debug t "Received %d peers" (List.length ok.new_peers) >>= fun () ->
      t.send_peer_mgr (Some (Msg.PeersFromTracker (ih, ok.new_peers)));
      t.send_status (Some (Msg.TrackerStat (ih, ok.complete, ok.incomplete)));
      begin match t.status with
      | Running   -> t.status <- Running
      | Stopped   -> t.status <- Stopped
      | Completed -> t.status <- Running
      | Started   -> t.status <- Running
      end;
      Lwt.return (ok.interval, ok.min_interval)
  with
  | HTTPError err ->
    debug t "Tracker HTTP Error: %s" err >>
    Lwt.return (fail_timer_interval, None)
  | DecodeError ->
    debug t "Response Decode Error" >>
    Lwt.return (fail_timer_interval, None)

let handle_message t msg : unit Lwt.t =
  debug t "%s" (string_of_msg msg) >>
  match msg with
  | Msg.TrackerTick x ->
    if x+1 = t.next_tick then
      poke_tracker t >>= timer_update t
    else
      Lwt.return_unit
  | Msg.Stop ->
    t.status <- Stopped;
    poke_tracker t >>= timer_update t
  | Msg.Start ->
    t.status <- Started;
    poke_tracker t >>= timer_update t
  | Msg.Complete ->
    t.status <- Completed;
    poke_tracker t >>= timer_update t

let shuffle_array a =
  for i = (Array.length a)-1 downto 1 do
    let j = Random.int (i+1) in
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
  done

let start ~send_super ~msgs ~send ~info_hash ~peer_id ~local_port
  ~tier ~send_status ~send_peer_mgr =
  let run id =
    let t = {
      send;
      send_status;
      send_peer_mgr;
      info_hash;
      peer_id;
      tier = Array.of_list tier;
      local_port;
      status        = Stopped;
      next_tick     = 0;
      id
    }
    in
    shuffle_array t.tier;
    Lwt_stream.iter_s (handle_message t) msgs
  in
  let id =
    Proc.spawn (Proc.cleanup run
      (Super.default_stop send_super)
      (fun _ -> Lwt.return_unit))
  in
  id
