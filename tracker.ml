open Printf

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let debug = Proc.debug

let failwith_lwt msg =
  raise_lwt (Failure msg)

type msg =
  | Stop
  | TrackerTick of int
  | Start
  | Complete

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

type t = {
  status_ch           : Status.msg Lwt_pipe.t;
  ch          : msg Lwt_pipe.t;
  peer_mgr_ch         : Msg.peer_mgr_msg Lwt_pipe.t;
  info_hash             : Info.Digest.t;
  peer_id               : Info.peer_id;
  tier                  : Uri.t array;
  local_port            : int;
  mutable status        : status;
  mutable next_tick     : int;
  id : Proc.Id.t
}

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
        Lwt_pipe.write t.ch (TrackerTick nt)) in
    debug t.id "Set Timer to: %d" interval >>
    Lwt.return_unit
  | _ ->
    Lwt.return_unit

let udp_send fd buf =
  Lwt_unix.write fd buf 0 (String.length buf) >>= fun len ->
  if len <> String.length buf then
    failwith_lwt "udp_send: could not send entire packet"
  else
    Lwt.return_unit

let udp_packet_length = 512

let read_udp_packet fd =
  let buf = String.create udp_packet_length in
  Lwt_unix.read fd buf 0 udp_packet_length >>= fun len ->
  Lwt.return (String.sub buf 0 len)

let udp_fresh_transaction_id () =
  Random.int32 Int32.max_int

let rec udp_request_connect self fd ss n =
  let trans_id = udp_fresh_transaction_id () in
  let write_packet =
    let open Put in
    BE.int64 0x41727101980L >>=
    BE.int32 0l >>=
    BE.int32 trans_id
  in
  udp_send fd (Put.run write_packet) >>= fun () ->
  let handle_error = function
    | Unix.Unix_error (Unix.ETIMEDOUT, _, _) ->
      if n >= 8 then
        failwith_lwt "udp_request_connect: too many retries"
      else
        debug self.id "UDP Connect Request Timeout after %d s; Retrying..."
          (truncate (15.0 *. 2.0 ** float n)) >>= fun () ->
        udp_request_connect self fd ss (n+1)
    | exn -> Lwt.fail exn
  in
  Lwt_unix.setsockopt_float fd Lwt_unix.SO_RCVTIMEO (15.0 *. 2.0 ** float n);
  Lwt.catch (fun () -> udp_connect_response self fd ss trans_id) handle_error

and udp_connect_response self fd ss trans_id =
  read_udp_packet fd >>= fun buf ->
  let read_packet : response Lwt.t Get.t =
    let open Get in
    BE.int32 >>= fun n ->
    if n = 3l then
      string >|= fun msg -> Lwt.return (Error msg)
    else begin
      BE.int32 >>= fun trans_id' ->
      assert (Int32.compare trans_id trans_id' = 0);
      BE.int64 >|= fun conn_id ->
      udp_request_announce self fd ss conn_id 0
    end
  in
  Get.run read_packet buf

and udp_request_announce self fd ss conn_id n =
  let trans_id = udp_fresh_transaction_id () in
  let create_packet =
    let open Put in
    BE.int64 conn_id >>=
    BE.int32 1l >>=
    BE.int32 trans_id >>=
    string (Info.Digest.to_bin self.info_hash) >>=
    string (Info.PeerId.to_string self.peer_id) >>=
    BE.int64 ss.Status.downloaded >>=
    BE.int64 ss.Status.left >>=
    BE.int64 ss.Status.uploaded >>=
    BE.int32
      begin match self.status with
        | Running -> 0l
        | Completed -> 1l
        | Started -> 2l
        | Stopped -> 3l
      end >>=
    BE.int32 0l >>=
    BE.int32 0l >>=
    BE.int32 (-1l) >>=
    BE.int16 self.local_port
  in
  let handle_error = function
    | Unix.Unix_error (Unix.ETIMEDOUT, _, _) ->
      debug self.id "UDP Announce Request Timeout after %d s; Retrying..."
        (truncate (15.0 *. 2.0 ** float n)) >>= fun () ->
      if n >= 2 then udp_request_connect self fd ss (n+1)
      else udp_request_announce self fd ss conn_id (n+1)
    | exn ->
      Lwt.fail exn
  in
  udp_send fd (Put.run create_packet) >>= fun () ->
  Lwt_unix.setsockopt_float fd Lwt_unix.SO_RCVTIMEO (15.0 *. 2.0 ** float n);
  Lwt.catch (fun () -> udp_announce_response fd trans_id) handle_error

and udp_announce_response fd trans_id =
  let read_packet =
    let open Get in
    BE.int32 >>= fun n ->
    assert (n = 1l || n = 3l);
    if n = 3l then (* error *)
      string >|= fun msg -> Error msg
    else begin
      BE.int32 >>= fun trans_id' ->
      assert (Int32.compare trans_id trans_id' = 0);
      BE.int32 >>= fun interval ->
      BE.int32 >>= fun leechers ->
      BE.int32 >>= fun seeders ->
      let rec loop () =
        let peer_info =
          BE.uint8 >>= fun a ->
          BE.uint8 >>= fun b ->
          BE.uint8 >>= fun c ->
          BE.uint8 >>= fun d ->
          BE.uint16 >>= fun port ->
          let addr =
            Unix.inet_addr_of_string (sprintf "%03d.%03d.%03d.%03d" a b c d)
          in
          return (addr, port)
        in
        either (end_of_input >|= fun () -> [])
          (peer_info >>= fun pi -> loop () >>= fun rest -> return (pi :: rest))
      in
      loop () >|= fun new_peers ->
      Success
        { new_peers;
          complete = Some (Int32.to_int seeders);
          incomplete = Some (Int32.to_int leechers);
          interval = Int32.to_int interval;
          min_interval = None }
    end
  in
  try
    read_udp_packet fd >|= Get.run_full read_packet
  with
  | Get.Get_error -> failwith_lwt "udp_announce_response: packet too short"
  | exn -> Lwt.fail exn
             
let try_udp_server self ss url =
  let host = match Uri.host url with
    | None -> failwith "Empty Hostname"
    | Some host -> host
  in
  let port = match Uri.port url with
    | None -> failwith "Empty Port"
    | Some port -> port
  in
  Lwt_unix.gethostbyname host >>= fun he ->
  let addr = he.Lwt_unix.h_addr_list.(0) in
  let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0 in
  Lwt_unix.connect fd (Lwt_unix.ADDR_INET (addr, port)) >>= fun () ->
  udp_request_connect self fd ss 0

let decode_http_response (d : Bcode.t) =
  let open Bcode in
  let open Option in
  let success () =
    let complete = find "complete" d >>= to_int in
    let incomplete = find "incomplete" d >>= to_int in
    find "interval" d >>= to_int >>= fun interval ->
    let min_interval = find "min interval" d >>= to_int in
    let compact_peers peers =
      to_string peers >>= fun pr ->
      let rec loop i =
        if i >= String.length pr then []
        else
          (Unix.inet_addr_of_string (sprintf "%03d.%03d.%03d.%03d"
            (int_of_char pr.[i+0]) (int_of_char pr.[i+1])
            (int_of_char pr.[i+2]) (int_of_char pr.[i+3])),
            (int_of_char pr.[i+4] lsl 8 + int_of_char pr.[i+5])) :: loop (i+6)
      in return (loop 0)
    in
    let usual_peers peers =
      to_list peers >>=
      map (fun d ->
        find "ip" d >>= to_string >>= fun ip ->
        find "port" d >>= to_int >>= fun port ->
        let addr = Unix.inet_addr_of_string ip in
        return (addr, port))
    in
    find "peers" d >>= either compact_peers usual_peers >>= fun new_peers ->
    return (Success {new_peers; complete; incomplete; interval; min_interval})
  in
  let error () =
    find "error" d >>= to_string >>= fun s ->
    return (Error s)
  in
  let warning () =
    find "warning" d >>= to_string >>= fun s ->
    return (Warning s)
  in
  either warning (either error success) ()

let try_http_server self ss (uri : Uri.t) : response Lwt.t =
  let uri =
    let params =
      ("info_hash",   (Info.Digest.to_bin self.info_hash)) ::
      ("peer_id",     (Info.PeerId.to_string self.peer_id)) ::
      ("uploaded",    Int64.to_string ss.Status.uploaded) ::
      ("downloaded",  Int64.to_string ss.Status.downloaded) ::
      ("left",        Int64.to_string ss.Status.left) ::
      ("port",        string_of_int self.local_port) ::
      match self.status with
      | Running -> []
      | Completed -> ("event", "completed") :: []
      | Started -> ("event", "started") :: []
      | Stopped -> ("event", "stopped") :: []
    in
    Uri.add_query_params' uri params
  in
  Cohttp_lwt_unix.Client.get uri >>= function
  | None -> raise_lwt (HTTPError "no response")
  | Some (resp, body) ->
    Cohttp_lwt_body.string_of_body body >>= fun body ->
    (* debug id "Got tracker response" >> *)
    let dict = Bcode.from_string body in
    match decode_http_response dict with
    | Some response -> Lwt.return response
    | None -> raise_lwt DecodeError
    (* if not (Cohttp_lwt_unix.Response.has_body resp) then *)
    (*   let err = "Response has no body" in *)
    (*   Log.debug err >> fail (HTTPError err) (* return (Left err) *) *)
    (* else *)
    (*   Log.debug "Got good response!" >> (* decode answer *) assert false *)

let try_server self ss url =
  debug self.id "Querying Tracker: %s" (Uri.to_string url) >>
  match Uri.scheme url with
  | Some "http" ->
    failwith_lwt "skipping over http tracker"
    (* try_http_server t ss url *)
  | Some "udp" ->
    try_udp_server self ss url
  | Some other -> failwith_lwt (sprintf "Unknown Tracker Scheme: %S" other)
  | None -> failwith_lwt "No Tracker Scheme Specified"

let query_trackers self ss : response Lwt.t =
  let rec loop i =
    if i >= Array.length self.tier then
      raise_lwt EmptyAnnounceList
    else
      try_lwt
        try_server self ss self.tier.(i) >>= fun resp ->
        let tmp = self.tier.(0) in
        self.tier.(0) <- self.tier.(i);
        self.tier.(i) <- tmp;
        Lwt.return resp
      with
      | Lwt.Canceled ->
        raise_lwt Lwt.Canceled
      | exn ->
        debug self.id ~exn "Failed" >>= fun () ->
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
let poke_tracker self : (int * int option) Lwt.t =
  let mv = Lwt_mvar.create_empty () in
  Lwt_pipe.write self.status_ch (`RequestStatus mv);
  Lwt_mvar.take mv >>= fun ss ->
  let query () =
    query_trackers self ss >>= function
    | Warning warn ->
      debug self.id "Tracker Warning Response: %s" warn >>= fun () ->
      Lwt.return (fail_timer_interval, None)
    | Error err ->
      debug self.id "Tracker Error Response: %s" err >>= fun () ->
      Lwt.return (fail_timer_interval, None)
    | Success ok ->
      debug self.id "Received %d peers" (List.length ok.new_peers) >>= fun () ->
      Lwt_pipe.write self.peer_mgr_ch (Msg.PeersFromTracker ok.new_peers);
      Lwt_pipe.write self.status_ch (`UpdateStats (ok.complete, ok.incomplete));
      begin match self.status with
        | Running   -> self.status <- Running
        | Stopped   -> self.status <- Stopped
        | Completed -> self.status <- Running
        | Started   -> self.status <- Running
      end;
      Lwt.return (ok.interval, ok.min_interval)
  in
  let handle_error = function
    | HTTPError err ->
      debug self.id "Tracker HTTP Error: %s" err >>= fun () ->
      Lwt.return (fail_timer_interval, None)
    | DecodeError ->
      debug self.id "Response Decode Error" >>= fun () ->
      Lwt.return (fail_timer_interval, None)
    | exn ->
      Lwt.fail exn
  in
  Lwt.catch query handle_error

let handle_message self msg : unit Lwt.t =
  debug self.id "%s" (string_of_msg msg) >>
  match msg with
  | TrackerTick x ->
    if x+1 = self.next_tick then
      poke_tracker self >>= timer_update self
    else
      Lwt.return_unit
  | Stop ->
    self.status <- Stopped;
    poke_tracker self >>= timer_update self
  | Start ->
    self.status <- Started;
    poke_tracker self >>= timer_update self
  | Complete ->
    self.status <- Completed;
    poke_tracker self >>= timer_update self

let shuffle_array a =
  for i = (Array.length a)-1 downto 1 do
    let j = Random.int (i+1) in
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
  done

let start ~super_ch ~ch ~info_hash ~peer_id ~local_port
  ~tier ~status_ch ~peer_mgr_ch =
  let run id =
    let self =
      { ch;
        status_ch;
        peer_mgr_ch;
        info_hash;
        peer_id;
        tier = Array.of_list tier;
        local_port;
        status        = Stopped;
        next_tick     = 0;
        id }
    in
    shuffle_array self.tier;
    Lwt_pipe.iter_s (handle_message self) ch
  in
  Proc.spawn ~name:"Tracker" run (Super.default_stop super_ch)
