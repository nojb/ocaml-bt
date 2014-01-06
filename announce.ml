let default_tracker_interval = 5

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)
              
let failwith_lwt fmt =
  Printf.ksprintf (fun msg -> Lwt.fail (Failure msg)) fmt

type tier = {
  trackers : Uri.t array;
  mutable interval : int
}

type event =
  | STARTED
  | STOPPED
  | NONE
  | COMPLETED

type t = {
  info_hash : Word160.t;
  up : unit -> int64;
  down : unit -> int64;
  amount_left : unit -> int64;
  port : int;
  id : Word160.t;
  control : [`Announce of event | `Stop] Lwt_condition.t;
  got_peer : (Unix.sockaddr -> unit) Lwt_sequence.t
  (* new_tier : Uri.t list -> unit; *)
  (* connect : Unix.sockaddr -> unit; *)
  (* mutable tiers : tier list; *)
  (* udp_sock : Lwt_unix.file_descr *)
}

type response =
  | Error of string
  | Warning of string
  | Success of Unix.sockaddr list * int

let string_of_event = function
  | STARTED -> "STARTED"
  | STOPPED -> "STOPPED"
  | NONE -> "NONE"
  | COMPLETED -> "COMPLETED"

let shuffle_array a =
  let l = Array.length a in
  for i = l-1 downto 1 do
    let j = Random.int (i+1) in
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t
  done

let stop self =
  Lwt_condition.broadcast self.control (`Announce STOPPED);
  Lwt_condition.broadcast self.control `Stop
    
let udp_send fd buf =
  Lwt_unix.write fd buf 0 (String.length buf) >>= fun len ->
  if len <> String.length buf then
    failwith_lwt "udp_send: could not send entire packet"
  else
    Lwt.return_unit

let udp_packet_length = 512

let udp_recv fd =
  let buf = String.create udp_packet_length in
  Lwt_unix.read fd buf 0 udp_packet_length >>= fun len ->
  Lwt.return (String.sub buf 0 len)

let udp_fresh_transaction_id () =
  Random.int32 Int32.max_int

let rec udp_request_connect ann fd ev n =
  let trans_id = udp_fresh_transaction_id () in
  let put_packet =
    let open Put in
    let open Put.BE in
    int64 0x41727101980L >>
    int32 0l >>
    int32 trans_id
  in
  udp_send fd (Put.run put_packet) >>= fun () ->
  let handle_error = function
    | Unix.Unix_error (Unix.ETIMEDOUT, _, _) ->
      if n >= 8 then
        failwith_lwt "udp_request_connect: too many retries"
      else begin
        Trace.infof "UDP connect request timeout after %d s; retrying..."
          (truncate (15.0 *. 2.0 ** float n));
        udp_request_connect ann fd ev (n+1)
      end
    | exn -> Lwt.fail exn
  in
  Lwt_unix.setsockopt_float fd Lwt_unix.SO_RCVTIMEO (15.0 *. 2.0 ** float n);
  Lwt.catch (fun () -> udp_connect_response ann fd ev trans_id) handle_error

and udp_connect_response ann fd ev trans_id =
  let get_packet : response Lwt.t Get.t =
    let open Get in
    let open Get.BE in
    int32 >>= fun n ->
    if n = 3l then
      any_string >|= fun msg -> Lwt.return (Error msg)
    else begin
      int32 >>= fun trans_id' ->
      assert (Int32.compare trans_id trans_id' = 0);
      int64 >|= fun conn_id ->
      udp_request_announce ann fd ev conn_id 0
    end
  in
  udp_recv fd >>= Get.run get_packet

and udp_request_announce ann fd ev conn_id n =
  let trans_id = udp_fresh_transaction_id () in
  let create_packet =
    let open Put in
    let open Put.BE in
    int64 conn_id >>
    int32 1l >>
    int32 trans_id >>
    string (Word160.to_bin ann.info_hash) >>
    string (Word160.to_bin ann.id) >>
    int64 (ann.down ()) >>
    int64 (ann.amount_left ()) >>
    int64 (ann.up ()) >>
    int32
      begin match ev with
        | NONE -> 0l
        | COMPLETED -> 1l
        | STARTED -> 2l
        | STOPPED -> 3l
      end >>
    int32 0l >>
    int32 0l >>
    int32 (-1l) >>
    int16 ann.port
  in
  let handle_error = function
    | Unix.Unix_error (Unix.ETIMEDOUT, _, _) ->
      Trace.infof "ANNOUNCE UDP announce request timeout after %d s; retrying..."
        (truncate (15.0 *. 2.0 ** float n));
      if n >= 2 then udp_request_connect ann fd ev (n+1)
      else udp_request_announce ann fd ev conn_id (n+1)
    | exn ->
      Lwt.fail exn
  in
  udp_send fd (Put.run create_packet) >>= fun () ->
  Lwt_unix.setsockopt_float fd Lwt_unix.SO_RCVTIMEO (15.0 *. 2.0 ** float n);
  Lwt.catch (fun () -> udp_announce_response fd ev trans_id) handle_error

and udp_announce_response fd ev trans_id =
  let get_packet =
    let open Get in
    let open Get.BE in
    int32 >>= fun n ->
    assert (n = 1l || n = 3l);
    if n = 3l then (* error *)
      any_string >|= fun msg -> Error msg
    else begin
      let peer_info =
        uint8 >>= fun a ->
        uint8 >>= fun b ->
        uint8 >>= fun c ->
        uint8 >>= fun d ->
        uint16 >>= fun port ->
        let addr =
          Unix.inet_addr_of_string (Printf.sprintf "%03d.%03d.%03d.%03d" a b c d)
        in
        return (Unix.ADDR_INET (addr, port))
      in
      int32 >>= fun trans_id' ->
      assert (Int32.compare trans_id trans_id' = 0);
      int32 >>= fun interval ->
      int32 >>= fun leechers ->
      int32 >>= fun seeders ->
      many peer_info >|= fun new_peers ->
      (* let rec loop () = *)
      (*   in *)
      (*   either (end_of_input >|= fun () -> []) *)
      (*     (peer_info >>= fun pi -> loop () >>= fun rest -> return (pi :: rest)) *)
      (* in *)
      (* loop () >|= fun new_peers -> *)
      Success (new_peers, Int32.to_int interval)
    end
  in
  try
    udp_recv fd >|= Get.run get_packet
  with
  | Get.Get_error -> failwith_lwt "udp_announce_response: packet too short"
  | exn -> Lwt.fail exn
             
let udp_announce ann url ev =
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
  udp_request_connect ann fd ev 0

let either f g x =
  try f x with _ -> g x

let http_decode_response (d : Bcode.t) =
  let success () =
    (* let seeders = *)
    (*   try Some (Bcode.find "complete" d |> Bcode.to_int) with Not_found -> None *)
    (* in *)
    (* let leechers = *)
    (*   try Some (Bcode.find "incomplete" d |> Bcode.to_int) with Not_found -> None *)
    (* in *)
    let interval = Bcode.find "interval" d |> Bcode.to_int in
    (* let min_interval = try Some (Bcode.find "min interval" d) with _ -> None in *)
    let compact_peers peers =
      let pr = Bcode.to_string peers in
      let rec loop i =
        if i >= String.length pr then []
        else
          let addr =
            Unix.inet_addr_of_string
              (Printf.sprintf "%03d.%03d.%03d.%03d"
                 (int_of_char pr.[i+0]) (int_of_char pr.[i+1])
                 (int_of_char pr.[i+2]) (int_of_char pr.[i+3]))
          in
          let port = int_of_char pr.[i+4] lsl 8 + int_of_char pr.[i+5] in
          Unix.ADDR_INET (addr, port) :: loop (i+6)
      in
      loop 0
    in
    let usual_peers peers =
      Bcode.to_list peers |>
      List.map (fun d ->
          let ip = Bcode.find "ip" d |> Bcode.to_string in
          let port = Bcode.find "port" d |> Bcode.to_int in
          let addr = Unix.inet_addr_of_string ip in
          Unix.ADDR_INET (addr, port))
    in
    let peers = Bcode.find "peers" d in
    let peers = either compact_peers usual_peers peers in
    Success (peers, interval)
  in
  let error () =
    let s = Bcode.find "failure reason" d |> Bcode.to_string in
    Error s
  in
  let warning () =
    let s = Bcode.find "warning message" d |> Bcode.to_string in
    Warning s
  in
  either warning (either error success) ()

let http_announce self tr ev =
  let uri =
    let params =
      ("info_hash",   Word160.to_bin self.info_hash) ::
      ("peer_id",     Word160.to_bin self.id) ::
      ("uploaded",    Int64.to_string (self.up ())) ::
      ("downloaded",  Int64.to_string (self.down ())) ::
      ("left",        Int64.to_string (self.amount_left ())) ::
      ("port",        string_of_int self.port) ::
      ("compact",     "1") ::
      match ev with
      | NONE -> []
      | _ -> ("event", String.lowercase (string_of_event ev)) :: []
    in
    Uri.add_query_params' tr params
  in
  Cohttp_lwt_unix.Client.get uri >>= function
  | None -> Lwt.fail (Failure "http error: no reponse")
  | Some (_, body) ->
    Cohttp_lwt_body.string_of_body body >>= fun body ->
    Trace.infof "Received response from HTTP tracker body: %S" body;
    try
      Get.run Bcode.bdecode body |> http_decode_response |> Lwt.return
    with exn ->
      Lwt.fail (Failure ("http error: decode error: " ^ Printexc.to_string exn))

let announce self tr ev =
  Trace.infof "Announcing on %S" (Uri.to_string tr);
  match Uri.scheme tr with
  | Some "http" | Some "https" ->
    http_announce self tr ev
  | Some "udp" ->
    udp_announce self tr ev
  | Some sch ->
    failwith_lwt "unknown tracker url scheme: %S" sch
  | None ->
    failwith_lwt "missing tracker url scheme"

let swap a i j =
  let l = Array.length a in
  if i < 0 || i >= l || j < 0 || j >= l then invalid_arg "Announce.swap";
  let t = a.(i) in
  a.(i) <- a.(j);
  a.(j) <- t

let announce_tier self tier ev =
  let trs = tier.trackers in
  let rec loop i =
    if i >= Array.length trs then begin
      Trace.infof "announce_tier: no working tracker";
      Lwt.return [] (* FIXME FIXME*)
    end else
      let tr = trs.(i) in
      Lwt.catch
        (fun () ->
           announce self tr ev >>= function
           | Success (peers, interval) ->
             Trace.infof "Tracker %S returned %d peers"
               (Uri.to_string tr) (List.length peers);
             (* List.iter (function *)
             (*     | Unix.ADDR_INET (i, p) -> *)
             (*       Trace.infof "%s:%d" (Unix.string_of_inet_addr i) p) peers; *)
             swap trs 0 i;
             tier.interval <- interval;
             Lwt.return peers
           | _ ->
             failwith_lwt "tracker returned error/warning")
        (fun exn ->
           Trace.infof ~exn "error while announcing on %S" (Uri.to_string tr);
           loop (i+1))
  in
  loop 0

let new_tier got_peer announce_tier control uris =
  let tier = { trackers = Array.of_list uris; interval = default_tracker_interval } in
  shuffle_array tier.trackers;
  let rec loop t ev =
    Lwt.pick
      [ Lwt_unix.sleep t >|= (fun () -> `Announce ev);
        Lwt_condition.wait control ] >>= function
    | `Announce ev ->
      announce_tier tier ev >|=
      List.iter got_peer >>= fun () ->
      loop (float tier.interval) NONE
    | `Stop ->
      Lwt.return_unit
  in
  (* FIXME error handling *)
  Lwt.async (fun () -> loop 0. STARTED)

let start self =
  Lwt_condition.broadcast self.control (`Announce STARTED)

let got_peer self sa =
  Lwt_sequence.iter_l (fun f -> f sa) self.got_peer

let create info_hash tiers up down amount_left port id on_got_peer =
  let control = Lwt_condition.create () in
  let rec self = {
    (* lazy { *)
      info_hash;
      up;
      down;
      amount_left;
      port;
      id;
      control;
      got_peer = Lwt_sequence.create ()
      (* new_tier = new_tier (fun tier ev -> announce_tier (Lazy.force self) tier ev) *)
          (* control connect; *)
      (* udp_sock = Lwt_unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 *)
    }
  in
  Lwt_sequence.add_r on_got_peer self.got_peer;
  (* let self = Lazy.force self in *)
  List.iter (new_tier (got_peer self) (announce_tier self) control) tiers;
  self

let on_got_peer self f =
  Lwt_sequence.add_r f self.got_peer |> ignore

