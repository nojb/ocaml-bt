(* The MIT License (MIT)

   Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
   FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
   COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let log fmt =
  Printf.ksprintf (fun msg -> Lwt_log.ign_debug_f "[tracker] %s\n%!" msg) fmt
              
let failwith_lwt fmt =
  Printf.ksprintf (fun msg -> Lwt.fail (Failure msg)) fmt

type event =
  | STARTED
  | STOPPED
  | COMPLETED

let string_of_event = function
  | STARTED -> "started"
  | STOPPED -> "stopped"
  | COMPLETED -> "completed"

type t = {
  uri : Uri.t
}

type tracker = t

type response = {
  peers : Addr.t list;
  leechers : int option;
  seeders : int option;
  interval : int
}

exception Error of string
exception Warning of string
           
module Udp = struct
  let send fd buf =
    Lwt_unix.write fd buf 0 (String.length buf) >>= fun len ->
    if len <> String.length buf then
      failwith_lwt "udp_send: could not send entire packet"
    else
      Lwt.return_unit

  let packet_length = 512

  let recv fd =
    let buf = String.create packet_length in
    Lwt_unix.read fd buf 0 packet_length >>= fun len ->
    Lwt.return (String.sub buf 0 len)

  let fresh_transaction_id () =
    Random.int32 Int32.max_int

  let get_ip_addr =
    let open Get in
    let open Get.BE in
    uint8 >>= fun a ->
    uint8 >>= fun b ->
    uint8 >>= fun c ->
    uint8 >|= fun d ->
    Addr.Ip.of_ints a b c d
    (* Unix.inet_addr_of_string (Printf.sprintf "%03d.%03d.%03d.%03d" a b c d) *)

  let map_maybe f x if_none =
    match x with
    | None -> f if_none
    | Some x -> f x

  let connect_response trans_id =
    let open Get in
    let open Get.BE in
    int32 >>= fun n ->
    if n = 3l then
      any_string >|= fun msg -> `Error msg
    else begin
      int32 >>= fun trans_id' ->
      assert (Int32.compare trans_id trans_id' = 0);
      int64 >|= fun conn_id -> `Ok conn_id
    end

  let connect_request trans_id =
    let open Put in
    let open Put.BE in
    int64 0x41727101980L >>
    int32 0l >>
    int32 trans_id
      
  let announce_request conn_id trans_id ih up down left event port id =
    let open Put in
    let open Put.BE in
    int64 conn_id >>
    int32 1l >>
    int32 trans_id >>
    string (Word160.to_bin ih) >>
    string (Word160.to_bin id) >>
    map_maybe int64 down 0L >>
    map_maybe int64 left 0L >>
    map_maybe int64 up 0L >>
    int32
      (match event with
       | None -> 0l
       | Some COMPLETED -> 1l
       | Some STARTED -> 2l
       | Some STOPPED -> 3l) >>
    int32 0l >>
    int32 0l >>
    int32 (-1l) >>
    int16 port

  let announce_response trans_id =
    let open Get in
    let open Get.BE in
    int32 >>= fun n ->
    assert (n = 1l || n = 3l);
    if n = 3l then (* error *)
      any_string >|= fun msg -> `Error msg
    else begin
      let peer_info =
        get_ip_addr >>= fun addr ->
        uint16 >>= fun port ->
        return (addr, port)
      in
      int32 >>= fun trans_id' ->
      assert (Int32.compare trans_id trans_id' = 0);
      int32 >|= Int32.to_int >>= fun interval ->
      int32 >|= Int32.to_int >>= fun leechers ->
      int32 >|= Int32.to_int >>= fun seeders ->
      many peer_info >|= fun peers ->
      (* let rec loop () = *)
      (*   in *)
      (*   either (end_of_input >|= fun () -> []) *)
      (*     (peer_info >>= fun pi -> loop () >>= fun rest -> return (pi :: rest)) *)
      (* in *)
      (* loop () >|= fun new_peers -> *)
      `Ok { peers; leechers = Some leechers; seeders = Some seeders; interval; }
    end

  let set_timeout fd x =
    Lwt_unix.setsockopt_float fd Lwt_unix.SO_RCVTIMEO x

  let do_announce fd ih ?up ?down ?left ?event port id =
    let rec loop = function
      | `Connect_request n ->
        let trans_id = fresh_transaction_id () in
        Put.run (connect_request trans_id) |> send fd >>= fun () ->
        set_timeout fd (15.0 *. 2.0 ** float n);
        Lwt.catch
          (fun () -> loop (`Connect_response trans_id))
          (function
            | Unix.Unix_error (Unix.ETIMEDOUT, _, _) ->
              if n >= 8 then
                failwith_lwt "connect_request: too many retries"
              else begin
                Lwt_log.info_f "UDP connect request timeout after %d s; retrying..."
                  (truncate (15.0 *. 2.0 ** float n)) >>= fun () ->
                loop (`Connect_request (n+1))
              end
            | exn -> Lwt.fail exn)
      | `Connect_response trans_id ->
        recv fd >|= Get.run (connect_response trans_id) >>= begin function
          | `Error msg -> Lwt.fail (Error msg)
          | `Ok conn_id -> loop (`Announce_request (conn_id, 0))
        end
      | `Announce_request (conn_id, n) ->
        let trans_id = fresh_transaction_id () in
        let create_packet = announce_request conn_id trans_id ih up down left event port id in
        send fd (Put.run create_packet) >>= fun () ->
        set_timeout fd (15.0 *. 2.0 ** float n);
        Lwt.catch
          (fun () -> loop (`Announce_response trans_id))
          (function
            | Unix.Unix_error (Unix.ETIMEDOUT, _, _) ->
              log "ANNOUNCE UDP announce request timeout after %d s; retrying..."
                (truncate (15.0 *. 2.0 ** float n));
              if n >= 2 then
                loop (`Connect_request (n+1))
              else
                loop (`Announce_request (conn_id, n+1))
            | exn ->
              Lwt.fail exn)
      | `Announce_response trans_id ->
        try
          recv fd >|= Get.run (announce_response trans_id) >>= function
          | `Error msg -> Lwt.fail (Error msg)
          | `Ok resp -> Lwt.return resp
        with
        | Get.Get_error -> failwith_lwt "Udp.announce_response: packet too short"
        | exn -> Lwt.fail exn
    in
    loop (`Connect_request 0)

  let announce tr ih ?up ?down ?left ?event port id =
    let url = tr.uri in
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
    do_announce fd ih ?up ?down ?left ?event port id
end

module Http = struct
  let either f g x =
    try f x with _ -> g x

  let decode_response (d : Bcode.t) =
    let success () =
      let seeders =
        try Some (Bcode.find "complete" d |> Bcode.to_int) with Not_found -> None
      in
      let leechers =
        try Some (Bcode.find "incomplete" d |> Bcode.to_int) with Not_found -> None
      in
      let interval = Bcode.find "interval" d |> Bcode.to_int in
      (* let min_interval = try Some (Bcode.find "min interval" d) with _ -> None in *)
      let compact_peers peers =
        let pr = Bcode.to_string peers in
        let rec loop i =
          if i >= String.length pr then []
          else
            let addr =
              Addr.Ip.of_ints
                (int_of_char pr.[i+0]) (int_of_char pr.[i+1])
                (int_of_char pr.[i+2]) (int_of_char pr.[i+3])
            in
            let port = int_of_char pr.[i+4] lsl 8 + int_of_char pr.[i+5] in
            (addr, port) :: loop (i+6)
        in
        loop 0
      in
      let usual_peers peers =
        Bcode.to_list peers |>
        List.map (fun d ->
            let ip = Bcode.find "ip" d |> Bcode.to_string in
            let port = Bcode.find "port" d |> Bcode.to_int in
            let addr = Addr.Ip.of_string ip in
            (addr, port))
      in
      let peers = Bcode.find "peers" d in
      let peers = either compact_peers usual_peers peers in
      Lwt.return {peers; leechers; seeders; interval}
    in
    let error () =
      let s = Bcode.find "failure reason" d |> Bcode.to_string in
      Lwt.fail (Error s)
    in
    let warning () =
      let s = Bcode.find "warning message" d |> Bcode.to_string in
      Lwt.fail (Warning s)
    in
    either warning (either error success) ()

  let announce tr ih ?up ?down ?left ?event port id =
    let add_param_maybe uri name f = function
      | None -> uri
      | Some x -> Uri.add_query_param' uri (name, f x)
    in
    let uri = tr.uri in
    let uri = Uri.add_query_param' uri ("info_hash", Word160.to_bin ih) in
    let uri = Uri.add_query_param' uri ("peer_id", Word160.to_bin id) in
    let uri = add_param_maybe uri "uploaded" Int64.to_string up in
    let uri = add_param_maybe uri "downloaded" Int64.to_string down in
    let uri = add_param_maybe uri "left" Int64.to_string left in
    let uri = Uri.add_query_param' uri ("port", string_of_int port) in
    let uri = Uri.add_query_param' uri ("compact", "1") in
    let uri = add_param_maybe uri "event" string_of_event event in
    Cohttp_lwt_unix.Client.get uri >>= fun (_, body) ->
    Cohttp_lwt_body.to_string body >>= fun body ->
    Lwt_log.info_f "Received response from HTTP tracker body: %S" body >>= fun () ->
    try
      Get.run Bcode.bdecode body |> decode_response
    with exn ->
      Lwt.fail (Failure ("http error: decode error: " ^ Printexc.to_string exn))
end

let query tr ih ?up ?down ?left ?event port id =
  log "announcing on %S" (Uri.to_string tr.uri);
  match Uri.scheme tr.uri with
  | Some "http" | Some "https" ->
    Http.announce tr ih ?up ?down ?left ?event port id
  | Some "udp" ->
    Udp.announce tr ih ?up ?down ?left ?event port id
  | Some sch ->
    failwith_lwt "unknown tracker url scheme: %S" sch
  | None ->
    failwith_lwt "missing tracker url scheme"

let create uri =
  { uri }

module Tier = struct
  type t = tracker list ref

  exception No_valid_tracker

  let create () =
    ref []
      
  let shuffle tier =
    let a = Array.of_list !tier in
    Util.shuffle_array a;
    tier := Array.to_list a

  let add_tracker seq tr =
    seq := tr :: !seq

  let query tier ih ?up ?down ?left ?event port id =
    let rec loop failtrs = function
      | [] -> Lwt.fail No_valid_tracker
      | tr :: rest ->
        try
          query tr ih ?up ?down ?left ?event port id >>= fun resp ->
          tier := tr :: List.rev_append failtrs rest;
          Lwt.return resp
        with
        | _ ->
          loop (tr :: failtrs) rest
    in
    loop [] !tier
end
