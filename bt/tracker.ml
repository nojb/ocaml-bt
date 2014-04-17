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

type response = {
  peers : Addr.t list;
  leechers : int option;
  seeders : int option;
  interval : int
}

exception Error of string
exception Warning of string

module UdpTracker = struct
  let fresh_transaction_id () =
    Random.int32 Int32.max_int

  let connect_response trans_id s =
    bitmatch Bitstring.bitstring_of_string s with
    | { 3l : 32; msg : -1 : string } ->
      `Error msg
    | { n : 32; trans_id' : 32 : check (trans_id = trans_id'); conn_id : 64 } ->
      `Ok conn_id

  let connect_request trans_id =
    BITSTRING { 0x41727101980L : 64; 0l : 32; trans_id : 32 }

  let announce_request conn_id trans_id ih ?(up = 0L) ?(down = 0L) ?(left = 0L) event port id =
    let event = match event with
      | None -> 0l
      | Some COMPLETED -> 1l
      | Some STARTED -> 2l
      | Some STOPPED -> 3l
    in
    BITSTRING { conn_id : 64; 1l : 32; trans_id : 32;
                SHA1.to_bin ih : 20 * 8 : string;
                SHA1.to_bin id : 20 * 8 : string;
                down : 64; left : 64; up : 64;
                event : 32; 0l : 32; 0l : 32; -1l : 32;
                port : 16 }                

  let announce_response trans_id s =
    bitmatch Bitstring.bitstring_of_string s with
    | { 3l : 32; msg : -1 : string } ->
      `Error msg
    | { 1l : 32; trans_id' : 32 : check (trans_id = trans_id');
        interval : 32 : bind (Int32.to_int interval);
        leechers : 32 : bind (Int32.to_int leechers);
        seeders : 32 : bind (Int32.to_int seeders);
        peers : -1 : bitstring } ->
      let rec loop bs =
        bitmatch bs with
        | { addr : 6 * 8 : bitstring; bs : -1 : bitstring } ->
          Addr.of_string_compact addr :: loop bs
        | { _ } ->
          []
      in
      let peers = loop peers in
      `Ok {peers; leechers = Some leechers; seeders = Some seeders; interval}

  let do_announce fd addr ih ?up ?down ?left ?event port id : response Lwt.t =
    let rec loop = function
      | `Connect_request n ->
        let trans_id = fresh_transaction_id () in
        Udp.send_bitstring fd (connect_request trans_id) addr >>= fun () ->
        Udp.set_timeout fd (15.0 *. 2.0 ** float n);
        Lwt.catch
          (fun () -> loop (`Connect_response trans_id))
          (function
            | Unix.Unix_error (Unix.ETIMEDOUT, _, _) ->
              if n >= 8 then
                failwith_lwt "connect_request: too many retries"
              else begin
                Log.info "UDP connect request timeout after %d s; retrying..."
                  (truncate (15.0 *. 2.0 ** float n));
                loop (`Connect_request (n+1))
              end
            | exn -> Lwt.fail exn)
      | `Connect_response trans_id ->
        Udp.recv fd >>= fun (s, _) ->
        begin match connect_response trans_id s with
        | `Error msg -> Lwt.fail (Error msg)
        | `Ok conn_id -> loop (`Announce_request (conn_id, 0))
        end
      | `Announce_request (conn_id, n) ->
        let trans_id = fresh_transaction_id () in
        let create_packet = announce_request conn_id trans_id ih ?up ?down ?left event port id in
        Udp.send_bitstring fd create_packet addr >>= fun () ->
        Udp.set_timeout fd (15.0 *. 2.0 ** float n);
        Lwt.catch
          (fun () -> loop (`Announce_response trans_id))
          (function
            | Unix.Unix_error (Unix.ETIMEDOUT, _, _) ->
              Log.info "ANNOUNCE UDP announce request timeout after %d s; retrying..."
                (truncate (15.0 *. 2.0 ** float n));
              if n >= 2 then
                loop (`Connect_request (n+1))
              else
                loop (`Announce_request (conn_id, n+1))
            | exn ->
              Lwt.fail exn)
      | `Announce_response trans_id ->
        let doit () =
          Udp.recv fd >>= fun (s, _) ->
          match announce_response trans_id s with
          | `Error msg -> Lwt.fail (Error msg)
          | `Ok resp -> Lwt.return resp
        in
        Lwt.catch doit Lwt.fail
    in
    loop (`Connect_request 0)

  let announce tr ih ?up ?down ?left ?event port id =
    let url = tr in
    let host = match Uri.host url with
      | None -> failwith "Empty Hostname"
      | Some host -> host
    in
    let port = match Uri.port url with
      | None -> failwith "Empty Port"
      | Some port -> port
    in
    let addr = (Addr.Ip.of_string host, port) in
    let fd = Udp.create_socket () in
    do_announce fd addr ih ?up ?down ?left ?event port id
end

module HttpTracker = struct
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
        let peers = Bcode.to_string peers in
        let rec loop bs =
          bitmatch bs with
          | { addr : 6 * 8 : bitstring; bs : -1 : bitstring } ->
            Addr.of_string_compact addr :: loop bs
          | { _ } ->
            []
        in
        loop (Bitstring.bitstring_of_string peers)
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
    let uri = tr in
    let uri = Uri.add_query_param' uri ("info_hash", SHA1.to_bin ih) in
    let uri = Uri.add_query_param' uri ("peer_id", SHA1.to_bin id) in
    let uri = add_param_maybe uri "uploaded" Int64.to_string up in
    let uri = add_param_maybe uri "downloaded" Int64.to_string down in
    let uri = add_param_maybe uri "left" Int64.to_string left in
    let uri = Uri.add_query_param' uri ("port", string_of_int port) in
    let uri = Uri.add_query_param' uri ("compact", "1") in
    let uri = add_param_maybe uri "event" string_of_event event in
    Cohttp_lwt_unix.Client.get uri >>= fun (_, body) ->
    Cohttp_lwt_body.to_string body >>= fun body ->
    Log.info "Received response from HTTP tracker body: %S" body;
    try
      Bcode.decode body |> decode_response
    with exn ->
      Lwt.fail (Failure ("http error: decode error: " ^ Printexc.to_string exn))
end

let query tr ~ih ?up ?down ?left ?event ~port ~id =
  Log.info "announcing on %S" (Uri.to_string tr);
  match Uri.scheme tr with
  | Some "http" | Some "https" ->
    HttpTracker.announce tr ih ?up ?down ?left ?event port id
  | Some "udp" ->
    UdpTracker.announce tr ih ?up ?down ?left ?event port id
  | Some sch ->
    failwith_lwt "unknown tracker url scheme: %S" sch
  | None ->
    failwith_lwt "missing tracker url scheme"

module Tier = struct
  type t = Uri.t list ref

  exception No_valid_tracker

  let create () =
    ref []

  let shuffle tier =
    let a = Array.of_list !tier in
    Util.shuffle_array a;
    tier := Array.to_list a

  let add_tracker seq tr =
    seq := tr :: !seq

  let query tier ~ih ?up ?down ?left ?event ~port ~id =
    let rec loop failtrs = function
      | [] -> Lwt.fail No_valid_tracker
      | tr :: rest ->
        try
          query tr ~ih ?up ?down ?left ?event ~port ~id >>= fun resp ->
          tier := tr :: List.rev_append failtrs rest;
          Lwt.return resp
        with
        | e ->
          Log.error ~exn:e "error while announcing on %S" (Uri.to_string tr);
          loop (tr :: failtrs) rest
    in
    loop [] !tier

  let show tier =
    match !tier with
    | [] -> "(no trackers)"
    | tr :: _ -> Uri.to_string tr
end
