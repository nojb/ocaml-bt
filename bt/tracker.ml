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

let section = Log.make_section "Tracker"

let debug ?exn fmt = Log.debug section ?exn fmt

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
  type state =
    | Idle
    | Wait_connect of int32 * int
    | Wait_announce of int32 * int64 * int

  type tracker =
    { mutable state : state }

  let fresh_transaction_id () =
    Random.int32 Int32.max_int

  let conn_req trans_id =
    let buf = Byte.create (8 + 4 + 4) in
    BE.set_int64 buf 0 0x41727101980L;
    BE.set_int32 buf 8 0l;
    BE.set_int32 buf 12 trans_id;
    buf

  let ann_req conn_id trans_id ih ?(up = 0L) ?(down = 0L) ?(left = 0L) event ?(port = 0) id =
    let event = match event with
      | None -> 0l
      | Some COMPLETED -> 1l
      | Some STARTED -> 2l
      | Some STOPPED -> 3l
    in
    let buf = Bytes.create (8 + 4 + 4 + 20 + 20 + 8 + 8 + 8 + 4 + 4 + 4 + 4 + 2) in
    BE.set_int64 buf 00 conn_id;
    BE.set_int32 buf 08 1l;
    BE.set_int32 buf 12 trans_id;
    Bytes.blit ih 0 buf 16 20;
    Bytes.blit id 0 buf 36 20;
    BE.set_int64 buf 56 down;
    BE.set_int64 buf 64 left;
    BE.set_int64 buf 72 up;
    BE.set_int32 buf 80 event;
    BE.set_int32 buf 84 0l;
    BE.set_int32 buf 88 0l;
    BE.set_int32 buf 92 -1l;
    BE.set_int16 buf 96 port;
    buf
    (* BITSTRING *)
    (*   { conn_id : 64; 1l : 32; trans_id : 32; *)
    (*     SHA1.to_bin ih : 20 * 8 : string; *)
    (*     SHA1.to_bin id : 20 * 8 : string; *)
    (*     down : 64; left : 64; up : 64; *)
    (*     event : 32; 0l : 32; 0l : 32; -1l : 32; *)
    (*     (match port with None -> 0 | Some p -> p) : 16 } *)

  (* let send_announce_request fd addr conn_id ih ?up ?down ?left event ?port id = *)
  (*   let trans_id = fresh_transaction_id () in *)
  (*   let create_packet = make_announce_request conn_id trans_id ih ?up ?down ?left event ?port id in *)
  (*   UDP.send_bitstring fd create_packet addr >>= fun () -> *)
  (*   Lwt.return trans_id *)

  (* let read_announce_response fd trans_id = *)
  (*   Lwt.catch begin fun () -> *)
  (*     UDP.recv fd >>= fun (s, _) -> *)
  (*     match parse_announce_response trans_id s with *)
  (*     | `Error msg -> Lwt.fail (Error msg) *)
  (*     | `Ok resp -> Lwt.return resp *)
  (*   end Lwt.fail *)

  let handle ~now tr data =
    match tr.state with
    | Idle -> None
    | Wait_connect (trans_id, _, _) ->
        begin match BE.get_int32 data 0 with
        | 3l -> Some (`Error (String.from data 4))
        | _ ->
            let trans_id' = BE.get_int32 data 4 in
            let conn_id = BE.get_int64 data 8 in
            let trans_id = fresh_transaction_id () in
            let data = ann_req conn_id trans_id ih ?up ?down ?left event ?port id in
            tr.state <- Wait_announce (trans_id, conn_id, 0, now);
            Some (`Respond (data, 15.0))
        end
    | Wait_announce (trans_id, conn_id, _, _) ->
        begin match BE.get_int32 s 0 with
        | 3l -> Some (`Error (String.from s 4))
        | 1l ->
            let trans_id' = BE.get_int32 s 4 in
            let interval = BE.get_int32 s 8 in
            let leechers = BE.get_int32 s 12 in
            let seeders = BE.get_int32 s 16 in
            let rec scan s off =
              if off + 6 <= String.length s then
                let x1 = BE.get_uint8 s (off+0) in
                let x2 = BE.get_uint8 s (off+1) in
                let x3 = BE.get_uint8 s (off+2) in
                let x4 = BE.get_uint8 s (off+3) in
                let pt = BE.get_uint16 s (off+4) in
                let addr =
                  Unix.inet_addr_of_string (Printf.sprintf "%d.%d.%d.%d" x1 x2 x3 x4)
                in
                (addr, pt) :: scan s (off+6)
              else
                []
            in
            let peers = loop peers in
            Some (`Peers (peers, interval, leechers, seeders))
        end

  let announce ~now tr =
    match tr.state with
    | Idle ->
        let trans_id = fresh_transaction_id () in
        let data = conn_req trans_id in
        tr.state <- Wait_connect (trans_id, 0, now);
        Some (`Respond (data, 15.0))
    | Wait_connect _ | Wait_announce _ ->
        None

  let periodic ~now tr =
    let delay n = 15.0 * 2.0 ** float n in
    match tr.state with
    | Idle -> None
    | Wait_connect (trans_id, n, last) when last +. delay n >= now ->
        if n >= 8 then Some (`Error "too many retries")
        else begin
          let trans_id = fresh_transaction_id () in
          let data = conn_req trans_id in
          let n = n+1 in
          tr.state <- Wait_connect (trans_id, n, now);
          Some (`Respond (data, delay n))
        end
    | Wait_connect _ -> None
    | Wait_announce (trans_id, conn_id, n, last) when last + delay n >= now ->
        let trans_id = fresh_transaction_id () in
        if n >= 2 then begin
          let data = conn_req trans_id in
          tr.state <- Wait_connect (trans_id, 0, now);
          Some (`Respond (data, 15.0))
        end else begin
          let data = ann_req conn_id trans_id ih ?up ?down ?left event ?port id in
          let n = n+1 in
          tr.state <- Wait_announce (trans_id, conn_id, n, now);
          Some (`Respond (data, delay n))
        end
    | Wait_announce _ -> None

  (* let do_announce fd addr ih ?up ?down ?left ?event ?port id : response Lwt.t = *)
  (*   let rec try_connect n = *)
  (*     send_connect_request fd addr >>= fun trans_id -> *)
  (*     UDP.set_timeout fd (15.0 *. 2.0 ** float n); *)
  (*     Lwt.catch begin fun () -> *)
  (*       read_connect_response fd trans_id >>= *)
  (*       try_announce 0 *)
  (*     end begin function *)
  (*     | Unix.Unix_error (Unix.ETIMEDOUT, _, _) -> *)
  (*       if n >= 8 then *)
  (*         Lwt.fail (Failure "connect_request: too many retries") *)
  (*       else begin *)
  (*         debug "udp connect request timeout after %.0fs; retrying..." (15.0 *. 2.0 ** float n); *)
  (*         try_connect (n+1) *)
  (*       end *)
  (*     | exn -> *)
  (*       Lwt.fail exn *)
  (*     end *)
  (*   and try_announce n conn_id = *)
  (*     send_announce_request fd addr conn_id ih ?up ?down ?left event ?port id >>= fun trans_id -> *)
  (*     UDP.set_timeout fd (15.0 *. 2.0 ** float n); *)
  (*     Lwt.catch begin fun () -> *)
  (*       read_announce_response fd trans_id *)
  (*     end begin function *)
  (*     | Unix.Unix_error (Unix.ETIMEDOUT, _, _) -> *)
  (*       debug "udp announce request timeout after %.0fs; retrying..." (15.0 *. 2.0 ** float n); *)
  (*       if n >= 2 then *)
  (*         try_connect (n+1) *)
  (*       else *)
  (*         try_announce (n+1) conn_id *)
  (*     | exn -> *)
  (*       Lwt.fail exn *)
  (*     end *)
  (*   in *)
  (*   try_connect 0 *)

    (* let rec loop = function *)
    (*   | Connect_request n -> *)
    (*     let trans_id = fresh_transaction_id () in *)
    (*     UDP.send_bitstring fd (connect_request trans_id) addr >>= fun () -> *)
    (*     UDP.set_timeout fd (15.0 *. 2.0 ** float n); *)
    (*     Lwt.catch begin fun () -> *)
    (*       loop (Connect_response trans_id) *)
    (*     end begin function *)
    (*     | Unix.Unix_error (Unix.ETIMEDOUT, _, _) -> *)
    (*       if n >= 8 then *)
    (*         failwith_lwt "connect_request: too many retries" *)
    (*       else begin *)
    (*         debug "udp connect request timeout after %ds; retrying..." *)
    (*           (truncate (15.0 *. 2.0 ** float n)); *)
    (*         loop (Connect_request (n+1)) *)
    (*       end *)
    (*     | exn -> Lwt.fail exn *)
    (*     end *)
    (*   | Connect_response trans_id -> *)
    (*     UDP.recv fd >>= fun (s, _) -> *)
    (*     begin match connect_response trans_id s with *)
    (*     | `Error msg -> Lwt.fail (Error msg) *)
    (*     | `Ok conn_id -> loop (Announce_request (conn_id, 0)) *)
    (*     end *)
    (*   | Announce_request (conn_id, n) -> *)
    (*     let trans_id = fresh_transaction_id () in *)
    (*     let create_packet = announce_request conn_id trans_id ih ?up ?down ?left event ?port id in *)
    (*     UDP.send_bitstring fd create_packet addr >>= fun () -> *)
    (*     UDP.set_timeout fd (15.0 *. 2.0 ** float n); *)
    (*     Lwt.catch begin fun () -> *)
    (*       loop (Announce_response trans_id) *)
    (*     end begin function *)
    (*     | Unix.Unix_error (Unix.ETIMEDOUT, _, _) -> *)
    (*       debug "udp announce request timeout after %ds; retrying..." *)
    (*         (truncate (15.0 *. 2.0 ** float n)); *)
    (*       if n >= 2 then *)
    (*         loop (Connect_request (n+1)) *)
    (*       else *)
    (*         loop (Announce_request (conn_id, n+1)) *)
    (*     | exn -> *)
    (*       Lwt.fail exn *)
    (*     end *)
    (*   | Announce_response trans_id -> *)
    (*     Lwt.catch begin fun () -> *)
    (*       UDP.recv fd >>= fun (s, _) -> *)
    (*       match announce_response trans_id s with *)
    (*       | `Error msg -> Lwt.fail (Error msg) *)
    (*       | `Ok resp -> Lwt.return resp *)
    (*     end Lwt.fail *)
    (* in *)
    (* loop (Connect_request 0) *)

  let announce tr ih ?up ?down ?left ?event ?port id =
    let url = tr in
    let host = match Uri.host url with
      | None -> failwith "Empty Hostname"
      | Some host -> host
    in
    let p = match Uri.port url with
      | None -> failwith "Empty Port"
      | Some port -> port
    in
    let addr = (Addr.Ip.of_string host, p) in
    let fd = UDP.create_socket () in
    do_announce fd addr ih ?up ?down ?left ?event ?port id
end

(* module HttpTracker = struct *)
(*   let either f g x = *)
(*     try f x with _ -> g x *)

(*   let decode_response (d : Bcode.t) = *)
(*     let success () = *)
(*       let seeders = *)
(*         try Some (Bcode.find "complete" d |> Bcode.to_int) with Not_found -> None *)
(*       in *)
(*       let leechers = *)
(*         try Some (Bcode.find "incomplete" d |> Bcode.to_int) with Not_found -> None *)
(*       in *)
(*       let interval = Bcode.find "interval" d |> Bcode.to_int in *)
(*       (\* let min_interval = try Some (Bcode.find "min interval" d) with _ -> None in *\) *)
(*       let compact_peers peers = *)
(*         let peers = Bcode.to_string peers in *)
(*         let rec loop bs = *)
(*           bitmatch bs with *)
(*           | { addr : 6 * 8 : bitstring; bs : -1 : bitstring } -> *)
(*             Addr.of_string_compact addr :: loop bs *)
(*           | { _ } -> *)
(*             [] *)
(*         in *)
(*         loop (Bitstring.bitstring_of_string peers) *)
(*       in *)
(*       let usual_peers peers = *)
(*         Bcode.to_list peers |> *)
(*         List.map (fun d -> *)
(*             let ip = Bcode.find "ip" d |> Bcode.to_string in *)
(*             let port = Bcode.find "port" d |> Bcode.to_int in *)
(*             let addr = Addr.Ip.of_string ip in *)
(*             (addr, port)) *)
(*       in *)
(*       let peers = Bcode.find "peers" d in *)
(*       let peers = either compact_peers usual_peers peers in *)
(*       Lwt.return {peers; leechers; seeders; interval} *)
(*     in *)
(*     let error () = *)
(*       let s = Bcode.find "failure reason" d |> Bcode.to_string in *)
(*       Lwt.fail (Error s) *)
(*     in *)
(*     let warning () = *)
(*       let s = Bcode.find "warning message" d |> Bcode.to_string in *)
(*       Lwt.fail (Warning s) *)
(*     in *)
(*     either warning (either error success) () *)

(*   let announce tr ih ?up ?down ?left ?event ?port id = *)
(*     let uri = ref tr in *)
(*     let add name x = uri := Uri.add_query_param' !uri (name, x) in *)
(*     let add_opt name f = function *)
(*       | None -> () *)
(*       | Some x -> uri := Uri.add_query_param' !uri (name, f x) *)
(*     in *)
(*     add "info_hash" (SHA1.to_bin ih); *)
(*     add "peer_id" (SHA1.to_bin id); *)
(*     add_opt "uploaded" Int64.to_string up; *)
(*     add_opt "downloaded" Int64.to_string down; *)
(*     add_opt "left" Int64.to_string left; *)
(*     add_opt "port" string_of_int port; *)
(*     add "compact" "1"; *)
(*     add_opt "event" string_of_event event; *)
(*     Cohttp_lwt_unix.Client.get !uri >>= fun (_, body) -> *)
(*     Cohttp_lwt_body.to_string body >>= fun body -> *)
(*     debug "received response from http tracker: %S" body; *)
(*     try *)
(*       Bcode.decode body |> decode_response *)
(*     with exn -> *)
(*       Lwt.fail (Failure ("http error: decode error: " ^ Printexc.to_string exn)) *)
(* end *)

(* let query tr ~ih ?up ?down ?left ?event ?port ~id = *)
(*   debug "announcing on %S" (Uri.to_string tr); *)
(*   match Uri.scheme tr with *)
(*   | Some "http" | Some "https" -> *)
(*     HttpTracker.announce tr ih ?up ?down ?left ?event ?port id *)
(*   | Some "udp" -> *)
(*     UdpTracker.announce tr ih ?up ?down ?left ?event ?port id *)
(*   | Some sch -> *)
(*     failwith_lwt "unknown tracker url scheme: %S" sch *)
(*   | None -> *)
(*     failwith_lwt "missing tracker url scheme" *)

(* module Tier = struct *)
(*   type t = Uri.t array *)

(*   let create tier = *)
(*     let a = Array.of_list tier in *)
(*     Util.shuffle_array a; *)
(*     a *)

(*   let query tier ~ih ?up ?down ?left ?event ?port ~id = *)
(*     let rec loop i = *)
(*       if i >= Array.length tier then Lwt.fail (Failure "all trackers in tier failed") *)
(*       else *)
(*         let tr = tier.(i) in *)
(*         Lwt.catch begin fun () -> *)
(*           query tr ~ih ?up ?down ?left ?event ?port ~id >>= fun resp -> *)
(*           if i > 0 then begin *)
(*             (\* move the first successful tracker to the front of the tier *\) *)
(*             Array.blit tier 0 tier 1 i; *)
(*             tier.(0) <- tr; *)
(*           end; *)
(*           Lwt.return resp *)
(*         end begin fun exn -> *)
(*           debug ~exn "error announcing to %S" (Uri.to_string tr); *)
(*           loop (i+1) *)
(*         end *)
(*     in *)
(*     loop 0 *)

(*   let strl f l = *)
(*     "[" ^ String.concat " " (List.map f l) ^ "]" *)

(*   let to_string tier = *)
(*     strl Uri.to_string (Array.to_list tier) *)
(* end *)
