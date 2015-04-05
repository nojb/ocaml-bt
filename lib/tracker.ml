(* The MIT License (MIT)

   Copyright (c) 2015 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

module Log = Log.Make (struct let section = "[Tracker]" end)

type event =
  [ `Started
  | `None
  | `Stopped
  | `Completed ]

type addr = Unix.inet_addr * int

module Udp = struct

  type state =
    | WaitConnectResponse of int32 * int
    | WaitAnnounceResponse of int32 * int64 * int

  let (>>=) m f = match m with `Ok x -> f x | `Error _ as e -> e

  type udp =
    { info_hash : SHA1.t;
      id : SHA1.t;
      up : int64;
      down : int64;
      left : int64;
      event : event;
      port : int;
      state : state }

  type t =
    [ `Udp of udp ]

  let generate_trans_id () =
    Random.int32 Int32.max_int

  let parse_connect_response cs =
    match Cstruct.BE.get_uint32 cs 0 with
    | 3l ->
        `Error Cstruct.(to_string @@ shift cs 4)
    | n ->
        let trans_id' = Cstruct.BE.get_uint32 cs 4 in
        let conn_id = Cstruct.BE.get_uint64 cs 8 in
        `Ok (trans_id', conn_id)

  let make_connect_request trans_id =
    let cs = Cstruct.create 16 in
    Cstruct.BE.set_uint64 cs 0 0x41727101980L;
    Cstruct.BE.set_uint32 cs 8 0l;
    Cstruct.BE.set_uint32 cs 12 trans_id;
    cs

  let int32_of_event = function
    | `None -> 0l
    | `Completed -> 1l
    | `Started -> 2l
    | `Stopped -> 3l

  let make_announce_request conn_id trans_id t =
    let cs = Cstruct.create (8 + 4 + 4 + 20 + 20 + 8 + 8 + 8 + 4 + 4 + 4 + 4 + 2) in
    Cstruct.BE.set_uint64 cs 0 conn_id;
    Cstruct.BE.set_uint32 cs 8 1l;
    Cstruct.BE.set_uint32 cs 12 trans_id;
    Cstruct.blit (SHA1.to_raw t.info_hash) 0 cs 16 20;
    Cstruct.blit (SHA1.to_raw t.id) 0 cs 36 20;
    Cstruct.BE.set_uint64 cs 56 t.down;
    Cstruct.BE.set_uint64 cs 64 t.left;
    Cstruct.BE.set_uint64 cs 72 t.up;
    Cstruct.BE.set_uint32 cs 80 (int32_of_event t.event);
    Cstruct.BE.set_uint32 cs 84 0l;
    Cstruct.BE.set_uint32 cs 88 0l;
    Cstruct.BE.set_uint32 cs 92 (-1l);
    Cstruct.BE.set_uint16 cs 96 t.port;
    cs

  let parse_announce_response cs =
    match Cstruct.BE.get_uint32 cs 0 with
    | 3l ->
        `Error Cstruct.(to_string @@ shift cs 4)
    | 1l ->
        let trans_id = Cstruct.BE.get_uint32 cs 4 in
        let interval = Cstruct.BE.get_uint32 cs 8 in
        let leechers = Cstruct.BE.get_uint32 cs 12 in
        let seeders = Cstruct.BE.get_uint32 cs 16 in
        let rec loop buf =
          if Cstruct.len buf >= 6 then
            let p, buf = Cstruct.split buf 6 in
            let ip =
              Printf.sprintf "%d.%d.%d.%d"
                (Cstruct.get_uint8 p 0) (Cstruct.get_uint8 p 1)
                (Cstruct.get_uint8 p 2) (Cstruct.get_uint8 p 3)
            in
            let ip = Unix.inet_addr_of_string ip in
            let port = Cstruct.BE.get_uint16 p 4 in
            (ip, port) :: loop buf
          else
            []
        in
        let peers = loop (Cstruct.shift cs 20) in
        `Ok (trans_id, interval, leechers, seeders, peers)
    | _ ->
        `Error "parse_announce_response"

  let delay n = 15. *. 2. ** float n

  let timeout t =
    match t.state with
    | WaitConnectResponse (_, n) when n >= 8 ->
        `Error "too many retries"

    | WaitConnectResponse (_, n) ->
        let n = n + 1 in
        let trans_id = generate_trans_id () in
        `Ok (WaitConnectResponse (trans_id, n), (delay n, make_connect_request trans_id))

    | WaitAnnounceResponse (_, _, n) when n >= 2 ->
        let trans_id = generate_trans_id () in
        `Ok (WaitConnectResponse (trans_id, 0), (delay 0, make_connect_request trans_id))

    | WaitAnnounceResponse (_, conn_id, n) ->
        let n = n + 1 in
        let trans_id = generate_trans_id () in
        `Ok (WaitAnnounceResponse (trans_id, conn_id, n), (delay n, make_announce_request conn_id trans_id t))

  let timeout (`Udp t) =
    timeout t >>= fun (state, dbuf) -> `Ok (`Udp { t with state }, dbuf)

  let handle (`Udp t) buf =
    match t.state with
    | WaitConnectResponse (trans_id, _) ->
        parse_connect_response buf >>= fun (trans_id', conn_id) ->
        if trans_id' <> trans_id then
          `Error "trans_id don't match"
        else
          let trans_id = generate_trans_id () in
          let req = make_announce_request conn_id trans_id t in
          let t = `Udp { t with state = WaitAnnounceResponse (trans_id, conn_id, 0) } in
          `Ok (t, (delay 0, req))

    | WaitAnnounceResponse (trans_id, conn_id, n) ->
        parse_announce_response buf >>= fun (trans_id', interval, leechers, seeders, peers) ->
        if trans_id <> trans_id' then
          `Error "trans_id don't match"
        else
          `Success (Int32.to_float interval, Int32.to_int leechers, Int32.to_int seeders, peers)

  let create ~up ~left ~down ~info_hash ~id ~port ~event  =
    let trans_id = generate_trans_id () in
    let state = WaitConnectResponse (trans_id, 0) in
    let t = { up; left; down; info_hash; id; port; event; state } in
    `Ok (`Udp t, (delay 0, make_connect_request trans_id))

  (* let do_announce fd addr ih ?up ?down ?left ?(event = `None) ?port id = *)
  (*   let rec try_connect n = *)
  (*     send_connect_request fd addr >>= fun trans_id -> *)
  (*     UDP.set_timeout fd (15.0 *. 2.0 ** float n); *)
  (*     Lwt.catch begin fun () -> *)
  (*       read_connect_response fd trans_id >>= *)
  (*       try_announce 0 *)
  (*     end begin function *)
  (*     | Unix.Unix_error (Unix.ETIMEDOUT, _, _) -> *)
  (*         if n >= 8 then *)
  (*           Lwt.fail (Failure "connect_request: too many retries") *)
  (*         else begin *)
  (*           debug "udp connect request timeout after %.0fs; retrying..." (15.0 *. 2.0 ** float n); *)
  (*           try_connect (n+1) *)
  (*         end *)
  (*     | exn -> *)
  (*         Lwt.fail exn *)
  (*     end *)
  (*   and try_announce n conn_id = *)
  (*     send_announce_request fd addr conn_id ih ?up ?down ?left event ?port id >>= fun trans_id -> *)
  (*     UDP.set_timeout fd (15.0 *. 2.0 ** float n); *)
  (*     Lwt.catch begin fun () -> *)
  (*       read_announce_response fd trans_id *)
  (*     end begin function *)
  (*     | Unix.Unix_error (Unix.ETIMEDOUT, _, _) -> *)
  (*         debug "udp announce request timeout after %.0fs; retrying..." (15.0 *. 2.0 ** float n); *)
  (*         if n >= 2 then *)
  (*           try_connect (n+1) *)
  (*         else *)
  (*           try_announce (n+1) conn_id *)
  (*     | exn -> *)
  (*         Lwt.fail exn *)
  (*     end *)
  (*   in *)
  (*   try_connect 0 *)

end

(* module Http = struct *)
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
(*               [] (\* FIXME FIXME *\) *)
(*           (\* Addr.of_string_compact addr :: loop bs *\) *)
(*           | { _ } -> *)
(*               [] *)
(*         in *)
(*         loop (Bitstring.bitstring_of_string peers) *)
(*       in *)
(*       let usual_peers peers = *)
(*         Bcode.to_list peers |> *)
(*         List.map (fun d -> *)
(*           let ip = Bcode.find "ip" d |> Bcode.to_string in *)
(*           let port = Bcode.find "port" d |> Bcode.to_int in *)
(*           let addr = (assert false) in (\* FIXME FIXME *\) *)
(*           (\* let addr = Addr.Ip.of_string ip in *\) *)
(*           (addr, port)) *)
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
(*     add "info_hash" (Cstruct.to_string @@ SHA1.to_raw ih); *)
(*     add "peer_id" (Cstruct.to_string @@ SHA1.to_raw id); *)
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
(*       Cstruct.of_string body |> Bcode.decode |> decode_response *)
(*     with exn -> *)
(*       Lwt.fail (Failure ("http error: decode error: " ^ Printexc.to_string exn)) *)
(* end *)

module Http = struct
  type t = [ `Http of unit ]
  let timeout _ = `Error "not implemented"
  let handle _ _ = `Error "not implemented"
end

type t =
  [ Udp.t
  | Http.t ]

let create ?(up = 0L) ?(left = 0L) ?(down = 0L) ?(event = `None) ?(port = 0) ~id ~info_hash proto =
  match proto with
  | `Http ->
      failwith "not implemented"
      (* Http.announce tr ih ?up ?down ?left ?event ?port id *)
  | `Udp ->
      Udp.create ~up ~left ~down ~event ~port ~id ~info_hash

type ret =
  [ `Ok of t * (float * Cstruct.t)
  | `Error of string
  | `Success of float * int * int * addr list ]

let timeout = function
  | `Udp _ as t ->
      Udp.timeout t
  | `Http _ as t ->
      Http.timeout t

let handle t buf : ret =
  match t with
  | `Udp _ as t ->
      Udp.handle t buf
  | `Http _ as t ->
      Http.handle t buf

(* IO *)

open Lwt.Infix

let max_datagram_size = 1024

let debug url fmt =
  Log.debug ("[%s] " ^^ fmt) (Uri.to_string url)

let error url fmt =
  Log.error ("[%s] " ^^ fmt) (Uri.to_string url)

let announce ~info_hash url push id =
  let read_buf = Cstruct.create max_datagram_size in
  let rec loop fd = function
    | `Ok (t, (timeout, buf)) ->
        Lwt_cstruct.write fd buf >>= fun n ->
        debug url "writing %u bytes (timeout=%.1f)" n timeout;
        Lwt_unix.with_timeout timeout (fun () -> Lwt_cstruct.read fd read_buf) >>= fun n ->
        loop fd (handle t (Cstruct.sub read_buf 0 n))
    | `Error s ->
        error url "error : %S" s;
        Lwt.fail (Failure s)
    | `Success (interval, leechers, seeders, peers) ->
        debug url "received %d peers" (List.length peers);
        push peers;
        Lwt_unix.sleep interval >>= fun () ->
        loop fd (create ~info_hash ~id `Udp)
  in
  match Uri.scheme url with
  | Some "udp" ->
      let h = match Uri.host url with None -> assert false | Some h -> h in
      let p = match Uri.port url with None -> assert false | Some p -> p in
      Lwt_unix.gethostbyname h >>= fun he ->
      let ip = he.Unix.h_addr_list.(0) in
      let fd = Lwt_unix.(socket PF_INET SOCK_DGRAM 0) in
      let sa = Lwt_unix.ADDR_INET (ip, p) in
      Lwt_unix.connect fd sa >>= fun () ->
      loop fd (create ~info_hash ~id `Udp)
  | Some s ->
      error url "tracker scheme %S not support" s;
      Lwt.fail (Failure "tracker scheme not supported")
  | None ->
      error url "no tracker scheme";
      Lwt.fail (Failure "no tracker scheme")

let announce ~info_hash push id url =
  Lwt.ignore_result @@ Lwt.catch
    (fun () -> announce ~info_hash url push id)
    (fun exn -> error url "exn : %s" (Printexc.to_string exn); Lwt.return_unit)
