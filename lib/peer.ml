module Handshake = struct
  type t = { pstr : string; info_hash : string; peer_id : string }

  let to_sexp t =
    let open Sexp.Encoder in
    record
      [
        ("pstr", string t.pstr);
        ("info_hash", string t.info_hash);
        ("peer_id", string t.peer_id);
      ]

  let create ~info_hash ~peer_id =
    { pstr = "BitTorrent protocol"; info_hash; peer_id }

  let parser =
    let open Eio.Buf_read.Syntax in
    let* len = Eio.Buf_read.any_char in
    let len = Char.code len in
    let+ pstr = Eio.Buf_read.take len
    and+ () = Eio.Buf_read.skip 8
    and+ info_hash = Eio.Buf_read.take 20
    and+ peer_id = Eio.Buf_read.take 20 in
    { pstr; info_hash; peer_id }

  let write t buf =
    Eio.Buf_write.uint8 buf (String.length t.pstr);
    Eio.Buf_write.string buf t.pstr;
    Eio.Buf_write.string buf (String.make 8 '\000');
    Eio.Buf_write.string buf t.info_hash;
    Eio.Buf_write.string buf t.peer_id
end

module Message = struct
  type t =
    | Keepalive
    | Choke
    | Unchoke
    | Interested
    | Not_interested
    | Have of int
    | Bitfield of string
    | Request of { i : int; ofs : int; len : int }
    | Piece of { i : int; ofs : int; data : string }
    | Cancel of { i : int; ofs : int; len : int }

  let to_sexp t =
    let open Sexp.Encoder in
    match t with
    | Keepalive -> variant "keepalive" []
    | Choke -> variant "choke" []
    | Unchoke -> variant "unchoke" []
    | Interested -> variant "interested" []
    | Not_interested -> variant "not_interested" []
    | Have i -> variant "have" [ int i ]
    | Bitfield s -> variant "bitfield" [ string s ]
    | Request { i; ofs; len } -> variant "request" [ int i; int ofs; int len ]
    | Piece { i; ofs; data } -> variant "piece" [ int i; int ofs; string data ]
    | Cancel { i; ofs; len } -> variant "cancel" [ int i; int ofs; int len ]

  let get_int s ofs =
    match Int32.unsigned_to_int (String.get_int32_be s ofs) with
    | Some n -> n
    | None -> assert false

  let parser =
    let open Eio.Buf_read.Syntax in
    let* len = Eio.Buf_read.take 4 in
    let len = get_int len 0 in
    if len = 0 then Eio.Buf_read.return Keepalive
    else
      let+ msg = Eio.Buf_read.take len in
      match Char.code msg.[0] with
      | 0 -> Choke
      | 1 -> Unchoke
      | 2 -> Interested
      | 3 -> Not_interested
      | 4 -> Have (get_int msg 1)
      | 5 -> Bitfield (String.sub msg 1 (String.length msg - 1))
      | 6 ->
          let i = get_int msg 1 in
          let ofs = get_int msg 5 in
          let len = get_int msg 9 in
          Request { i; ofs; len }
      | 7 ->
          let i = get_int msg 1 in
          let ofs = get_int msg 5 in
          let data = String.sub msg 9 (String.length msg - 9) in
          Piece { i; ofs; data }
      | 8 ->
          let i = get_int msg 1 in
          let ofs = get_int msg 5 in
          let len = get_int msg 9 in
          Cancel { i; ofs; len }
      | _ -> assert false

  let write t buf =
    match t with
    | Keepalive -> Eio.Buf_write.BE.uint32 buf 0l
    | Choke ->
        Eio.Buf_write.BE.uint32 buf 1l;
        Eio.Buf_write.uint8 buf 0
    | Unchoke ->
        Eio.Buf_write.BE.uint32 buf 1l;
        Eio.Buf_write.uint8 buf 1
    | Interested ->
        Eio.Buf_write.BE.uint32 buf 1l;
        Eio.Buf_write.uint8 buf 2
    | Not_interested ->
        Eio.Buf_write.BE.uint32 buf 1l;
        Eio.Buf_write.uint8 buf 3
    | Have i ->
        Eio.Buf_write.BE.uint32 buf 5l;
        Eio.Buf_write.uint8 buf 4;
        Eio.Buf_write.BE.uint32 buf (Int32.of_int i)
    | Bitfield bits ->
        Eio.Buf_write.BE.uint32 buf (Int32.of_int (1 + String.length bits));
        Eio.Buf_write.uint8 buf 5;
        Eio.Buf_write.string buf bits
    | Request { i; ofs; len } ->
        Eio.Buf_write.BE.uint32 buf 13l;
        Eio.Buf_write.uint8 buf 6;
        Eio.Buf_write.BE.uint32 buf (Int32.of_int i);
        Eio.Buf_write.BE.uint32 buf (Int32.of_int ofs);
        Eio.Buf_write.BE.uint32 buf (Int32.of_int len)
    | Piece { i; ofs; data } ->
        Eio.Buf_write.BE.uint32 buf 13l;
        Eio.Buf_write.uint8 buf 7;
        Eio.Buf_write.BE.uint32 buf (Int32.of_int i);
        Eio.Buf_write.BE.uint32 buf (Int32.of_int ofs);
        Eio.Buf_write.string buf data
    | Cancel { i; ofs; len } ->
        Eio.Buf_write.BE.uint32 buf 13l;
        Eio.Buf_write.uint8 buf 8;
        Eio.Buf_write.BE.uint32 buf (Int32.of_int i);
        Eio.Buf_write.BE.uint32 buf (Int32.of_int ofs);
        Eio.Buf_write.BE.uint32 buf (Int32.of_int len)
end

type error =
  [ `Connect_failed of [ `Exn of exn | `Timeout ]
  | `Handshake_failed of
    [ `Exn of exn | `Msg of string | `Timeout | `Info_hash_mismatch ]
  | `Expected_bitfield
  | `Msg of string ]

let string_of_error = function
  | `Connect_failed err ->
      let msg =
        match err with
        | `Exn exn -> Printexc.to_string exn
        | `Timeout -> "timeout"
      in
      Printf.sprintf "connect failed: %s" msg
  | `Handshake_failed err ->
      let msg =
        match err with
        | `Exn exn -> Printexc.to_string exn
        | `Msg msg -> msg
        | `Timeout -> "timeout"
        | `Info_hash_mismatch -> "info hash mismatch"
      in
      Printf.sprintf "handshake failed: %s" msg
  | `Expected_bitfield -> "did not receive bitfield"
  | `Msg msg -> Printf.sprintf "connection error: %s" msg

let connect ~sw ~net ~clock addr port =
  let stream = `Tcp (Eio_unix.Ipaddr.of_unix addr, port) in
  Eio.traceln ~__POS__ "Connecting to %s" (Unix.string_of_inet_addr addr);
  match
    Eio.Time.with_timeout_exn clock 3.0 (fun () ->
        Eio.Net.connect ~sw net stream)
  with
  | flow ->
      Eio.traceln ~__POS__ "Connected to %s%!" (Unix.string_of_inet_addr addr);
      Ok (flow :> Eio.Net.stream_socket)
  | exception Eio.Time.Timeout -> Error (`Connect_failed `Timeout)
  | exception exn ->
      Eio.traceln ~__POS__ "Connection to %s failed: %s%!"
        (Unix.string_of_inet_addr addr)
        (Printexc.to_string exn);
      Error (`Connect_failed (`Exn exn))

let complete_handshake ~clock ~flow ~info_hash ~peer_id =
  let h = Handshake.create ~info_hash ~peer_id in
  match
    Eio.Time.with_timeout clock 3.0 @@ fun () ->
    Eio.Buf_write.with_flow flow (fun buf -> Handshake.write h buf);
    Eio.Buf_read.format_errors Handshake.parser
      (Eio.Buf_read.of_flow ~max_size:100000 flow)
  with
  | Ok r ->
      if r.info_hash <> info_hash then
        Error (`Handshake_failed `Info_hash_mismatch)
      else Ok r
  | Error err -> Error (`Handshake_failed err)
  | exception exn -> Error (`Handshake_failed (`Exn exn))

let receive_bitfield ~flow =
  match
    Eio.Buf_read.format_errors Message.parser
      (Eio.Buf_read.of_flow ~max_size:1_000_000 flow)
  with
  | Error err -> Error err
  | Ok (Bitfield bits) -> Ok bits
  | Ok _ -> Error `Expected_bitfield

type t = {
  flow : Eio.Net.stream_socket;
  info_hash : string;
  peer_id : string;
  bitfield : bytes;
  mutable choked : bool;
}

let run ~net ~clock ~sw ~info_hash ~peer_id addr port =
  match connect ~sw ~net ~clock addr port with
  | Error _ as err -> err
  | Ok flow -> (
      match complete_handshake ~clock ~flow ~info_hash ~peer_id with
      | Error _ as err -> err
      | Ok { Handshake.info_hash; peer_id; _ } -> (
          match receive_bitfield ~flow with
          | Error _ as err -> err
          | Ok bitfield ->
              let bitfield = Bytes.of_string bitfield in
              Ok { flow; info_hash; peer_id; bitfield; choked = true }))

let choked t = t.choked

let has_piece t i =
  assert (i >= 0);
  let byte_num = i / 8 in
  let byte_ofs = i mod 8 in
  (Char.code (Bytes.get t.bitfield byte_num) lsr (7 - byte_ofs)) land 1 <> 0

let set_piece t i =
  assert (i >= 0);
  let byte_num = i / 8 in
  let byte_ofs = i mod 8 in
  Bytes.set t.bitfield byte_num
    (Char.chr
       (Char.code (Bytes.get t.bitfield byte_num) lor ((1 lsl 7) - byte_ofs)))

let send_message t msg = Eio.Buf_write.with_flow t.flow (Message.write msg)
let send_unchoke t = send_message t Unchoke
let send_interested t = send_message t Interested
let send_request t ~i ~ofs ~len = send_message t (Request { i; ofs; len })

let read_message t =
  let msg = Message.parser (Eio.Buf_read.of_flow ~max_size:1_000_000 t.flow) in
  (match msg with
  | Choke -> t.choked <- true
  | Unchoke -> t.choked <- false
  | Have i -> set_piece t i
  | Keepalive | Interested | Not_interested | Bitfield _ | Request _ | Piece _
  | Cancel _ ->
      ());
  msg
