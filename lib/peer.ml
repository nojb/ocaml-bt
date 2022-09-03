module Handshake = struct
  type t =
    {
      pstr: string;
      info_hash: string;
      peer_id: string;
    }

  let to_sexp t =
    let open Sexp.Encoder in
    record
      [
        "pstr", string t.pstr;
        "info_hash", string t.info_hash;
        "peer_id", string t.peer_id;
      ]

  let create ~info_hash ~peer_id =
    {pstr = "BitTorrent protocol"; info_hash; peer_id}

  let parser =
    let open Eio.Buf_read.Syntax in
    let* len = Eio.Buf_read.any_char in
    let len = Char.code len in
    let+ pstr = Eio.Buf_read.take len
    and+ _ = Eio.Buf_read.take 8
    and+ info_hash = Eio.Buf_read.take 20
    and+ peer_id = Eio.Buf_read.take 20 in
    {pstr; info_hash; peer_id}

  let write t buf =
    Eio.Buf_write.uint8 buf (String.length t.pstr);
    Eio.Buf_write.string buf t.pstr;
    Eio.Buf_write.string buf (String.make 8 '\000');
    Eio.Buf_write.string buf t.info_hash;
    Eio.Buf_write.string buf t.peer_id
end

type t =
  {
    flow: Eio.Net.stream_socket;
  }

type error =
  [ `Connect_failed
  | `Connect_timeout
  | `Info_hash_mismatch
  | `Handshake_failed of [ `Exn of exn | `Msg of string | `Timeout ] ]

let string_of_error = function
  | `Connect_failed -> "connect failed"
  | `Connect_timeout -> "connect timed out"
  | `Info_hash_mismatch -> "info hash mismatch"
  | `Handshake_failed err ->
      let msg =
        match err with
        | `Exn exn -> Printexc.to_string exn
        | `Msg msg -> msg
        | `Timeout -> "timeout"
      in
      Printf.sprintf "handshake failed: %s" msg

let connect ~sw ~net ~clock addr port =
  let stream = `Tcp (Eio_unix.Ipaddr.of_unix addr, port) in
  Eio.traceln ~__POS__ "Connecting to %s" (Unix.string_of_inet_addr addr);
  match Eio.Time.with_timeout_exn clock 3.0 (fun () -> Eio.Net.connect ~sw net stream) with
  | flow ->
      Eio.traceln ~__POS__ "Connected to %s%!" (Unix.string_of_inet_addr addr);
      Ok {flow = (flow :> Eio.Net.stream_socket)}
  | exception Eio.Time.Timeout ->
      Error `Connect_timeout
  | exception exn ->
      Eio.traceln ~__POS__ "Connection to %s failed: %s%!" (Unix.string_of_inet_addr addr) (Printexc.to_string exn);
      Error `Connect_failed

let complete_handshake ~clock t ~info_hash ~peer_id =
  let h = Handshake.create ~info_hash ~peer_id in
  match
    Eio.Time.with_timeout clock 3.0 @@ fun () ->
    Eio.Buf_write.with_flow t.flow (fun buf -> Handshake.write h buf);
    Eio.Buf_read.format_errors Handshake.parser (Eio.Buf_read.of_flow ~max_size:100000 t.flow)
  with
  | Error err -> Error (`Handshake_failed err)
  | exception exn -> Error (`Handshake_failed (`Exn exn))
  | Ok r ->
      if r.info_hash <> info_hash then Error `Info_hash_mismatch
      else Ok r

let run ~net ~clock ~info_hash ~peer_id addr port =
  Eio.Switch.run @@ fun sw ->
  match connect ~sw ~net ~clock addr port with
  | Error _ as err -> err
  | Ok t -> complete_handshake ~clock t ~info_hash ~peer_id
