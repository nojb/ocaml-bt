module Event = struct
  type t = Started | Completed | Stopped

  let to_string = function
    | Started -> "started"
    | Completed -> "completed"
    | Stopped -> "stopped"
end

module Response = struct
  module Peer = struct
    type t = { id : string; ip : string; port : int }

    let to_sexp t =
      let open Sexp.Encoder in
      record [ ("id", string t.id); ("ip", string t.ip); ("port", int t.port) ]

    let decoder =
      let open Bencode.Decoder in
      let open Bencode.Decoder.O in
      let+ id = member "id" string
      and+ ip = member "ip" string
      and+ port = member "port" int in
      { id; ip; port }
  end

  type ok = { interval : int; peers : Peer.t list }

  type t = Failure of string | Ok of ok

  let to_sexp t =
    let open Sexp.Encoder in
    match t with
    | Failure s -> variant "failure" [ string s ]
    | Ok t ->
        record
          [ ("interval", int t.interval); ("peers", list Peer.to_sexp t.peers) ]

  let decoder =
    let open Bencode.Decoder in
    let open Bencode.Decoder.O in
    if_member "failure reason"
      (let+ s = string in
       Failure s)
      (let+ interval = member "interval" int
       and+ peers = member "peers" (list Peer.decoder) in
       Ok { interval; peers })
end

let urlencode s =
  let len = String.length s in
  let buf = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
    | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '~' | '-' | '.' | '_') as c ->
        Buffer.add_char buf c
    | '+' -> Buffer.add_string buf "%2B"
    | ' ' -> Buffer.add_char buf '+'
    | c -> Printf.bprintf buf "%%%02X" (Char.code c)
  done;
  Buffer.contents buf

let build_params params =
  String.concat "&"
    (List.map
       (fun (k, v) -> String.concat "=" [ urlencode k; urlencode v ])
       params)

let build_request route params =
  Printf.printf "GET %s?%s\r\n" route (build_params params)

let decode_response s =
  match Bencode.decode s with
  | None -> None
  | Some t -> Bencode.Decoder.query Response.decoder t

let announce ~info_hash ~peer_id ~port ~uploaded ~downloaded ~left ?event url =
  let params =
    [
      ("info_hash", info_hash);
      ("peer_id", peer_id);
      ("port", string_of_int port);
      ("uploaded", string_of_int uploaded);
      ("downloaded", string_of_int downloaded);
      ("left", string_of_int left);
    ]
  in
  let params =
    match event with
    | None -> params
    | Some event -> ("event", Event.to_string event) :: params
  in
  let request = build_request route params in
  assert false

(* let announce tr ih ?up ?down ?left ?event ?port id = *)
(*   let uri = ref tr in *)
(*   let add name x = uri := Uri.add_query_param' !uri (name, x) in *)
(*   let add_opt name f = function *)
(*     | None -> () *)
(*     | Some x -> uri := Uri.add_query_param' !uri (name, f x) *)
(*   in *)
(*   add "info_hash" (Cstruct.to_string @@ SHA1.to_raw ih); *)
(*   add "peer_id" (Cstruct.to_string @@ SHA1.to_raw id); *)
(*   add_opt "uploaded" Int64.to_string up; *)
(*   add_opt "downloaded" Int64.to_string down; *)
(*   add_opt "left" Int64.to_string left; *)
(*   add_opt "port" string_of_int port; *)
(*   add "compact" "1"; *)
(*   add_opt "event" string_of_event event; *)
(*   Cohttp_lwt_unix.Client.get !uri >>= fun (_, body) -> *)
(*   Cohttp_lwt_body.to_string body >>= fun body -> *)
(*   debug "received response from http tracker: %S" body; *)
(*   try *)
(*     Cstruct.of_string body |> Bcode.decode |> decode_response *)
(*   with exn -> *)
(*     Lwt.fail (Failure ("http error: decode error: " ^ Printexc.to_string exn)) *)
