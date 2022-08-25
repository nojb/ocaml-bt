module Event = struct
  type t = Started | Completed | Stopped

  let to_string = function
    | Started -> "started"
    | Completed -> "completed"
    | Stopped -> "stopped"
end

module Response = struct
  module Peer = struct
    type t = {
      ip : [ `Ipaddr of Unix.inet_addr | `Name of string ];
      port : int;
    }

    let to_sexp t =
      let open Sexp.Encoder in
      record
        [
          ( "ip",
            string
              (match t.ip with
              | `Ipaddr addr -> Unix.string_of_inet_addr addr
              | `Name s -> s) );
          ("port", int t.port);
        ]

    let decoder =
      let open Bencode.Decoder in
      let open Bencode.Decoder.O in
      let+ ip = member "ip" string and+ port = member "port" int in
      { ip = `Name ip; port }

    let decoder_compact n =
      let open Bencode.Decoder in
      let open Bencode.Decoder.O in
      let+ s = string in
      assert (String.length s mod (n + 2) = 0);
      let rec loop i =
        if i = String.length s then []
        else
          let addr =
            Eio_unix.Ipaddr.to_unix (Eio.Net.Ipaddr.of_raw (String.sub s i n))
          in
          let port = String.get_uint16_be s (i + n) in
          { ip = `Ipaddr addr; port } :: loop (i + n + 2)
      in
      loop 0
  end

  type ok = { interval : int; peers : Peer.t list; peers6 : Peer.t list }

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
       and+ peers =
         member "peers" (if_list Peer.decoder (Peer.decoder_compact 4))
       and+ peers6 =
         member "peers6" (if_list Peer.decoder (Peer.decoder_compact 16))
       in
       Ok { interval; peers; peers6 })

  let of_string s =
    match Bencode.decode s with
    | None -> None
    | Some t -> Bencode.Decoder.query decoder t
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
  Printf.sprintf "GET %s?%s HTTP/1.1\r\n\r\n" route (build_params params)

let parse_url url =
  match Scanf.sscanf_opt url "%s@://%s" (fun proto rest -> (proto, rest)) with
  | Some ("http", rest) -> (
      match String.split_on_char '/' rest with
      | host :: rest ->
          let host, port =
            match String.split_on_char ':' host with
            | [ host; port ] -> (
                match int_of_string_opt port with
                | None -> raise Exit
                | Some n -> (host, n))
            | [ host ] -> (host, 80)
            | _ -> raise Exit
          in
          ( host,
            port,
            String.concat "" (List.concat_map (fun s -> [ "/"; s ]) rest) )
      | [] -> raise Exit)
  | _ -> raise Exit

let parse_url url =
  match parse_url url with x -> Some x | exception Exit -> None

let announce ~net ~info_hash ~peer_id ~port ~uploaded ~downloaded ~left ?event
    url =
  let server_host, server_port, server_route =
    match parse_url url with
    | Some x -> x
    | None -> failwith (Printf.sprintf "Could not parse URL %s" url)
  in
  let payload =
    let params =
      [
        ("info_hash", info_hash);
        ("peer_id", peer_id);
        ("port", string_of_int port);
        ("uploaded", string_of_int uploaded);
        ("downloaded", string_of_int downloaded);
        ("left", string_of_int left);
        ("compact", "1");
      ]
    in
    let params =
      match event with
      | None -> params
      | Some event -> ("event", Event.to_string event) :: params
    in
    build_request server_route params
  in
  let addr =
    let he = Unix.gethostbyname server_host in
    Eio_unix.Ipaddr.of_unix he.Unix.h_addr_list.(0)
  in
  let addr = `Tcp (addr, server_port) in
  Eio.Switch.run @@ fun sw ->
  let flow = Eio.Net.connect ~sw net addr in
  Eio.Flow.copy_string payload flow;
  let buf = Eio.Buf_read.of_flow flow ~initial_size:100 ~max_size:1_000_000 in
  let rec loop () =
    match Eio.Buf_read.format_errors Eio.Buf_read.line buf with
    | Ok "" -> (
        match Response.of_string (Eio.Buf_read.take_all buf) with
        | None -> failwith "could not parse response"
        | Some r -> r)
    | Ok _ -> loop ()
    | Error (`Msg s) -> failwith s
  in
  loop ()
