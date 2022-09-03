open Bt

let ( / ) = Eio.Path.( / )

let read_torrent_file ~dir path =
  let s = Eio.Path.load (dir / path) in
  let t =
    match Bencode.decode s with
    | Some t -> t
    | None -> failwith (Printf.sprintf "Could not parse torrent file %S" path)
  in
  match Bencode.Decoder.query Metainfo.decoder t with
  | Some t -> t
  | None -> failwith (Printf.sprintf "Could not parse metainfo file %S" path)

let random_string state n =
  String.init n (fun _ -> char_of_int (Random.State.int state 256))

let anon ~env ~rng path =
  let meta =
    let dir = Eio.Stdenv.fs env in
    read_torrent_file ~dir path
  in
  let info_hash = meta.Metainfo.info_hash in
  let peer_id = random_string rng 20 in
  let port = Random.State.int rng 10000 in
  let left = Metainfo.length meta in
  let net = Eio.Stdenv.net env in
  let resp =
    Tracker.announce ~net ~info_hash ~peer_id ~port ~uploaded:0 ~downloaded:0
      ~left meta.Metainfo.announce
  in
  let sexp = Tracker.Response.to_sexp resp in
  Format.printf "@[%a@]@." Sexp.print sexp;
  match resp with
  | Failure s -> failwith s
  | Ok { Tracker.Response.interval = _; peers; peers6 = _ } ->
      let clock = Eio.Stdenv.clock env in
      Torrent.download ~net ~clock ~info_hash ~peer_id ~meta ~peers

let anon path =
  let rng = Random.State.make_self_init () in
  Eio_main.run (fun env -> anon ~env ~rng path)

let spec = []
let () = Arg.parse (Arg.align spec) anon ""
