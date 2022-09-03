open Bt

let spec = []

let read_torrent_file path =
  let s = In_channel.with_open_bin path In_channel.input_all in
  let t =
    match Bencode.decode s with
    | Some t -> t
    | None -> failwith (Printf.sprintf "Could not parse torrent file %S" path)
  in
  match Bencode.Decoder.query Metainfo.decoder t with
  | Some t -> t
  | None ->
      failwith (Printf.sprintf "Could not parse metainfo file %S" path)

let random_string state n =
  String.init n (fun _ -> char_of_int (Random.State.int state 256))

let anon ~env ~rng path =
  let t = read_torrent_file path in
  let info_hash = t.Metainfo.info_hash in
  let peer_id = random_string rng 20 in
  let port = Random.State.int rng 10000 in
  let left = Metainfo.total_length t in
  let resp =
    Tracker.announce ~net:(Eio.Stdenv.net env) ~info_hash ~peer_id ~port
      ~uploaded:0 ~downloaded:0 ~left t.Metainfo.announce
  in
  let sexp = Tracker.Response.to_sexp resp in
  Format.printf "@[%a@]@." Sexp.print sexp

let anon path =
  let rng = Random.State.make_self_init () in
  Eio_main.run (fun env -> anon ~env ~rng path)

let () =
  Arg.parse (Arg.align spec) anon ""
