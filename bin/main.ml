open Bt

let spec = []

let random state n =
  String.init n (fun _ -> char_of_int (Random.State.int state 256))

let anon ~env ~ran path =
  let s = In_channel.with_open_bin path In_channel.input_all in
  match Bencode.decode s with
  | Some t -> (
      match Bencode.Decoder.query Metainfo.decoder t with
      | Some t ->
          let info_hash = t.Metainfo.info_hash in
          let peer_id = random ran 20 in
          let port = Random.State.int ran 10000 in
          let left = Metainfo.total_length t in
          let resp =
            Tracker.announce ~net:(Eio.Stdenv.net env) ~info_hash ~peer_id ~port
              ~uploaded:0 ~downloaded:0 ~left t.Metainfo.announce
          in
          let sexp = Tracker.Response.to_sexp resp in
          Format.printf "@[%a@]@." Sexp.print sexp
      | None ->
          failwith (Printf.sprintf "Could not parse metainfo file %S" path))
  | None -> failwith (Printf.sprintf "Could not parse torrent file %S" path)

let () =
  Eio_main.run (fun env ->
      let ran = Random.State.make_self_init () in
      Arg.parse (Arg.align spec) (anon ~env ~ran) "")
