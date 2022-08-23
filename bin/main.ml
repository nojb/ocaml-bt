open Bt

let spec = []

let anon path =
  let s = In_channel.with_open_bin path In_channel.input_all in
  match Bencode.decode s with
  | Some t -> (
      match Bencode.Decoder.query Metainfo.decoder t with
      | Some t ->
          let sexp = Metainfo.to_sexp t in
          Format.printf "@[%a@]@." Sexp.print sexp
      | None ->
          failwith (Printf.sprintf "Could not parse metainfo file %S" path))
  | None -> failwith (Printf.sprintf "Could not parse torrent file %S" path)

let () = Arg.parse (Arg.align spec) anon ""
