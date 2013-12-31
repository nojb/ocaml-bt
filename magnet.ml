type t = {
  xt : Word160.t;
  dn : string option;
  tr : Uri.t list
}

let string_split (str : string) (delim : char) : string list =
  let rec loop start len =
    if start+len >= String.length str then
      [String.sub str start len]
    else if str.[start+len] = delim then
      String.sub str start len :: loop (start+len+1) 0
    else
      loop start (len+1)
  in
  loop 0 0

let match_prefix s p =
  (* Printf.eprintf "match_prefix: %S %S\n%!" str prefix; *)
  let sl = String.length s in
  let pl = String.length p in
  if sl < pl then
    raise Not_found
  else if String.sub s 0 pl <> p then
    raise Not_found
  else
    String.sub s pl (sl-pl)

let split_two delim s =
  let i = String.index s delim in
  String.sub s 0 i, String.sub s (i+1) ((String.length s)-i-1)

let _of_string s =
  let s = match_prefix s "magnet:?" in
  let comps = string_split s '&' |> List.map (split_two '=') in
  let rec loop xt dn tr = function
    | [] ->
      begin match xt with
        | None -> failwith "Magnet._of_string: no 'xt' component"
        | Some xt -> { xt; dn; tr = List.rev tr }
      end
    | ("xt", xt) :: rest ->
      let xt =
        try
          match_prefix xt "urn:btih:" |> Word160.of_hex
        with
        | Not_found ->
          match_prefix xt "urn:sha1:" |> Word160.of_base32
      in
      loop (Some xt) dn tr rest
    | ("dn", dn) :: rest ->
      loop xt (Some dn) tr rest
    | ("tr", uri) :: rest ->
      loop xt dn (Uri.of_string (Uri.pct_decode uri) :: tr) rest
    | _ :: rest ->
      (* Printf.eprintf "ignoring %S\n%!" n; *)
      loop xt dn tr rest
      (* invalid_arg "Magnet.of_string" *)
  in
  loop None None [] comps

let of_string s =
  try _of_string s
  with _ -> invalid_arg "Magnet.of_string"
