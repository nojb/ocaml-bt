type t = { xt : SHA1.t; dn : string option; tr : Uri.t list }

let string_split (str : string) (delim : char) : string list =
  let rec loop start len =
    if start + len >= String.length str then [ String.sub str start len ]
    else if str.[start + len] = delim then
      String.sub str start len :: loop (start + len + 1) 0
    else loop start (len + 1)
  in
  loop 0 0

let split_two delim s =
  let i = String.index s delim in
  (String.sub s 0 i, String.sub s (i + 1) (String.length s - i - 1))

let parse s =
  let s = Scanf.sscanf s "magnet:?%s" (fun s -> s) in
  let comps = string_split s '&' |> List.map (split_two '=') in
  let rec loop xt dn tr = function
    | [] -> (
        match xt with
        | None -> failwith "Magnet.parse: no 'xt' component"
        | Some xt -> { xt; dn; tr = List.rev tr })
    | ("xt", xt) :: rest ->
        let xt =
          try Scanf.sscanf xt "urn:btih:%s" SHA1.of_hex
          with _ -> Scanf.sscanf xt "urn:sah1:%S" SHA1.of_base32
        in
        loop (Some xt) dn tr rest
    | ("dn", dn) :: rest -> loop xt (Some dn) tr rest
    | ("tr", uri) :: rest ->
        loop xt dn (Uri.of_string (Uri.pct_decode uri) :: tr) rest
    | _ :: rest ->
        (* Printf.eprintf "ignoring %S\n%!" n; *)
        loop xt dn tr rest
  in
  loop None None [] comps

let parse s =
  Log.debug (fun m -> m "parsing %S" s);
  try
    let m = parse s in
    Log.debug (fun f -> f "  xt = %s" (SHA1.sprint_hex () m.xt));
    (match m.dn with
    | Some dn -> Log.debug (fun m -> m "  dn = %S" dn)
    | None -> ());
    List.iter
      (fun tr -> Log.debug (fun m -> m "  tr = %s" (Uri.to_string tr)))
      m.tr;
    `Ok m
  with e ->
    Log.err (fun m -> m "parsing failed: %S" (Printexc.to_string e));
    `Error
