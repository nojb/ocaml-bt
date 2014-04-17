(* The MIT License (MIT)

   Copyright (c) 2014 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
   FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
   COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
   IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)

type t = {
  xt : SHA1.t;
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
          match_prefix xt "urn:btih:" |> SHA1.of_hex
        with
        | Not_found ->
          match_prefix xt "urn:sha1:" |> SHA1.of_base32
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
