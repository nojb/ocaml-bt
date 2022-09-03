type t = Atom of string | List of t list

module Encoder = struct
  type sexp = t
  type 'a t = 'a -> sexp

  let int n = Atom (string_of_int n)
  let string s = Atom s
  let list t l = List (List.map t l)
  let pair t u (x, y) = List [ t x; u y ]
  let array t a = List (List.map t (Array.to_list a))
  let record l = List (List.map (fun (k, v) -> List [ string k; v ]) l)
  let variant s ts = List (string s :: ts)
  let run t x = t x
end

let is_ascii s =
  String.for_all
    (fun c ->
      let c = Char.code c in
      32 <= c && c <= 127)
    s

let max_blob_print_length = 20

let rec print ppf = function
  | Atom s ->
      if is_ascii s then Format.pp_print_string ppf s
      else
        let s =
          if String.length s > max_blob_print_length then
            String.sub s 0 (max_blob_print_length - 2) ^ ".."
          else s
        in
        Format.fprintf ppf "%S" s
  | List l ->
      Format.fprintf ppf "@[<1>(%a)@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space print)
        l
