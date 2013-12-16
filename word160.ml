type t =
  string
    
let zero =
  String.make 20 '\000'
    
let compare s1 s2 =
  compare s1 s2
    
let equal s1 s2 =
  s1 = s2
  
let hash (s : t) =
  Hashtbl.hash s
    
let to_hex s =
  let buf = Buffer.create 40 in
  for i = 0 to 19 do
    Printf.bprintf buf "%02x" (int_of_char s.[i])
  done;
  Buffer.contents buf

let to_hex_short s =
  let s = to_hex s in
  String.sub s 0 6
  (* Printf.sprintf "%s..%s" (String.sub s 0 6) (String.sub s 15 4) *)
    
let pp fmt s =
  Format.fprintf fmt "%s" (to_hex s)

let print oc s =
  output_string oc s

let sprint () s =
  to_hex s
    
let to_bin x =
  x
    
let from_bin x =
  if String.length x <> 20 then invalid_arg "Word160.from_bin";
  x
      
let digest_of_string s =
  Sha1.to_bin (Sha1.string s)
    
let to_z s =
  Z.of_bits s

let dist s1 s2 : Z.t =
  Z.(logxor (of_bits s1) (of_bits s2))

let random () =
  let s = String.create 20 in
  for i = 0 to 19 do
    s.[i] <- Char.chr (Random.int 256)
  done;
  s

let peer_id prefix =
  if String.length prefix > 20 then invalid_arg "Word160.peer_id";
  let random_digit () =
    char_of_int ((Random.int 10) + (int_of_char '0'))
  in
  let random_string len =
    let s = String.create len in
    let rec loop i =
      if i >= len then s
      else begin
        s.[i] <- random_digit ();
        loop (i+1)
      end
    in
    loop 0
  in
  prefix ^ random_string (20 - String.length prefix)
