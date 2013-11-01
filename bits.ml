type t =
  string

external get_byte: string -> int -> int = "%string_unsafe_get"
external set_byte: string -> int -> int -> unit = "%string_unsafe_set"

let zero len =
  String.make len '\000'

let length v =
  String.length v

let equal v1 v2 =
  let len = String.length v1 in
  if len <> String.length v2 then false
  else
    let rec loop i =
      if i >= len then true
      else
        let b1 = get_byte v1 i <> 0 in
        let b2 = get_byte v2 i <> 0 in
        if b1 && not b2 || not b1 && b2 then false
        else loop (i+1)
    in loop 0

let check_args name i len =
  if i < 0 || i >= len then invalid_arg name

let set i v =
  check_args "Bits.set" i (String.length v);
  let v' = String.copy v in
  set_byte v' i 1;
  v'

let unset i v =
  check_args "Bits.unset" i (String.length v);
  let v' = String.copy v in
  set_byte v' i 0;
  v'

let toggle i v =
  check_args "Bits.toggle" i (String.length v);
  let v' = String.copy v in
  set_byte v' i (lnot (get_byte v' i));
  v'

let is_set i v =
  check_args "Bits.is_set" i (String.length v);
  get_byte v i <> 0

let lognot v =
  let v' = String.copy v in
  for i = 0 to (String.length v)-1 do
    set_byte v' i (lnot (get_byte v' i))
  done;
  v'

let logand v1 v2 =
  let n = String.length v1 in
  if n <> String.length v2 then invalid_arg "Bits.logand";
  let s = String.create n in
  for i = 0 to n-1 do
    set_byte s i ((get_byte v1 i) land (get_byte v2 i))
  done;
  s

let logor v1 v2 =
  let n = String.length v1 in
  if n <> String.length v2 then invalid_arg "Bits.logor";
  let s = String.create n in
  for i = 0 to n-1 do
    set_byte s i ((get_byte v1 i) lor (get_byte v2 i))
  done;
  s

let iter f v =
  let n = String.length v in
  for i = 0 to n-1 do
    f (get_byte v i <> 0)
  done

let map f v =
  let n = String.length v in
  let s = String.create n in
  for i = 0 to n-1 do
    set_byte s i (if f (get_byte s i <> 0) then 1 else 0)
  done;
  s

let to_string v =
  let n = String.length v in
  let s = String.create n in
  for i = 0 to n-1 do
    String.unsafe_set s i (if get_byte v i <> 0 then '1' else '0')
  done;
  s

let of_bin b =
  let len = String.length b in
  let b' = String.create (len*8) in
  for i = 0 to len-1 do
    let n = get_byte b i in
    for j = 0 to 7 do
      set_byte b' (i*8 + j) ((n lsr (7-j)) land 1)
    done
  done;
  b'

let to_bin v =
  let len = String.length v in
  if len mod 8 <> 0 then invalid_arg "Bits.to_bin";
  let n = len/8 in
  (* let n = int_of_float (ceil (float len /. 8.0)) in *)
  let b = String.create n in
  (* let rec loop acc i j = *)
  (*   if i >= len then b *)
  (*   else *)
  (*     if j < 0 then *)
  (*       (set_byte b (i/8) acc; loop 0 (i+1) 7) *)
  (*     else *)
  (*       if get_byte v i <> 0 then *)
  (*         loop (acc lor (1 lsl j)) (i+1) (j-1) *)
  (*       else *)
  (*         loop acc (i+1) (j-1) *)
  (* in loop 0 0 7 *)
  for i = 0 to n-1 do
    let rec loop acc j =
      if j >= 8 then
        set_byte b i acc
      else
        if get_byte v (i*8 + j) <> 0 then
          loop (acc lor (1 lsl (7-j))) (j+1)
        else
          loop acc (j+1)
    in loop 0 0
  done;
  b

let pad n v =
  let len = String.length v in
  let r = len mod n in
  let len' = len / n + (if r <> 0 then 1 else 0) in
  let v' = String.create (len'*n) in
  String.blit v 0 v' 0 len;
  v'

let arbitrary len =
  let int_multiples m n =
    QCheck.Arbitrary.(map (map n (fun x -> x / m)) (fun x -> x * m))
  in
  QCheck.Arbitrary.(string_len (int_multiples 8 len))

let _ =
  let test = QCheck.mk_test ~n:100 ~name:"Binary encoding/decoding"
    ~pp:to_string
    (arbitrary QCheck.Arbitrary.(int_range ~start:0 ~stop:100))
    (fun b -> equal (of_bin (to_bin b)) b && equal (to_bin (of_bin b)) b) in
  QCheck.run test

(* type t = { *)
(*   data : string; *)
(*   len : int *)
(* } *)
(*  *)
(* external get_byte: string -> int -> int = "%string_safe_get" *)
(* external set_byte: string -> int -> int -> unit = "%string_safe_set" *)
(*  *)
(* let zero n = *)
(*   if n < 0 then invalid_arg "Bits.zero"; *)
(*   let slen = int_of_float (ceil ((float n) /. 8.0)) in *)
(*   { data = String.make slen '\000'; len = n } *)
(*  *)
(* let length v = *)
(*   v.len *)
(*  *)
(* let check_args name i len = *)
(*   if i >= len || i < 0 then invalid_arg name; *)
(*   (i / 8, i mod 8) *)
(*  *)
(* let set i v = *)
(*   let j, k = check_args "Bits.set" i v.len in *)
(*   let s = String.copy v.data in *)
(*   set_byte s j ((get_byte s j) lor (1 lsl k)); *)
(*   { v with data = s } *)
(*  *)
(* let unset i v = *)
(*   let j, k = check_args "Bits.unset" i v.len in *)
(*   let s = String.copy v.data in *)
(*   set_byte s j ((get_byte s j) land (0 lsl k)); *)
(*   { v with data = s } *)
(*  *)
(* let toggle i v = *)
(*   let j, k = check_args "Bits.toggle" i v.len in *)
(*   let s = String.copy v.data in *)
(*   set_byte s j ((get_byte s j) lxor (1 lsl k)); *)
(*   { v with data = s } *)
(*  *)
(* let is_set i v = *)
(*   let j, k = check_args "Bits.is_set" i v.len in *)
(*   (get_byte v.data j) land (1 lsl k) <> 0 *)
(*  *)
(* let lognot v = *)
(*   let s = String.copy v.data in *)
(*   for i = 0 to (String.length s)-1 do *)
(*     set_byte s i (lnot (get_byte s i)) *)
(*   done; *)
(*   { v with data = s } *)
(*  *)
(* let to_string v = *)
(*   let *)
