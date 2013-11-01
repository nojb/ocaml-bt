
(** Combinator parser *)

type 'a result =
  | Success of 'a * int
  | Fail of int

type 'a p =
  string -> int -> 'a result

(* type 'a parser = string -> int -> 'a result *)

exception Parsing_error of int

let run_parser ?full:(full=true) s ?start:(i=0) (p : 'a p) : 'a =
  match p s i with
  | Success (x, j) ->
      if full && j < String.length s then raise (Parsing_error j) else x
  | Fail j -> raise (Parsing_error j)

let return x =
  fun _ i -> Success (x, i)

let bind p f =
  fun s i ->
    match p s i with | Fail j -> Fail j | Success (x, j) -> (f x) s j

let fail =
  fun _ i -> Fail i

let alt p q =
  fun s i ->
    match p s i with | Fail _ -> q s i | Success _ as res -> res

let (>>=) p f = bind p f
let (>>) p q = bind p (fun _ -> q)
let (>|=) p f = bind p (fun x -> return (f x))
let (>|) p x = bind p (fun _ -> return x)
let (<|>) p q = alt p q

let sat pred p =
  fun s i ->
    match p s i with
    | Success (x, j) -> if pred x then Success (x, j) else Fail i
    | Fail j -> Fail j

let any_char =
  fun s i ->
    if i >= String.length s then Fail i
    else Success (s.[i], i+1)

let char c =
  sat (fun ch -> ch = c) any_char

(* parses all the characters satisfying [pred] into
 * a string *)
let chars1_pred (pred : char -> bool) =
  fun s i ->
    let rec loop j =
      if j >= String.length s then
        if j > i then Success (String.sub s i (j-i), j)
        else Fail i
      else
        if pred s.[j] then loop (j+1)
        else
          if j > i then Success (String.sub s i (j-i), j)
          else Fail i
    in loop i

let digits =
  chars1_pred (fun ch -> '0' <= ch && ch <= '9')

let rec many p =
  many1 p <|> return []

and many1 p =
  p >>= fun x -> many p >|= fun xs -> x :: xs

let any_int =
  digits >|= int_of_string

let any_int64 =
  digits >|= Int64.of_string

let wrapped p1 p p2 =
  p1 >> p >>= fun res -> p2 >| res

let string_of_length len =
  fun s i ->
    if i+len > String.length s then Fail i
    else Success (String.sub s i len, i+len)

let pair p1 p2 =
  p1 >>= fun x1 -> p2 >|= fun x2 -> (x1, x2)

let fix f =
  fun s i ->
    (f ()) s i
