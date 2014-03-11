let bucket_count = 8
let minutes n = n * 60
let reannounce_interval = minutes 15

type time = float

type status =
  | Good
  | Bad
  | Unknown
  | Pinged

type node = {
  id : Word160.t;
  addr : Unix.sockaddr;
  mutable last : time;
  mutable status : status
}

type bucket = {
  lo : Word160.t;
  hi : Word160.t;
  mutable last_change : time;
  mutable nodes : node array
}

type tree =
  | L of bucket
  | N of tree * id * tree

type table = {
  mutable root : tree;
  self : Word160.t
}

let string_of_status = function
  | Good -> "Good"
  | Bad -> "Bad"
  | Unknown -> "Unknown"
  | Pinged -> "Pinged"

let make_node id addr status =
  { id; addr; last = Time.gettimeofday (); status }

let mark n status =
  n.last <- Unix.gettimeofday ();
  n.status <- status

let touch b =
  b.last_change <- Unix.gettimeofday ()

let rec update ping table status id data =
  let rec loop = function
    | N (l, mid, r) ->
