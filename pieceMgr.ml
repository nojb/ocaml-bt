open Messages

module BlockSet = Set.Make (struct type t = block let compare = compare end)
module IntMap = Map.Make (struct type t = int let compare = compare end)
module DownloadSet = Set.Make (struct type t = int * block let compare =
  compare end)

type piece_status =
  | Pending
  | Done
  | InProgress of int * BlockSet.t * block list

type state = {
  pieces : piece_status IntMap.t;
  downloading : DownloadSet.t;
  info : piece_info array
}
