(* The MIT License (MIT)

   Copyright (c) 2016 Nicolas Ojeda Bar <n.oje.bar@gmail.com>

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

let first_n k l =
  let rec loop i l =
    match i >= k, l with
    | false, []
    | true, _ -> []
    | false, a :: l -> a :: loop (i+1) l
  in
  loop 0 l

module Routing : sig
  val distance: string -> string -> Big_int.big_int

  type 'a node =
    {
      id: string;
      data: 'a;
    }

  type 'a t

  val create: ?k:int -> string -> 'a t
  val add: 'a t -> string -> 'a -> unit
  val remove: 'a t -> string -> unit
  val find: 'a t -> ?k:int -> string -> 'a node list
end = struct
  type 'a node =
    {
      id: string;
      data: 'a;
    }

  type 'a tree =
    | Bucket of Big_int.big_int * 'a node list * Big_int.big_int
    | Node of 'a tree * 'a tree

  type 'a t =
    {
      k: int;
      id: Big_int.big_int;
      mutable tree: 'a tree;
    }

  let debug = ref true

  let big_int_of_id id =
    let res = ref Big_int.zero_big_int in
    for i = 0 to 19 do
      res := Big_int.add_int_big_int (Char.code id.[i]) (Big_int.mult_int_big_int 256 !res)
    done;
    !res

  let distance id1 id2 =
    Big_int.xor_big_int (big_int_of_id id1) (big_int_of_id id2)

  let create ?(k = 8) id =
    assert (String.length id = 20);
    let id = big_int_of_id id in
    {
      k;
      id;
      tree = Bucket (Big_int.zero_big_int, [], Big_int.power_int_positive_int 2 160);
    }

  let split min bucket max =
    if !debug then Printf.eprintf "split: %s %s\n%!" (Big_int.string_of_big_int min) (Big_int.string_of_big_int max);
    let mid = Big_int.div_big_int (Big_int.add_big_int min max) (Big_int.big_int_of_int 2) in
    let left, right = List.partition (fun {id; data = _} -> Big_int.compare_big_int (big_int_of_id id) mid <= 0) bucket in
    Bucket (min, left, mid), Bucket (mid, right, max)

  let nth id i =
    Big_int.compare_big_int
      (Big_int.and_big_int id (Big_int.power_int_positive_int 2 (160-i-1))) Big_int.zero_big_int != 0

  let between n1 n2 n3 =
    Big_int.compare_big_int n1 n2 <= 0 && Big_int.compare_big_int n2 n3 < 0

  let add table id data =
    let bi = big_int_of_id id in
    let rec add tree i =
      if !debug then Printf.eprintf "add: %s: %i\n%!" (Big_int.string_of_big_int bi) i;
      match tree with
      | Bucket (min, bucket, max) ->
          let bucket = List.filter (fun {id = id'; data = _} -> id <> id') bucket in
          if List.length bucket < table.k then
            Bucket (min, {id; data} :: bucket, max)
          else if between min table.id max || i mod 5 != 0 then
            let left, right = split min bucket max in
            if nth bi i then
              Node (left, add right (i+1))
            else
              Node (add left (i+1), right)
          else begin
            if !debug then
              Printf.eprintf "add: %s: %i: bucket %s %s full\n%!"
                (Big_int.string_of_big_int bi) i (Big_int.string_of_big_int min) (Big_int.string_of_big_int max);
            tree
          end
      | Node (left, right) ->
          if nth bi i then
            Node (left, add right (i+1))
          else
            Node (add left (i+1), right)
    in
    table.tree <- add table.tree 0

  let remove table id =
    let bi = big_int_of_id id in
    let rec remove tree i =
      match tree with
      | Bucket (min, bucket, max) ->
          let bucket = List.filter (fun (node : _ node) -> node.id <> id) bucket in
          Bucket (min, bucket, max)
      | Node (left, right) ->
          if nth bi i then
            Node (left, remove right (i+1))
          else
            Node (remove left (i+1), right)
    in
    table.tree <- remove table.tree 0

  let find table ?(k = table.k) id =
    let id = big_int_of_id id in
    let rec find k tree i =
      if k = 0 then []
      else begin
        match tree with
        | Bucket (_, bucket, _) ->
            let bucket =
              List.sort (fun (node1 : _ node) node2 ->
                Big_int.compare_big_int
                  (Big_int.xor_big_int (big_int_of_id node1.id) id) (Big_int.xor_big_int (big_int_of_id node2.id) id)
              ) bucket
            in
            let found = first_n k bucket in
            if !debug then begin
              Printf.eprintf "find: %s: %d: found %d nodes\n%!" (Big_int.string_of_big_int id) k (List.length found);
              List.iter (fun {id; data = _} -> Printf.eprintf "\t%s\n%!" (Big_int.string_of_big_int (big_int_of_id id))) found
            end;
            found
        | Node (left, right) ->
            if nth id i then
              let r = find k right (i+1) in
              let l = find (k - List.length r) left (i+1) in
              r @ l
            else
              let l = find k left (i+1) in
              let r = find (k - List.length l) right (i+1) in
              l @ r
      end
    in
    find k table.tree 0
end

let random_id () =
  let bytes = Bytes.create 20 in
  for i = 0 to 19 do
    Bytes.set bytes i (Char.chr (Random.int 256))
  done;
  Bytes.to_string bytes

(* let test () = *)
(*   let my_id = random_id () in *)
(*   let tbl = Routing.create my_id in *)
(*   for i = 1 to 10 do *)
(*     let id = random_id () in *)
(*     Routing.add tbl id i *)
(*   done; *)
(*   Routing.find tbl my_id *)

(* let () = *)
(*   ignore (test ()) *)

module Bcode = struct
  type t =
    | Int of int64
    | String of string
    | List of t list
    | Dict of (string * t) list

  let rec pp fmt = function
    | Int n -> Format.pp_print_string fmt (Int64.to_string n)
    | String s -> Format.fprintf fmt "%S" s
    | List l ->
        let first = ref true in
        let aux fmt l =
          List.iter (fun x ->
            if !first then first := false else Format.fprintf fmt ",@ ";
            pp fmt x
          ) l
        in
        Format.fprintf fmt "@[<hv 2>[@,%a@;<0 -2>]@]" aux l
    | Dict d ->
        let first = ref true in
        let aux fmt l =
          List.iter (fun (k, v) ->
            if !first then first := false else Format.fprintf fmt ",@ ";
            Format.fprintf fmt "@[<hov 2>%s:@ %a@]" k pp v
          ) l
        in
        Format.fprintf fmt "@[<hv 2>{@,%a@;<0 -2>}@]" aux d

  let find name = function
    | Dict d when List.mem_assoc name d ->
        List.assoc name d
    | _ ->
        invalid_arg (Printf.sprintf "Bcode.find: key %S not found" name)

  let to_list = function
    | List l ->
        l
    | _ ->
        invalid_arg "Bcode.to_list"

  let to_int64 = function
    | Int n -> n
    | _ -> invalid_arg "Bcode.to_int64"

  let to_int = function
    | Int n ->
        Int64.to_int n
    | _ ->
        invalid_arg "Bcode.to_int"

  let to_string = function
    | String s ->
        s
    | _ ->
        invalid_arg "Bcode.to_string"

  let to_cstruct = function
    | String cs ->
        cs
    | _ ->
        invalid_arg "Bcode.to_cstruct"

  let to_dict = function
    | Dict d ->
        d
    | _ ->
        invalid_arg "Bcode.to_dict"

  let bdecode s =
    let rec loop i =
      match s.[i] with
      | 'i' ->
          let start = i + 1 in
          let rec loop' i =
            assert (i < String.length s);
            match s.[i] with
            | 'e' ->
                Int (Int64.of_string (String.sub s start (i - start))), (i + 1)
            | '0' .. '9' ->
                loop' (i + 1)
            | c ->
                Printf.ksprintf failwith "Bcode.decode_partial: bad digit %C" c
          in
          loop' start
      | 'l' ->
          let rec loop' acc i =
            assert (i < String.length s);
            match s.[i] with
            | 'e' ->
                List (List.rev acc), (i + 1)
            | _ ->
                let x, i = loop i in
                loop' (x :: acc) i
          in
          loop' [] (i + 1)
      | '0' .. '9' ->
          let start = i in
          let rec loop' i =
            assert (i < String.length s);
            match s.[i] with
            | '0' .. '9' ->
                loop' (i + 1)
            | ':' ->
                let n = int_of_string (String.sub s start (i - start)) in
                String (String.sub s (i + 1) n), (i + n + 1)
            | _ ->
                failwith "Bcode.decode_partial: bad string"
          in
          loop' start
      | 'd' ->
          let rec loop' acc i =
            assert (i < String.length s);
            match s.[i] with
            | 'e' ->
                Dict (List.rev acc), (i + 1)
            | _ ->
                begin match loop i with
                | String k, i ->
                    let v, i = loop i in
                    loop' ((k, v) :: acc) i
                | _ ->
                    failwith "Bcode.decode_partial: bad dict"
                end
          in
          loop' [] (i + 1)
      | _ ->
          failwith "Bcode.decode_partial: bad"
    in
    let bc, _ = loop 0 in
    bc

  let bencode bc =
    let rec loop buf = function
      | Int n ->
          Printf.bprintf buf "i%Lde" n
      | String s ->
          Printf.bprintf buf "%d:%s" (String.length s) s
      | List l ->
          let aux buf l = List.iter (loop buf) l in
          Printf.bprintf buf "l%ae" aux l
      | Dict d ->
          let aux buf l = List.iter (fun (k, v) -> Printf.bprintf buf "%d:%s%a" (String.length k) k loop v) l in
          Printf.bprintf buf "d%ae" aux d
    in
    let buf = Buffer.create 0 in
    loop buf bc;
    Buffer.contents buf
end

open Lwt.Infix

module Rpc : sig
  type t

  val create: string -> t

  val ping: t -> Unix.sockaddr -> string Lwt.t
  val find_node: t -> Unix.sockaddr -> string -> (string * Unix.sockaddr) list Lwt.t
  val get_peers: t -> Unix.sockaddr -> string ->
    (string * [`Peers of Unix.sockaddr list | `Nodes of (string * Unix.sockaddr) list]) Lwt.t
  val announce_peer: t -> Unix.sockaddr -> string -> int -> string -> unit Lwt.t
end = struct
  type t =
    {
      id: string;
      fd: Lwt_unix.file_descr;
      in_progress: (string, (string * Bcode.t) list Lwt.u) Hashtbl.t;
      mutable t: int;
    }

  open Bcode

  exception Error of int * string

  let get_response bc =
    match find "t" bc |> to_string with
    | t ->
        let resp =
          match find "y" bc with
          | String "r" ->
              begin match find "r" bc with
              | exception Not_found -> `Error (203, "")
              | Dict dict -> `Ok dict
              | _ -> `Error (203, "")
              end
          | String "e" ->
              begin match find "e" bc with
              | List [Int n; String s] ->
                  `Error (Int64.to_int n, s)
              | _ ->
                  `Error (203, "")
              end
          | _ ->
              `Error (203, "")
        in
        Some (t, resp)
    | exception _ ->
        None

  let string_of_sockaddr = function
    | Unix.ADDR_UNIX s -> s
    | Unix.ADDR_INET (ip, port) ->
        Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port

  let create id =
    let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0 in
    let in_progress = Hashtbl.create 0 in
    let buf = Bytes.create 4096 in
    let rec loop () =
      Lwt_unix.recvfrom fd buf 0 (Bytes.length buf) [] >>= fun (n, addr) ->
      let bc = bdecode buf in
      Format.eprintf "@[<v 2>Received Response from %s:@,%a@]@." (string_of_sockaddr addr) Bcode.pp bc;
      match get_response bc with
      | None ->
          loop ()
      | Some (t, `Error (code, s)) ->
          let u = Hashtbl.find in_progress t in
          Hashtbl.remove in_progress t;
          Lwt.wakeup_later_exn u (Error (code, s));
          loop ()
      | Some (t, `Ok dict) ->
          let u = Hashtbl.find in_progress t in
          Hashtbl.remove in_progress t;
          Lwt.wakeup_later u dict;
          loop ()
    in
    Lwt.ignore_result (loop ());
    {id; fd; in_progress; t = 0}

  let query rpc addr q a =
    let t =
      let t = rpc.t in
      rpc.t <- rpc.t + 1;
      string_of_int t
    in
    let dict =
      [
        "t", String t;
        "y", String "q";
        "q", String q;
        "a", Dict (("id", String rpc.id) :: List.map (fun (k, v) -> k, String v) a);
      ]
    in
    let s = bencode (Dict dict) in
    let w, u = Lwt.wait () in
    Hashtbl.add rpc.in_progress t u;
    Lwt.catch (fun () ->
      Lwt_unix.sendto rpc.fd (Bytes.of_string s) 0 (String.length s) [] addr >>= fun n ->
      assert (n = String.length s);
      Format.eprintf "@[<v 2>Sent Query to %s:@,%a@]@." (string_of_sockaddr addr) Bcode.pp (Dict dict);
      w
    ) (fun e -> Hashtbl.remove rpc.in_progress t; Lwt.fail e)

  let ping rpc addr =
    query rpc addr "ping" [] >|= List.assoc "id" >|= to_string

  let get_nodes s =
    let rec loop i =
      if i + 26 > String.length s then
        []
      else
        let id = String.sub s i 20 in
        let ip = Obj.magic (String.sub s (i+20) 4) in
        let port =
          let b1 = s.[i+24] in
          let b2 = s.[i+25] in
          Char.code b1 lsl 8 + Char.code b2
        in
        (id, Unix.ADDR_INET (ip, port)) :: loop (i+26)
    in
    loop 0

  let get_peers s =
    let rec loop i =
      if i + 6 > String.length s then
        []
      else
        let ip = Obj.magic (String.sub s i 4) in
        let port =
          let b1 = s.[i+4] in
          let b2 = s.[i+5] in
          Char.code b1 lsl 8 + Char.code b2
        in
        Unix.ADDR_INET (ip, port) :: loop (i+6)
    in
    loop 0

  let find_node rpc addr target =
    query rpc addr "find_node" ["target", target] >|= List.assoc "nodes" >|= to_string >|= get_nodes

  let get_peers rpc addr info_hash =
    query rpc addr "get_peers" ["info_hash", info_hash] >|= fun resp ->
    let token = List.assoc "token" resp |> to_string in
    match List.assoc "values" resp with
    | String values ->
        let peers = get_peers values in
        (token, `Peers peers)
    | _ ->
        failwith ""
    | exception Not_found ->
        let nodes = List.assoc "nodes" resp |> to_string in
        let nodes = get_nodes nodes in
        (token, `Nodes nodes)

  let announce_peer rpc addr info_hash port1 token =
    query rpc addr "announce_peers" ["info_hash", info_hash; "port", string_of_int port1; "token", token] >>= fun _ ->
    Lwt.return_unit
end

module Dht = struct
  type data =
    {
      addr: Unix.sockaddr;
    }

  type t =
    {
      id: string;
      table: data Routing.t;
      rpc: Rpc.t;
    }

  let create id =
    let table = Routing.create id in
    let rpc = Rpc.create id in
    {id; table; rpc}

  let find_node dht id addr target =
    Lwt.try_bind
      (fun () -> Rpc.find_node dht.rpc addr target)
      (fun resp ->
         Routing.add dht.table id {addr};
         Lwt.return resp
      )
      (function
        | Lwt_unix.Timeout as e ->
            Routing.remove dht.table id;
            Lwt.fail e
        | e ->
            Lwt.fail e
      )

  let lookup ?(k = 8) ?(alpha = 3) dht target =
    let queried = ref [] in
    let not_yet_queried (id, _) = List.for_all (fun (id', _) -> id <> id') !queried in
    let rec loop nodes =
      Lwt_list.iter_p (fun (id, addr) ->
        queried := (id, addr) :: !queried;
        find_node dht id addr target >>= fun nodes ->
        let nodes = List.filter not_yet_queried nodes in
        let nodes =
          match nodes with
          | [] ->
              let nodes = Routing.find dht.table ~k target in
              List.filter not_yet_queried (List.map (fun node -> node.Routing.id, node.Routing.data.addr) nodes)
          | _ :: _ ->
              let nodes =
                List.sort (fun (id1, _) (id2, _) ->
                  Big_int.compare_big_int (Routing.distance id1 id) (Routing.distance id2 id)
                ) nodes
              in
              first_n alpha nodes
        in
        loop nodes
      ) nodes
    in
    let nodes = Routing.find dht.table ~k:alpha target in
    loop (List.map (fun node -> node.Routing.id, node.Routing.data.addr) nodes) >>= fun () ->
    Lwt.return (Routing.find dht.table ~k target)
end

let getaddrbyname hname =
  Lwt_unix.gethostbyname hname >>= fun he ->
  Lwt.return he.Unix.h_addr_list.(0)

let _ =
  let selfid = random_id () in
  let dht = Dht.create selfid in
  let t =
    getaddrbyname "router.utorrent.com" >>= fun addr ->
    let addr = Unix.ADDR_INET (addr, 6881) in
    Rpc.ping dht.Dht.rpc addr >>= fun id ->
    Printf.eprintf "Pinged!\n%!";
    Routing.add dht.Dht.table id {Dht.addr};
    Dht.lookup dht selfid
  in
  Lwt_main.run t
