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

let debug = ref true

let first_n k l =
  let rec loop i l =
    match i >= k, l with
    | false, []
    | true, _ -> []
    | false, a :: l -> a :: loop (i+1) l
  in
  loop 0 l

let string_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (ip, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port

module Id : sig
  type t
  val make: string -> t
  val to_string: t -> string
  val random: unit -> t
  val hex: t -> string
  val hex_short: t -> string
  val output: out_channel -> t -> unit
  val to_z: t -> Z.t
  val distance: t -> t -> Z.t
end = struct
  type t = string

  let make s =
    assert (String.length s = 20);
    s

  let to_string id =
    id

  let random () =
    let b = Bytes.create 20 in
    for i = 0 to 19 do
      Bytes.set b i (Char.chr (Random.int 256))
    done;
    make (Bytes.unsafe_to_string b)

  let hex id =
    let b = Bytes.create 40 in
    let to_char n =
      if n < 10 then Char.chr (Char.code '0' + n)
      else Char.chr (Char.code 'a' + n - 10)
    in
    for i = 0 to 19 do
      let n = Char.code id.[i] in
      let h1 = (n lsr 4) land 0xf in
      let h2 = n land 0xf in
      let j = 2*i in
      Bytes.set b j (to_char h1);
      Bytes.set b (j+1) (to_char h2)
    done;
    Bytes.unsafe_to_string b

  let hex_short id =
    String.sub (hex id) 0 6

  let output oc id =
    Printf.fprintf oc "%s" (hex id)

  let to_z id =
    let res = ref Z.zero in
    for i = 0 to 19 do
      let c = Char.code id.[i] in
      res := Z.(~$c + !res lsl 8)
    done;
    !res

  let distance id1 id2 =
    Z.logxor (to_z id1) (to_z id2)
end

type id = Id.t

let sockaddr_of_network s i =
  assert (String.length s >= i+6);
  let a = Char.code s.[i+0] in
  let b = Char.code s.[i+1] in
  let c = Char.code s.[i+2] in
  let d = Char.code s.[i+3] in
  let e = Char.code s.[i+4] in
  let f = Char.code s.[i+5] in
  let ip = Unix.inet_addr_of_string (Printf.sprintf "%d.%d.%d.%d" a b c d) in
  let port = (e lsl 8) lor f in
  Unix.ADDR_INET (ip, port)

let get_nodes s =
  let rec loop i =
    if i + 26 > String.length s then
      []
    else
      let id = Id.make (String.sub s i 20) in
      let addr = sockaddr_of_network s (i+20) in
      (id, addr) :: loop (i+26)
  in
  loop 0

let get_peers s =
  let rec loop i =
    if i + 6 > String.length s then
      []
    else
      let addr = sockaddr_of_network s i in
      addr :: loop (i+6)
  in
  loop 0

module Routing : sig
  type 'a node =
    {
      id: id;
      data: 'a;
    }

  type 'a t

  val create: ?k:int -> id -> 'a t
  val add: 'a t -> id -> 'a -> unit
  val remove: 'a t -> id -> unit
  val find: 'a t -> ?k:int -> id -> 'a node list

  val number_of_nodes: 'a t -> int
end = struct
  type 'a node =
    {
      id: id;
      data: 'a;
    }

  type 'a tree =
    | Bucket of Z.t * 'a node list * Z.t
    | Node of 'a tree * 'a tree

  type 'a t =
    {
      k: int;
      id: id;
      mutable tree: 'a tree;
    }

  let create ?(k = 8) id =
    {
      k;
      id;
      tree = Bucket (Z.zero, [], Z.(~$1 lsl 160));
    }

  let split lz bucket rz =
    if !debug then
      Printf.eprintf "split: %a %a\n%!" Z.output lz Z.output rz;
    let mz = Z.((lz + rz) / ~$2) in
    let left, right =
      List.partition (fun {id; data = _} -> Id.to_z id <= mz) bucket
    in
    Bucket (lz, left, mz), Bucket (mz, right, rz)

  let nth id i =
    let i = 160-i-1 in
    Z.(Id.to_z id land (~$1 lsl i)) <> Z.zero

  let between n1 n2 n3 =
    n1 <= n2 && n2 < n3

  let add table id data =
    let rec add tree i =
      match tree with
      | Bucket (min, bucket, max) ->
          let bucket = List.filter (fun {id = id'; data = _} -> id <> id') bucket in
          if List.length bucket < table.k then
            Bucket (min, {id; data} :: bucket, max)
          else if between min (Id.to_z table.id) max || i mod 5 != 0 then
            let left, right = split min bucket max in
            if nth id i then
              Node (left, add right (i+1))
            else
              Node (add left (i+1), right)
          else begin
            if !debug then
              Printf.eprintf "add: %a: %i: bucket %a %a full\n%!"
                Id.output id i Z.output min Z.output max;
            tree
          end
      | Node (left, right) ->
          if nth id i then
            Node (left, add right (i+1))
          else
            Node (add left (i+1), right)
    in
    table.tree <- add table.tree 0

  let remove table id =
    let rec remove tree i =
      match tree with
      | Bucket (min, bucket, max) ->
          let bucket = List.filter (fun (node : _ node) -> node.id <> id) bucket in
          Bucket (min, bucket, max)
      | Node (left, right) ->
          if nth id i then
            Node (left, remove right (i+1))
          else
            Node (remove left (i+1), right)
    in
    table.tree <- remove table.tree 0

  let find table ?(k = table.k) id =
    let rec find k tree i =
      if k = 0 then []
      else begin
        match tree with
        | Bucket (_, bucket, _) ->
            let bucket =
              List.sort (fun (node1 : _ node) node2 ->
                Z.compare (Id.distance node1.id id) (Id.distance node2.id id)
              ) bucket
            in
            let found = first_n k bucket in
            if !debug then begin
              Printf.eprintf "find: %a: %d: found %d nodes\n%!" Id.output id k (List.length found);
              List.iter (fun {id; data = _} -> Printf.eprintf "\t%a\n%!" Id.output id) found
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

  let number_of_nodes table =
    let rec count = function
      | Bucket (_, bucket, _) ->
          List.length bucket
      | Node (left, right) ->
          count left + count right
    in
    count table.tree
end

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
                Printf.ksprintf
                  failwith "Bcode.decode_partial: bad string: %S"
                  (String.sub s start (String.length s - start))
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

  let bencode =
    let b = Buffer.create 0 in
    fun bc ->
      let rec loop b = function
        | Int n ->
            Printf.bprintf b "i%Lde" n
        | String s ->
            Printf.bprintf b "%d:%s" (String.length s) s
        | List l ->
            let aux b l = List.iter (loop b) l in
            Printf.bprintf b "l%ae" aux l
        | Dict d ->
            let aux b l = List.iter (fun (k, v) -> Printf.bprintf b "%d:%s%a" (String.length k) k loop v) l in
            Printf.bprintf b "d%ae" aux d
      in
      loop b bc;
      let s = Buffer.contents b in
      Buffer.clear b;
      s
end

open Lwt.Infix

module Dht = struct
  module Wire = struct
    type query =
      | Ping of id
      | Find_node of id * id
      | Get_peers of id * id
      | Announce_peer of id * id * int * string

    type message =
      | Query of query
      | Response of (string * Bcode.t) list
      | Error of int * string

    let decode bc =
      let open Bcode in
      let t = find "t" bc |> to_string in
      let msg =
        match find "y" bc |> to_string with
        | "r" ->
            Response (find "r" bc |> to_dict)
        | "e" ->
            let code, s =
              match find "e" bc |> to_list with
              | [Int n; String s] ->
                  Int64.to_int n, s
              | _ ->
                  failwith "Unexpected error format"
            in
            Error (code, s)
        | "q" ->
            let a = find "a" bc in
            let id = find "id" a |> to_string |> Id.make in
            let query =
              match find "q" bc |> to_string with
              | "ping" ->
                  Ping id
              | "find_node" ->
                  let target = find "target" a |> to_string |> Id.make in
                  Find_node (id, target)
              | "get_peers" ->
                  let info_hash = find "info_hash" a |> to_string |> Id.make in
                  Get_peers (id, info_hash)
              | "announce_peer" ->
                  let info_hash = find "info_hash" a |> to_string |> Id.make in
                  let port = find "port" a |> to_int in
                  let token = find "token" a |> to_string in
                  Announce_peer (id, info_hash, port, token)
              | s ->
                  Printf.ksprintf failwith "Unexpected query type: %s" s
            in
            Query query
        | s ->
            Printf.ksprintf failwith "Unexpected message type: %s" s
      in
      t, msg

    let encode t msg =
      let open Bcode in
      let dict =
        match msg with
        | Response dict ->
            ("y", String "r") :: dict
        | Query q ->
            let a =
              match q with
              | Ping id ->
                  ["id", String (Id.to_string id)]
              | Find_node (id, target) ->
                  ["id", String (Id.to_string id); "target", String (Id.to_string target)]
              | Get_peers (id, info_hash) ->
                  ["id", String (Id.to_string id); "info_hash", String (Id.to_string info_hash)]
              | Announce_peer (id, info_hash, port, token) ->
                  ["id", String (Id.to_string id); "info_hash", String (Id.to_string info_hash);
                   "port", Int (Int64.of_int port); "token", String token]
            in
            let q =
              match q with
              | Ping _ -> "ping"
              | Find_node _ -> "find_node"
              | Get_peers _ -> "get_peers"
              | Announce_peer _ -> "announce_peer"
            in
            ["y", String "q"; "q", String q; "a", Dict a]
        | Error (code, s) ->
            ["y", String "e"; "e", List [Int (Int64.of_int code); String s]]
      in
      let dict = ("t", String t) :: dict in
      Dict dict
  end

  type t =
    {
      id: id;
      table: Unix.sockaddr Routing.t;
      fd: Lwt_unix.file_descr;
      in_progress: (string, float * (string * Bcode.t) list Lwt.u) Hashtbl.t;
      mutable next_t: int;
    }

  let handle_query dht addr q =
    match q with
    | Wire.Ping _ ->
        prerr_endline "Received PING"
    | Find_node _ ->
        prerr_endline "Received FIND_NODE"
    | Get_peers _ ->
        prerr_endline "Received GET_PEERS"
    | Announce_peer _ ->
        prerr_endline "Received ANNOUNCE_PEER"

  let create id =
    let fd = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_DGRAM 0 in
    let table = Routing.create id in
    let in_progress = Hashtbl.create 0 in
    let buf = Bytes.create 4096 in
    let rec loop () =
      Lwt_unix.recvfrom fd buf 0 (Bytes.length buf) [] >>= fun (n, addr) ->
      let bc = Bcode.bdecode buf in
      Format.eprintf "@[<v 2>Received Response from %s:@,%a@]@."
        (string_of_sockaddr addr) Bcode.pp bc;
      begin match Wire.decode bc with
      | t, Error (code, s) ->
          begin match Hashtbl.find in_progress t with
          | (_, u) ->
              Hashtbl.remove in_progress t;
              Lwt.wakeup_later_exn u (Failure (Printf.sprintf "%d: %s" code s))
          | exception Not_found ->
              Printf.eprintf "Orphaned response\n%!"
          end
      | t, Response dict ->
          begin match Hashtbl.find in_progress t with
          | (_, u) ->
              Hashtbl.remove in_progress t;
              Lwt.wakeup_later u dict
          | exception Not_found ->
              Printf.eprintf "Orphaned response\n%!"
          end
      | t, Query q ->
          handle_query (Lazy.force dht) addr q
      | exception e ->
          Printf.eprintf "Error while decoding response: %s\n%!" (Printexc.to_string e)
      end;
      loop ()
    and tick () =
      let now = Sys.time () in
      Hashtbl.iter (fun t (limit, u) ->
        if limit >= now then begin
          Printf.eprintf "tick: timing out %S\n%!" t;
          Hashtbl.remove in_progress t;
          Lwt.wakeup_later_exn u Lwt_unix.Timeout
        end
      ) in_progress;
      Lwt_unix.sleep 1. >>= tick
    and dht =
      lazy begin
        Lwt.ignore_result (tick ());
        Lwt.ignore_result (loop ());
        {id; fd; table; in_progress; next_t = 0}
      end
    in
    Lazy.force dht

  let query rpc addr q =
    let t =
      let t = rpc.next_t in
      rpc.next_t <- rpc.next_t + 1;
      string_of_int t
    in
    let bc = Wire.encode t (Query q) in
    let s = Bcode.bencode bc in
    let w, u = Lwt.wait () in
    Hashtbl.add rpc.in_progress t (Sys.time () +. 5.0, u);
    Lwt.catch (fun () ->
      Lwt_unix.sendto rpc.fd (Bytes.of_string s) 0 (String.length s) [] addr >>= fun n ->
      assert (n = String.length s);
      Format.eprintf "@[<v 2>Sent Query to %s:@,%a@]@." (string_of_sockaddr addr) Bcode.pp bc;
      w
    ) (fun e -> Hashtbl.remove rpc.in_progress t; Lwt.fail e)

  let ping rpc addr =
    query rpc addr (Ping rpc.id) >|= List.assoc "id" >|= Bcode.to_string >|= Id.make

  let ping dht addr =
    Lwt.try_bind
      (fun () -> ping dht addr)
      (fun id ->
         Routing.add dht.table id addr;
         Lwt.return_unit
      )
      (fun _ -> Lwt.return_unit)

  let find_node rpc addr target =
    query rpc addr (Find_node (rpc.id, target)) >|= List.assoc "nodes" >|= Bcode.to_string >|= get_nodes

  let find_node dht id addr target =
    Lwt.try_bind
      (fun () -> find_node dht addr target)
      (fun resp ->
         Routing.add dht.table id addr;
         Lwt.return resp
      )
      (fun _ -> Routing.remove dht.table id; Lwt.return_nil)

  let get_peers rpc addr info_hash =
    let open Bcode in
    query rpc addr (Get_peers (rpc.id, info_hash)) >|= fun resp ->
    let token = List.assoc "token" resp |> to_string in
    match List.assoc "values" resp |> to_string with
    | values ->
        let peers = get_peers values in
        (token, `Peers peers)
    | exception Not_found ->
        let nodes = List.assoc "nodes" resp |> to_string in
        let nodes = get_nodes nodes in
        (token, `Nodes nodes)

  let announce_peer rpc addr info_hash port token =
    query rpc addr (Announce_peer (rpc.id, info_hash, port, token)) >>= fun _ ->
    Lwt.return_unit

  let lookup ?(k = 8) ?(alpha = 3) dht target =
    let queried = ref [] in
    let not_yet_queried (id, _) = not (List.mem_assoc id !queried) in
    let cmp (id1, _) (id2, _) = Z.compare (Id.distance id1 target) (Id.distance id2 target) in
    let rec loop nodes =
      let nodes = first_n alpha nodes in
      Printf.eprintf "loop: querying %d nodes (%d in table)\n%!"
        (List.length nodes) (Routing.number_of_nodes dht.table);
      List.iter (fun (id, _) -> Printf.eprintf "\t%a\n%!" Z.output (Id.distance id target)) nodes;
      List.iter (fun node -> queried := node :: !queried) nodes;
      let find_node (id, addr) =
        find_node dht id addr target >>= fun nodes ->
        Printf.eprintf "Received %d nodes from %s (%d not yet queried)\n%!"
          (List.length nodes) (string_of_sockaddr addr)
          (List.length (List.filter not_yet_queried nodes));
        Lwt.return nodes
      in
      Lwt_list.map_p find_node nodes >|=
      List.flatten >|=
      List.filter not_yet_queried >|=
      List.sort cmp >>= function
      | [] ->
          Printf.eprintf "loop: no more nodes returned; trying with %d closest ...\n%!" k;
          let nodes = Routing.find dht.table ~k target in
          prerr_endline "foo";
          let nodes = List.filter not_yet_queried (List.map (fun {Routing.id; data} -> id, data) nodes) in
          if nodes <> [] then loop nodes else Lwt.return_unit
      | _ :: _ as nodes ->
          loop nodes
    in
    let nodes = Routing.find dht.table ~k:alpha target in
    loop (List.map (fun {Routing.id; data} -> id, data) nodes) >|= fun () ->
    Routing.find dht.table ~k target
end

let getaddrbyname hname =
  Lwt_unix.gethostbyname hname >>= fun he ->
  Lwt.return he.Unix.h_addr_list.(0)

let _ =
  let selfid = Id.random () in
  let dht = Dht.create selfid in
  let t =
    getaddrbyname "router.utorrent.com" >>= fun addr ->
    let addr = Unix.ADDR_INET (addr, 6881) in
    Dht.ping dht addr >>= fun () ->
    Printf.eprintf "Pinged!\n%!";
    Dht.lookup dht selfid >|=
    List.iter (fun {Routing.id; _} ->
      Printf.eprintf "\t%a\n%!" Z.output (Id.distance id selfid)
    )
  in
  Lwt_main.run t
