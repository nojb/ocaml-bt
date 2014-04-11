open Printf
module Hash = Word160

let alpha = 3
let (&) f x = f x
    
module B = Bcode

type dict =
  (string * B.t) list

let show_dict d = String.concat "," & List.map fst d

type msg =
  | Query of string * dict
  | Response of dict
  | Error of int64 * string

let show_msg = function
  | Query (name, args) -> sprintf "query %s(%s)" name (show_dict args)
  | Response d -> sprintf "response (%s)" (show_dict d)
  | Error (n, s) -> sprintf "error (%Ld,%S)" n s

let encode (txn, msg) =
  let x = match msg with
    | Query (name, args) ->
      ["y", B.BString "q"; "q", B.BString name; "a", B.BDict args]
    | Response dict ->
      ["y", B.BString "r"; "r", B.BDict dict]
    | Error (code, s) ->
      ["y", B.BString "e"; "e", B.BList [B.BInt code; B.BString s]]
  in
  let x = ("t", B.BString txn) :: x in
  Put.run (B.bencode (B.BDict x))

let decode_exn s =
  let bc = Get.run B.bdecode s in
  let txn = B.find "t" bc |> B.to_string in
  let msg = match B.find "y" bc |> B.to_string with
  | "q" -> Query (B.find "q" bc |> B.to_string, B.find "a" bc |> B.to_dict)
  | "r" -> Response (B.find "r" bc |> B.to_dict)
  | "e" ->
    begin match B.find "e" bc |> B.to_list with
    | B.BInt n :: B.BString s :: _ -> Error (n, s)
    | _ -> failwith "decode_exn"
    end
  | _ ->
    failwith "decode_exn"
  in
  (txn, msg)

module A = Assoc2

let send sock stats addr txnmsg =
  let s = encode txnmsg in
  stats_add stats `Sent 1;
  stats_add stats `SentBytes (String.length s);
  Udp.write sock s addr

type rpc_response =
  [ `Error
  | `Timeout
  | `Response of dict ]

type t =
    Udp.socket * (stats_key, int) Hashtbl.t * (addr, string, rpc_response Lwt.u) A.t

let create answer =
  let sock = Udp.create_socket () in
  let h = A.create () in
  let stats = Hashtbl.create 3 in
  let handle addr (txn, msg) =
    match msg with
    | Error (code, s) ->
      begin match A.find addr txn h with
        | None ->
          stats_add stats `NoTxn 1
        | Some w ->
          A.remove h addr txn;
          Lwt.wakeup w `Error
      end
    | Response ret ->
      begin match A.find addr txn with
      | None ->
        stats_add stats `NoTxn 1
      | Some w ->
        A.remove h addr txn;
        Lwt.wakeup w (`Ok ret)
      end
    | Query (name, args) ->
      let ret = answer addr name args in
      send sock stats addr (txn, ret)
  in
  let rec listen_loop sock =
    recv sock >>= fun (s, addr) ->

    let handle (s, addr) =
      try
        stats_add stats `RecvBytse (String.length s);
        stats_add stats `Recv 1;
        let r = decode_exn s in
        stats_add stats `Decoded `;
        ret := r;
        match r with
        | Error (code, msg) ->
          

  let handle p =
    match p.udp_addr with
    | Unix.ADDR_UNIX _ -> assert false
    | Unix.ADDR_INET (inet_addr,port) ->
      let addr = (Ip.of_inet_addr inet_addr, port) in
      let ret = ref None in
      try
        stats_add stats `RecvBytes (String.length p.udp_content);
        stats_add stats `Recv 1;
        let r = decode_exn p.udp_content in
        stats_add stats `Decoded 1;
        ret := Some r;
        handle addr r;
        stats_add stats `Handled 1;
      with exn ->
        let version = match !ret with Some (_,Some s,_) -> sprintf " client %S" s | _ -> "" in
        if !verb then lprintf_nl ~exn "handle packet from %s%s : %S" (show_addr addr) version p.udp_content;
        let error txn code str = send socket stats addr (txn,(Error (Int64.of_int code,str))) in
        match exn,!ret with
        | Malformed_packet x, Some (txn, _, _)
        | Protocol_error ("",x), Some(txn, _, _) | Protocol_error (txn,x), _ -> error txn 203 x
        | Method_unknown x, Some (txn, _, _) -> error txn 204 x
        | _, Some (txn, _, Query _) -> error txn 202 ""
        | _ -> ()


let handle addr (txn,ver,msg) =
    let version = lazy (match ver with Some s -> sprintf " client %S" s | None -> "") in
    if !debug then lprintf_nl "KRPC from %s %stxn %S : %s" (show_addr addr) !!version txn (show_msg msg);
    match msg with
    | Error (code,msg) ->
        if !verb then lprintf_nl "error received from %s%s : %Ld %S" (show_addr addr) !!version code msg;
        begin match A.find h addr txn with
        | None ->
          stats_add stats `NoTxn 1;
          if !verb then lprintf_nl "no txn %S for %s%s" txn (show_addr addr) !!version
        | Some (_, kerr, _) -> A.remove h addr txn; kerr `Error
        end
    | Query (name,args) ->
        let ret = answer addr name args in
        send socket stats addr (txn, ret)
    | Response ret ->
        match A.find h addr txn with
        | None ->
          stats_add stats `NoTxn 1;
          if !verb then lprintf_nl "no txn %S for %s%s" txn (show_addr addr) !!version
        | Some (k,_,_) -> A.remove h addr txn; k addr ret
  in
  let handle p =
    match p.udp_addr with
    | Unix.ADDR_UNIX _ -> assert false
    | Unix.ADDR_INET (inet_addr,port) ->
      let addr = (Ip.of_inet_addr inet_addr, port) in
      let ret = ref None in
      try
        stats_add stats `RecvBytes (String.length p.udp_content);
        stats_add stats `Recv 1;
        let r = decode_exn p.udp_content in
        stats_add stats `Decoded 1;
        ret := Some r;
        handle addr r;
        stats_add stats `Handled 1;
      with exn ->
        let version = match !ret with Some (_,Some s,_) -> sprintf " client %S" s | _ -> "" in
        if !verb then lprintf_nl ~exn "handle packet from %s%s : %S" (show_addr addr) version p.udp_content;
        let error txn code str = send socket stats addr (txn,(Error (Int64.of_int code,str))) in
        match exn,!ret with
        | Malformed_packet x, Some (txn, _, _)
        | Protocol_error ("",x), Some(txn, _, _) | Protocol_error (txn,x), _ -> error txn 203 x
        | Method_unknown x, Some (txn, _, _) -> error txn 204 x
        | _, Some (txn, _, Query _) -> error txn 202 ""
        | _ -> ()
    

let write ... = 0
