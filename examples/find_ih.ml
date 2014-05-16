let (>>=) = Lwt.(>>=)
    
let num_target = 50
let dht_port = 4567
  
let main _ =
  if Array.length Sys.argv <> 2 then begin
    Printf.printf "Usage: %s <info-hash>\n%!" Sys.argv.(0);
    exit 2
  end;
  let ih = Bt.SHA1.of_hex Sys.argv.(1) in
  let dht = Bt.DHT.create dht_port in
  Bt.DHT.start dht;
  Printf.printf "=========================== DHT\n";
  Printf.printf "Note that there are many bad nodes that reply to anything you ask.\n";
  Printf.printf "Peers found:\n";
  let count = ref 0 in
  let handle_peers _ token peers =
    List.iter (fun addr ->
      incr count;
      Printf.printf "%d: %s\n%!" !count (Bt.Addr.to_string addr)
      (* if !count >= num_target then DHT.stop dht *)) peers
  in
  let rec loop () =
    Bt.DHT.query_peers dht ih handle_peers >>= fun () ->
    Lwt_unix.sleep 5.0 >>=
    loop
  in
  Lwt_main.run (Bt.DHT.auto_bootstrap dht Bt.DHT.bootstrap_nodes >>= loop)

let _ =
  main ()
