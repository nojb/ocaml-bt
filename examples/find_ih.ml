let num_target = 50

let main _ =
  if Array.length Sys.argv <> 2 then begin
    Printf.printf "Usage: %s <info-hash>\n%!" Sys.argv.(0);
    exit 2
  end;
  let ih = Word160.of_string Sys.argv.(1) in
  let strm, push = Lwt_stream.create () in
  let dht = Dht.create push in
  Dht.run dht;
  Printf.printf "=========================== DHT\n";
  Printf.printf "Note that there are many bad nodes that reply to anything you ask.\n";
  Printf.printf "Peers found:\n";
  let count = ref 0 in
  Lwt_stream.iter (fun (_, peers) ->
      List.iter (fun x ->
          incr count;
          Printf.printf "%d: %s" !count (Udp.to_string addr);
          if !count >= num_target then Dht.stop dht) peers) strm
  |> ignore;
  let rec loop () =
    Dht.request_peers dht ih;
    Lwt_unix.sleep 5. >>= loop
  in
  Lwt_main.run (loop ())

let _ =
  main ()
