let verbose = ref true
let time_stamp = ref false
    
let start = ref (Unix.gettimeofday ())

let init () =
  start := Unix.gettimeofday ()

let print_time oc =
  if !verbose && !time_stamp then
    let t = Unix.gettimeofday () -. !start in
    (* let t0 = floor t in *)
    (* let t1 = t -. t0 in *)
    (* let tm = Unix.gmtime t0 in *)
    (* Printf.eprintf "%02d:%02d:%02d%#.2f " tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec t1 *)
    Printf.eprintf "%.3f " t

let infof ?exn fmt =
  Printf.ksprintf (fun msg ->
      if !verbose then begin
        print_time stderr;
        match exn with
        | None ->
          Printf.eprintf "* %s.\n%!" msg
        | Some exn ->
          Printf.eprintf "* %s (exn: %s).\n%!" msg (Printexc.to_string exn)
      end) fmt

(* let recvf src fmt = *)
(*   Printf.ksprintf (fun msg -> *)
(*       if !time_stamp then Printf.eprintf "[%.2f] " (Unix.gettimeofday () -. !start); *)
(*       Printf.eprintf "< %s < %s\n%!" msg src) fmt *)

let recv src data =
  print_time stderr;
  if !verbose then
    Printf.ksprintf prerr_endline "< %t < %t" data src

let sent dst data =
  print_time stderr;
  if !verbose then
    Printf.ksprintf prerr_endline "> %t > %t" data dst  

(* let sentf dst fmt = *)
(*   Printf.ksprintf (fun msg -> *)
(*       if !time_stamp then Printf.eprintf "[%.2f] " (Unix.gettimeofday () -. !start); *)
(*       Printf.eprintf "> %s > %s\n%!" msg dst) fmt *)
