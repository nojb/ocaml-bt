let now () = Unix.gettimeofday ()
    
let start_time = ref (now ())

let reset_timer () = start_time := now ()

type color =
  | Red
  | Yellow
  | Green
  | None

let colorcode = function
  | Red -> "\027[31;1m"
  | Yellow -> "\027[33;1m"
  | Green -> "\027[32m"
  | None -> ""

let log title color ?sec ?exn fmt =
  let color = color in (* if !Sys.interactive then color else None in  *)
  Printf.ksprintf (fun msg ->
      Printf.eprintf "%s[%.2f] %s%s: %s%s\n%!\027[0m"
        (colorcode color)
        (now () -. !start_time)
        title
        (match sec with None -> "" | Some s -> "(" ^ s ^ ")")
        msg
        (match exn with None -> "" | Some exn -> ": " ^ (Printexc.to_string exn))) fmt
    
let error ?sec ?exn fmt = log "error" Red ?sec ?exn fmt

let info ?sec ?exn fmt = log "info" None ?sec ?exn fmt

let warning ?sec ?exn fmt = log "warning" Yellow ?sec ?exn fmt

let success ?sec ?exn fmt = log "success" Green ?sec ?exn fmt
    
