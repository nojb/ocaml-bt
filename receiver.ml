(* open Msg *)

let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let stream_of_channel id (ic : Lwt_io.input_channel) : Wire.msg Lwt_stream.t =
  Lwt_stream.from (fun () -> lwt msg = Wire.read ic in Lwt.return (Some msg))

let start ~super_ch ic ~peer_ch =
  let run id =
    (* let t = { send_peer; id } in *)
    Lwt_stream.iter (fun msg -> Lwt_pipe.write peer_ch (Msg.PeerMsg msg))
      (stream_of_channel id ic)
  in
  Proc.spawn ~name:"Receiver" run (Super.default_stop super_ch)
