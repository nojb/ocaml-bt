open Eio.Std

let get ~net ~addr ~stdout ~info_hash:_ ~peer_id:_ ~port:_ ~uploaded:_
    ~downloaded:_ ~left:_ ~event:_ =
  Switch.run @@ fun sw ->
  let flow = Eio.Net.connect ~sw net addr in
  let payload = "GET /" in
  Eio.Flow.copy_string payload flow;
  let buf = Buffer.create 1024 in
  Eio.Flow.copy flow (Eio.Flow.buffer_sink buf);
  Eio.Flow.shutdown flow `Send;
  Eio.Flow.copy flow Eio.stdout
