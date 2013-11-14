type 'a t =
  'a Lwt_stream.t * ('a option -> unit)

let create () =
  Lwt_stream.create ()

let write (_, w) x =
  w (Some x)

let close (_, w) =
  w None

let read (s, _) =
  Lwt_stream.next s

let iter f (s, _) =
  Lwt_stream.iter f s

let iter_s f (s, _) =
  Lwt_stream.iter_s f s

let fold_s f (s, _) =
  Lwt_stream.fold_s f s
