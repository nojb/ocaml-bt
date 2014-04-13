let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)
              
type 'a t = {
  seq : 'a Lwt_sequence.t;
  popped : unit Lwt_condition.t;
  capacity : int
}

let create ?(capacity = 0) () =
  { seq = Lwt_sequence.create ();
    popped = Lwt_condition.create ();
    capacity }

let rec push seq x =
  if Lwt_sequence.length seq.seq > seq.capacity then begin
    Lwt_condition.wait seq.popped >>= fun () -> push seq x
  end else begin
    ignore (Lwt_sequence.add_r x seq.seq);
    Lwt.return ()
  end

let push' seq x =
  ignore (Lwt_sequence.add_r x seq.seq)

let filter seq f =
  Lwt_sequence.iter_node_r
    (fun n -> if not (f (Lwt_sequence.get n)) then Lwt_sequence.remove n) seq.seq;
  Lwt_condition.signal seq.popped ()

let length seq =
  Lwt_sequence.length seq.seq

let is_empty seq =
  Lwt_sequence.is_empty seq.seq

let assoc ?(equal = (=)) seq x =
  let n = Lwt_sequence.find_node_l (fun (a, _) -> equal a x) seq.seq in
  snd (Lwt_sequence.get n)

let remove_assoc ?(equal = (=)) seq x =
  let n = Lwt_sequence.find_node_l (fun (a, _) -> equal a x) seq.seq in
  Lwt_sequence.remove n;
  Lwt_condition.signal seq.popped ();
  snd (Lwt_sequence.get n)

let iter seq f =
  Lwt_sequence.iter_l f seq.seq
