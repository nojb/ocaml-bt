(** bitfields *)

type t

val create : int -> t

val length : t -> int

val resize : t -> int -> unit

val set_length : t -> int -> unit

val clear : t -> unit
(** [clear b] sets every bit of [b] to [0]. *)

val set_all : t -> unit
(** [set_all b] sets every bit of [b] to [1]. *)

val copy : t -> t
(** Copy the bitfield. *)

val count_ones : t -> int
(** [count_ones b] is the number of [1]'s in [b]. *)

val count_zeroes : t -> int
(** [count_zeroes b size] is the number of those [i], [0 <= i < size] such
    that the [i]-th bit of [b] is [0]. *)

(* val equal : t -> t -> bool *)
(** Whether two bitfields are equal.  They must have the same length to be
    equal. *)

val set : t -> int -> unit
(** [set b i] sets the [i]-th bit of [b] to [1]. *)

val unset : t -> int -> unit
(** [unset b i] sets the [i]-th bit of [b] to [0]. *)

val is_set : t -> int -> bool
(** [unset b i] is [true] if the [i]-th bit of [b] is [1], else [false]. *)

val of_cstruct : Cstruct.t -> t
(** Make a bitfield from the bits of the input string. *)

val to_cstruct : t -> Cstruct.t
(** Pack the bitfield into bytes.  The length of the output string is the
    smallest integer larger or equal to [l/8] where [l] is the length of the
    bitfield. *)

(* val copy_into : t -> t -> unit *)
(** [copy_into b1 b2] copies [b1] into [b2], enlargin [b2] if necessary. *)

val has_all : t -> bool
(** [has_all b size] is [count_zeroes b size = 0]. *)

val blit : t -> int -> t -> int -> int -> unit
