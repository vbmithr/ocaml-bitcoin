type t = private {
  filter : Bitv.t ;
  nb_funcs : int ;
  tweak : Int32.t ;
} [@@deriving sexp]

val filter_len : t -> int
(** [filter_len t] is Bitv.length t.filter. *)

val create : int -> float -> Int32.t -> t
(** [create max_elts false_pos_rate tweak] is a bloom filter
    configured to hold a maximum of [max_elts] for a false positive
    rate below [false_pos_rate]. *)

val to_filter : t -> string
(** [to_filter t] is the serialized bit vector (FilterLoad "filter"
    field). *)

val of_filter : string -> int -> Int32.t -> t
(** [import bitv nb_funcs tweak] imports a bloom filter from a
    FilterLoad message. *)

val reset : t -> t
val add : t -> Cstruct.t -> unit
val mem : t -> Cstruct.t -> bool
