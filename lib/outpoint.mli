open Util

type t =
  { hash : Hash256.t
  ; i : int
  }
[@@deriving sexp]

(* val pp : Format.formatter -> t -> unit
   * val show : t -> string *)

val create : Hash256.t -> int -> t
val of_cstruct : Cstruct.t -> t * Cstruct.t
val to_cstruct : Cstruct.t -> t -> Cstruct.t
val pp : Format.formatter -> t -> unit
val show : t -> string
val size : int
