module CS = Bitcoin_cstruct

type t =
  { header : Header.t
  ; txns : Transaction.t list
  }
[@@deriving sexp]

val pp : Format.formatter -> t -> unit
val show : t -> string
val of_cstruct : Cstruct.t -> t * Cstruct.t
