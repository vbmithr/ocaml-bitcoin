type t =
  { value : Int64.t
  ; script : Script.t
  }
[@@deriving sexp]

val pp : Format.formatter -> t -> unit
val show : t -> string
val create : value:Int64.t -> script:Script.t -> t
val size : t -> int
val of_cstruct : Cstruct.t -> t * Cstruct.t
val to_cstruct : Cstruct.t -> t -> Cstruct.t
