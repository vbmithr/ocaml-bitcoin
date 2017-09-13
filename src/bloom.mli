type t

val create : ?tweak:Int32.t -> int -> float -> t
val reset : t -> t
val add : t -> Cstruct.t -> unit
val mem : t -> Cstruct.t -> bool
