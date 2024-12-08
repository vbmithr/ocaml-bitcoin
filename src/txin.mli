open Util

type t =
  { prev_out : Outpoint.t
  ; script : Script.t
  ; seq : Int32.t
  }
[@@deriving sexp]

val pp : Format.formatter -> t -> unit
val show : t -> string
val create : ?seq:Int32.t -> prev_out:Outpoint.t -> script:Script.t -> unit -> t

val create'
  :  ?seq:Int32.t
  -> prev_out_hash:Hash256.t
  -> prev_out_i:int
  -> script:Script.t
  -> unit
  -> t

val size : t -> int
val of_cstruct : Cstruct.t -> t * Cstruct.t
val to_cstruct : Cstruct.t -> t -> Cstruct.t

(** [remove_script t] is [t] with [t.script] set to [[]]. *)
val remove_script : t -> t
