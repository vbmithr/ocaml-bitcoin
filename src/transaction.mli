open Libsecp256k1.External
open Util

module LockTime : sig
  type t =
    | Timestamp of Timestamp.t
    | Block of int

  val timestamp : Timestamp.t -> t
  val block : int -> t
  val to_int32 : t -> Int32.t
  val of_cstruct : Cstruct.t -> t * Cstruct.t
  val to_cstruct : Cstruct.t -> t -> Cstruct.t
end

type t =
  { version : int
  ; inputs : Txin.t array
  ; outputs : Txout.t array
  ; lock_time : LockTime.t
  }
[@@deriving sexp]

val nb_inputs : t -> int
val nb_outputs : t -> int
val pp : Format.formatter -> t -> unit
val show : t -> string

val create
  :  ?version:int
  -> ?lock_time:LockTime.t
  -> inputs:Txin.t array
  -> outputs:Txout.t array
  -> unit
  -> t

val of_cstruct : Cstruct.t -> t * Cstruct.t
val to_cstruct : Cstruct.t -> t -> Cstruct.t
val of_hex : Hex.t -> t
val to_hex : t -> Hex.t
val size : t -> int
val hash256 : t -> Hash256.t

type sighash =
  | All
  | None
  | Single
  | AllAny
  | NoneAny
  | SingleAny

val int_of_sighash : sighash -> int

(** [sign ?prev_out_script t i sk sighash] is the endorsement of [t]
    by input [i], using secret key [sk] and sighash [sighash]. If
    [prev_out_script] is provided, it is used as the script for the
    [i]'s input, otherwise [i]'s input script is left as-is. *)
val sign
  :  ?prev_out_script:Script.t
  -> t
  -> int
  -> Key.secret Key.t
  -> sighash
  -> Cstruct.t

(** See above, but for Bitcoin Cash. *)
val sign_bch
  :  ?prev_out_script:Script.t
  -> t
  -> int
  -> Key.secret Key.t
  -> sighash
  -> Cstruct.t
