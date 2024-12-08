open Util

type t =
  { version : Int32.t
  ; prev_block : Hash256.t
  ; merkle_root : Hash256.t
  ; timestamp : Timestamp.t
  ; bits : Int32.t
  ; nonce : Int32.t
  }
[@@deriving sexp]

val genesis : t
val genesis_hash : Hash256.t
val compare : t -> t -> int
val equal : t -> t -> bool
(* val hash : t -> int *)

val of_cstruct : Cstruct.t -> t * Cstruct.t

(** For reading headers from a Header P2P message. *)
val of_cstruct_txcount : Cstruct.t -> t * Cstruct.t

val to_cstruct : Cstruct.t -> t -> Cstruct.t

(** Serialized size *)
val size : int

val hash256 : t -> Hash256.t
