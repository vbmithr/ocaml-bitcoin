(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the GNU Affero GPL license, see LICENSE.
  ---------------------------------------------------------------------------*)

open Util
open Libsecp256k1.External

module Header : sig
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
end

module Outpoint : sig
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
end

module TxIn : sig
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
end

module TxOut : sig
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
end

module Transaction : sig
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
    ; inputs : TxIn.t array
    ; outputs : TxOut.t array
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
    -> inputs:TxIn.t array
    -> outputs:TxOut.t array
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

  (** [sign ?prev_out_script t i sk sighash] is the endorsement of
        [t] by input [i], using secret key [sk] and sighash
        [sighash]. If [prev_out_script] is provided, it is used as the
        script for the [i]'s input, otherwise [i]'s input script is
        left as-is. *)
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
end

module Block : sig
  type t =
    { header : Header.t
    ; txns : Transaction.t list
    }
  [@@deriving sexp]

  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val of_cstruct : Cstruct.t -> t * Cstruct.t
end
