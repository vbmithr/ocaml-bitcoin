(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the GNU Affero GPL license, see LICENSE.
  ---------------------------------------------------------------------------*)

open Util

module Header : sig
  type t = {
    version : Int32.t ;
    prev_block : Hash256.t ;
    merkle_root : Hash256.t ;
    timestamp : Timestamp.t ;
    bits : Int32.t ;
    nonce : Int32.t ;
  } [@@deriving sexp]

  val genesis : t
  val genesis_hash : Hash256.t

  val compare : t -> t -> int
  val equal : t -> t -> bool
  (* val hash : t -> int *)

  val of_cstruct : Cstruct.t -> t * Cstruct.t
  val of_cstruct_txcount : Cstruct.t -> t * Cstruct.t
  (** For reading headers from a Header P2P message. *)

  val to_cstruct : Cstruct.t -> t -> Cstruct.t

  val size : int
  (** Serialized size *)

  val hash256 : t -> Hash256.t
end

module Outpoint : sig
  type t = {
    hash : Hash256.t ;
    i : int ;
  }

  val of_cstruct : Cstruct.t -> t * Cstruct.t
  val to_cstruct : Cstruct.t -> t -> Cstruct.t
end

module TxIn : sig
  type t = {
    prev_out : Outpoint.t ;
    script : Script.t ;
    seq : Int32.t ;
  }

  val size : t -> int
  val of_cstruct : Cstruct.t -> t * Cstruct.t
  val to_cstruct : Cstruct.t -> t -> Cstruct.t
end

module TxOut : sig
  type t = {
    value : Int64.t ;
    script : Script.t ;
  }

  val size : t -> int
  val of_cstruct : Cstruct.t -> t * Cstruct.t
  val to_cstruct : Cstruct.t -> t -> Cstruct.t
end

module Transaction : sig
  module LockTime : sig
    type t =
      | Timestamp of Timestamp.t
      | Block of int

    val to_int32 : t -> Int32.t

    val of_cstruct : Cstruct.t -> t * Cstruct.t
    val to_cstruct : Cstruct.t -> t -> Cstruct.t
  end

  type t = {
    version : int ;
    inputs : TxIn.t list ;
    outputs : TxOut.t list ;
    lock_time : LockTime.t ;
  } [@@deriving sexp]

  val of_cstruct : Cstruct.t -> t * Cstruct.t
  val to_cstruct : Cstruct.t -> t -> Cstruct.t

  val size : t -> int
  val hash256 : t -> Hash256.t
end

module Block : sig
  type t = {
    header : Header.t ;
    txns : Transaction.t list ;
  } [@@deriving sexp]

  val of_cstruct : Cstruct.t -> t * Cstruct.t
end
