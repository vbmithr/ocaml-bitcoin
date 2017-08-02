(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the GNU Affero GPL license, see LICENSE.
  ---------------------------------------------------------------------------*)

open Util

module Header : sig
  type t = {
    version : Int32.t ;
    prev_block : Hash.t ;
    merkle_root : Hash.t ;
    timestamp : Ptime.t ;
    bits : Int32.t ;
    nonce : Int32.t ;
  }

  val of_cstruct : Cstruct.t -> t * Cstruct.t
end

module Outpoint : sig
  type t = {
    hash : Hash.t ;
    i : int ;
  }
end

module Script : sig
  type t

  val of_cstruct :
    Cstruct.t -> int -> t * Cstruct.t
end

module TxIn : sig
  type t = {
    prev_out : Outpoint.t ;
    script : Script.t ;
    seq : Int32.t ;
  }
end

module TxOut : sig
  type t = {
    value : Int64.t ;
    script : Script.t ;
  }
end

module Transaction : sig
  module LockTime : sig
    type t =
      | Timestamp of Ptime.t
      | Block of int
  end

  type t = {
    version : int ;
    tx_in : TxIn.t list ;
    tx_out : TxOut.t list ;
    lock_time : LockTime.t ;
  }
end

module Block : sig
  type t = {
    header : Header.t ;
    txns : Transaction.t list ;
  }

  val of_cstruct : Cstruct.t -> t * Cstruct.t
end
