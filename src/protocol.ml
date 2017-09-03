(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the GNU Affero GPL license, see LICENSE.
  ---------------------------------------------------------------------------*)

open Base
open Util
module CS = Bitcoin_cstruct

module Header = struct
  type t = {
    version : Int32.t ;
    prev_block : Hash.t ;
    merkle_root : Hash.t ;
    timestamp : Timestamp.t ;
    bits : Int32.t ;
    nonce : Int32.t ;
  } [@@deriving sexp]

  let of_cstruct cs =
    let open CS.Header in
    let version = get_t_version cs in
    let prev_block, _ = get_t_prev_block cs |> Hash.of_cstruct in
    let merkle_root, _ = get_t_merkle_root cs |> Hash.of_cstruct in
    let timestamp = get_t_timestamp cs |> Timestamp.of_int32 in
    let bits = get_t_bits cs in
    let nonce = get_t_nonce cs in
    { version ; prev_block ; merkle_root ; timestamp ; bits ; nonce },
    Cstruct.shift cs sizeof_t

  let to_cstruct cs { version; prev_block; merkle_root; timestamp; bits; nonce } =
    let open CS.Header in
    set_t_version cs version ;
    set_t_prev_block (Hash.to_string prev_block) 0 cs ;
    set_t_merkle_root (Hash.to_string merkle_root) 0 cs ;
    set_t_timestamp cs (Timestamp.to_int32 timestamp) ;
    set_t_bits cs bits ;
    set_t_nonce cs nonce ;
    Cstruct.shift cs sizeof_t
end

module Outpoint = struct
  type t = {
    hash : Hash.t ;
    i : int ;
  } [@@deriving sexp]

  let of_cstruct cs =
    let open CS.Outpoint in
    let hash, _ = get_t_hash cs |> Hash.of_cstruct in
    let i = get_t_index cs |> Int32.to_int_exn in
    { hash ; i }, Cstruct.shift cs sizeof_t
end

module TxIn = struct
  type t = {
    prev_out : Outpoint.t ;
    script : Script.t ;
    seq : Int32.t ;
  } [@@deriving sexp]

  let of_cstruct cs =
    let prev_out, cs = Outpoint.of_cstruct cs in
    let script, cs = Script.of_cstruct cs in
    let seq = Cstruct.LE.get_uint32 cs 0 in
    { prev_out ; script ; seq }, Cstruct.shift cs 4
end

module TxOut = struct
  type t = {
    value : Int64.t ;
    script : Script.t ;
  } [@@deriving sexp]

  let of_cstruct cs =
    let value = Cstruct.LE.get_uint64 cs 0 in
    let cs = Cstruct.shift cs 8 in
    let script, cs = Script.of_cstruct cs in
    { value ; script }, cs
end

module Transaction = struct
  module LockTime = struct
    type t =
      | Timestamp of Timestamp.t
      | Block of int
    [@@deriving sexp]

    let of_int32 i =
      if Int32.(i < 500_000_000l)
      then Block (Int32.to_int_exn i)
      else Timestamp (Timestamp.of_int32 i)

    let of_cstruct cs =
      of_int32 (Cstruct.LE.get_uint32 cs 0), Cstruct.shift cs 4
  end

  type t = {
    version : int ;
    tx_in : TxIn.t list ;
    tx_out : TxOut.t list ;
    lock_time : LockTime.t ;
  } [@@deriving sexp]

  let of_cstruct cs =
    let version = Cstruct.LE.get_uint32 cs 0 |> Int32.to_int_exn in
    let cs = Cstruct.shift cs 4 in
    let tx_in, cs = ObjList.of_cstruct ~f:TxIn.of_cstruct cs in
    let tx_out, cs = ObjList.of_cstruct ~f:TxOut.of_cstruct cs in
    let lock_time, cs = LockTime.of_cstruct cs in
    { version ; tx_in ; tx_out ; lock_time }, cs
end

module Block = struct
  type t = {
    header : Header.t ;
    txns : Transaction.t list ;
  } [@@deriving sexp]

  let of_cstruct cs =
    let header, cs = Header.of_cstruct cs in
    let txns, cs = ObjList.of_cstruct ~f:Transaction.of_cstruct cs in
    { header ; txns }, cs
end
