(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the GNU Affero GPL license, see LICENSE.
  ---------------------------------------------------------------------------*)

module Hash = struct
  type t = Hash of string

  let of_string s =
    if String.length s <> 32 then invalid_arg "Hash.of_string" else Hash s

  let to_string (Hash s) = s

  let of_cstruct cs =
    Hash (Cstruct.copy cs 0 32)
end

module BlockHeader = struct
  module C = struct
    [%%cstruct type t = {
        version: uint32_t ;
        prev_block : uint8_t [@len 32] ;
        merkle_root : uint8_t [@len 32] ;
        timestamp : uint32_t ;
        bits : uint32_t ;
        nonce : uint32_t ;
        txn_count : uint8_t ;
      } [@@little_endian]]
  end

  type t = {
    version : Int32.t ;
    prev_block : Hash.t ;
    merkle_root : Hash.t ;
    timestamp : Ptime.t ;
    bits : Int32.t ;
    nonce : Int32.t ;
  }

  let of_cstruct cs =
    let open C in
    let version = get_t_version cs in
    let prev_block = get_t_prev_block cs |> Hash.of_cstruct in
    let merkle_root = get_t_merkle_root cs |> Hash.of_cstruct in
    let timestamp =
      match get_t_timestamp cs |> Int32.to_float |> Ptime.of_float_s with
      | None -> invalid_arg "BlockHeader.read: wrong timestamp"
      | Some ts -> ts in
    let bits = get_t_bits cs in
    let nonce = get_t_nonce cs in
    { version ; prev_block ; merkle_root ; timestamp ; bits ; nonce }
end
