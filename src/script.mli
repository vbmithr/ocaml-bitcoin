open Base

module Opcode : sig
  type t =
    | Op_pushdata of int
    | Op_pushdata1
    | Op_pushdata2
    | Op_pushdata4
    | Op_1negate
    | Op_1
    | Op_2
    | Op_3
    | Op_4
    | Op_5
    | Op_6
    | Op_7
    | Op_8
    | Op_9
    | Op_10
    | Op_11
    | Op_12
    | Op_13
    | Op_14
    | Op_15
    | Op_16
    | Op_nop
    | Op_if
    | Op_notif
    | Op_else
    | Op_endif
    | Op_verify
    | Op_return
    | Op_toaltstack
    | Op_fromaltstack
    | Op_ifdup
    | Op_depth
    | Op_drop
    | Op_dup
    | Op_nip
    | Op_over
    | Op_pick
    | Op_roll
    | Op_rot
    | Op_swap
    | Op_tuck
    | Op_2drop
    | Op_2dup
    | Op_3dup
    | Op_2over
    | Op_2rot
    | Op_2swap
    | Op_cat
    | Op_substr
    | Op_left
    | Op_right
    | Op_size
    | Op_invert
    | Op_and
    | Op_or
    | Op_xor
    | Op_equal
    | Op_equalverify
    | Op_1add
    | Op_1sub
    | Op_2mul
    | Op_2div
    | Op_negate
    | Op_abs
    | Op_not
    | Op_0notequal
    | Op_add
    | Op_sub
    | Op_mul
    | Op_div
    | Op_mod
    | Op_lshift
    | Op_rshift
    | Op_booland
    | Op_boolor
    | Op_numequal
    | Op_numequalverify
    | Op_numnotequal
    | Op_lessthan
    | Op_greaterthan
    | Op_lessthanorequal
    | Op_greaterthanorequal
    | Op_min
    | Op_max
    | Op_within
    | Op_ripemd160
    | Op_sha1
    | Op_sha256
    | Op_hash160
    | Op_hash256
    | Op_codeseparator
    | Op_checksig
    | Op_checksigverify
    | Op_checkmultisig
    | Op_checkmultisigverify
    | Op_checklocktimeverify
    | Op_checksequenceverify
    | Op_pubkeyhash
    | Op_pubkey
    | Op_invalidopcode
    | Op_reserved
    | Op_ver
    | Op_verif
    | Op_vernotif
    | Op_reserved1
    | Op_reserved2
    | Op_nop1
    | Op_nop4
    | Op_nop5
    | Op_nop6
    | Op_nop7
    | Op_nop8
    | Op_nop9
    | Op_nop10

  val of_int : int -> t
  val to_int : t -> int
end

module Element : sig
  type t =
    | O of Opcode.t
    | D of Cstruct.t
end

type t = Element.t list [@@deriving sexp]

val size : t -> int

val of_cstruct : Cstruct.t -> int -> t * Cstruct.t
val to_cstruct : Cstruct.t -> Element.t list -> Cstruct.t
val hash160 : t -> Util.Hash160.t

module Std : sig
  module P2PKH : sig
    val scriptRedeem : Secp256k1.Context.t -> Secp256k1.Key.public Secp256k1.Key.t -> t
    val scriptSig : Secp256k1.Context.t -> Cstruct.t -> Secp256k1.Key.public Secp256k1.Key.t -> t
  end

  module P2SH : sig
    val scriptRedeem : t -> t
  end
end

module Run : sig
  val eval_exn : t -> bool * Cstruct.t list * t
end
