open Libsecp256k1.External

module Private : sig
  val generate : Context.t -> Key.secret Key.t
end

module WIF : sig
  type t = private {
    privkey : Key.secret Key.t ;
    testnet : bool ;
    compress : bool ;
  }

  val pp : Context.t -> Format.formatter -> t -> unit
  val show : Context.t -> t -> string

  val create :
    ?testnet:bool -> ?compress:bool -> Key.secret Key.t -> t

  val to_base58 : Context.t -> t -> Base58.Bitcoin.t
  val of_base58 : Context.t -> Base58.Bitcoin.t -> t
end

module Address : sig
  val of_wif : Context.t -> WIF.t -> Base58.Bitcoin.t
  val of_pubkey :
    ?testnet:bool -> ?compress:bool ->
    Context.t -> Key.public Key.t -> Base58.Bitcoin.t
  val of_script : ?testnet:bool -> Script.t -> Base58.Bitcoin.t
  val to_script : Base58.Bitcoin.t -> Script.t
end

module KeyPath : sig
  type t = Int32.t list

  val of_hardened : int32 -> int32
  val to_hardened : int32 -> int32

  val of_string_exn : string -> t
  val of_string : string -> t option
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit

  val write_be : Bytes.t -> int -> t -> int
  val write_be_cstruct : Cstruct.t -> t -> Cstruct.t
end

module Bip44 : sig
  module Purpose : sig
    type t = Bip44
  end

  module CoinType : sig
    type t =
      | Bitcoin
      | Bitcoin_testnet
  end

  module Chain : sig
    type t =
      | External
      | Internal
  end

  type t = {
    purpose : Purpose.t ;
    coin_type : CoinType.t ;
    account : int ;
    chain : Chain.t ;
    index : int ;
  }

  val create :
    ?purpose:Purpose.t -> ?coin_type:CoinType.t ->
    ?account:int -> ?chain:Chain.t -> ?index:int ->
    unit -> t

  val of_keypath : KeyPath.t -> t
  val to_keypath : t -> KeyPath.t
end
