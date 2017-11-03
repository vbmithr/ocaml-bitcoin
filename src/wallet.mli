module Private : sig
  val generate : Secp256k1.Context.t -> Secp256k1.Secret.t
end

module WIF : sig
  type t = private {
    privkey : Secp256k1.Secret.t ;
    testnet : bool ;
    compress : bool ;
  }

  val create :
    ?testnet:bool -> ?compress:bool -> Secp256k1.Secret.t -> t

  val to_base58 : t -> Base58.Bitcoin.t

  val of_base58 :
    Secp256k1.Context.t -> Base58.Bitcoin.t -> t
end

module Address : sig
  val of_wif : Secp256k1.Context.t -> WIF.t -> Base58.Bitcoin.t
  val of_pubkey :
    ?testnet:bool -> ?compress:bool ->
    Secp256k1.Context.t -> Secp256k1.Public.t -> Base58.Bitcoin.t
  val of_script : ?testnet:bool -> Script.t -> Base58.Bitcoin.t
  val to_script : Base58.Bitcoin.t -> Script.t
end

module KeyPath : sig
  type derivation = N of Int32.t | H of Int32.t
  type t = derivation list

  val of_string_exn : string -> t
  val of_string : string -> t option
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit

  val write_be : Caml.Bytes.t -> int -> t -> int
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
