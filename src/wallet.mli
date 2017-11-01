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
end
