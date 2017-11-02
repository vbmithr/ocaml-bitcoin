module Private = struct
  let generate ctx =
    let cs = Cstruct.create 32 in
    let rec loop_gen () =
      Sodium.Random.Bigbytes.generate_into cs.buffer ;
      match Secp256k1.Secret.of_bytes ctx cs.buffer with
      | Some t -> t
      | None -> loop_gen ()
    in loop_gen ()
end

module WIF = struct
  type t = {
    privkey : Secp256k1.Secret.t ;
    testnet : bool ;
    compress : bool ;
  }

  let create ?(testnet=false) ?(compress=true) privkey =
    { privkey ; testnet ; compress }

  let to_base58 { privkey ; testnet ; compress } =
    let version =
      Base58.Bitcoin.(if testnet then Testnet_privkey else Privkey) in
    let cs = Cstruct.create (if compress then 32 else 33) in
    Secp256k1.Secret.write cs.buffer privkey ;
    if compress then Cstruct.set_uint8 cs 32 0x01 ;
    Base58.Bitcoin.create ~version (Cstruct.to_string cs)

  let of_base58 ctx { Base58.Bitcoin.version ; payload } =
    match version with
    | Privkey | Testnet_privkey ->
      let compress = String.length payload = 33 in
      let testnet = version = Testnet_privkey in
      let cs = Cstruct.of_string payload in
      let privkey = Secp256k1.Secret.of_bytes_exn ctx cs.buffer in
      create ~testnet ~compress privkey
    | _ -> invalid_arg "WIF.to_private: input is not a privkey address"
end

module Address = struct
  let of_wif ctx { WIF.privkey ; testnet ; compress } =
    let open Secp256k1 in
    let pk = Public.of_secret ctx privkey in
    let pk = Public.to_bytes ~compress ctx pk |> Cstruct.of_bigarray in
    let hash160, _ = Util.Hash160.of_cstruct pk in
    Base58.Bitcoin.create
      ~version:(if testnet then Testnet_P2PKH else P2PKH) (Util.Hash160.to_string hash160)

  let of_pubkey ?(testnet=false) ?(compress=true) ctx pk =
    let pk = Secp256k1.Public.to_bytes ~compress ctx pk |> Cstruct.of_bigarray in
    let hash160, _ = Util.Hash160.of_cstruct pk in
    Base58.Bitcoin.create
      ~version:(if testnet then Testnet_P2PKH else P2PKH) (Util.Hash160.to_string hash160)

  let max_serialized_script_size = 520

  let of_script ?(testnet=false) script =
    let cs = Cstruct.create max_serialized_script_size in
    let cs' = Script.to_cstruct cs script in
    let hash160, _ = Util.Hash160.of_cstruct (Cstruct.sub cs 0 cs'.off) in
    Base58.Bitcoin.create
      ~version:(if testnet then Testnet_P2SH else P2SH) (Util.Hash160.to_string hash160)

  let to_script { Base58.Bitcoin.version ; payload } =
    match version with
    | P2PKH | Testnet_P2PKH ->
      Script.Element.[O Op_dup ; O Op_hash160 ;
                      O (Op_pushdata 20) ; D (Cstruct.of_string payload) ;
                      O Op_equalverify ; O Op_checksig ]
    | P2SH | Testnet_P2SH ->
      Script.Element.[ O Op_hash160 ;
                       O (Op_pushdata 20) ; D (Cstruct.of_string payload) ;
                       O Op_equalverify ]
    | _ -> invalid_arg "Address.to_script: unsupported address format"
end
