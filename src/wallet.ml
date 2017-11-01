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
    Public.to_bytes ~compress ctx pk |> Cstruct.of_bigarray |> Cstruct.to_string |>
    Base58.Bitcoin.create ~version:(if testnet then P2PKH else Testnet_P2PKH)

  let of_pubkey ?(testnet=false) ?(compress=true) ctx pk =
    Secp256k1.Public.to_bytes ~compress ctx pk |> Cstruct.of_bigarray |> Cstruct.to_string |>
    Base58.Bitcoin.create ~version:(if testnet then P2PKH else Testnet_P2PKH)
end
