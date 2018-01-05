open Base

module Private = struct
  let generate ctx =
    let cs = Cstruct.create 32 in
    let rec loop_gen () =
      Sodium.Random.Bigbytes.generate_into cs.buffer ;
      match Secp256k1.Secret.read ctx cs.buffer with
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
    Base58.Bitcoin.create ~version ~payload:(Cstruct.to_string cs)

  let of_base58 ctx { Base58.Bitcoin.version ; payload } =
    let testnet = match version with
      | Privkey -> false
      | Testnet_privkey -> true
      | _ -> invalid_arg "WIF.to_private: input is not a privkey address" in
    let compress = String.length payload = 33 in
    let cs = Cstruct.of_string payload in
    let privkey = Secp256k1.Secret.read_exn ctx cs.buffer in
    create ~testnet ~compress privkey

  let pp ppf t =
    Base58.Bitcoin.pp ppf (to_base58 t)

  let show t =
    Base58.Bitcoin.show (to_base58 t)
end

module Address = struct
  let of_wif ctx { WIF.privkey ; testnet ; compress } =
    let open Secp256k1 in
    let pk = Public.of_secret ctx privkey in
    let pk = Public.to_bytes ~compress ctx pk in
    let hash160 = Util.Hash160.compute_bigarray pk in
    Base58.Bitcoin.create
      ~version:(if testnet then Testnet_P2PKH else P2PKH)
      ~payload:(Util.Hash160.to_string hash160)

  let of_pubkey ?(testnet=false) ?(compress=true) ctx pk =
    let pk = Secp256k1.Public.to_bytes ~compress ctx pk in
    let hash160 = Util.Hash160.compute_bigarray pk in
    Base58.Bitcoin.create
      ~version:(if testnet then Testnet_P2PKH else P2PKH)
      ~payload:(Util.Hash160.to_string hash160)

  let max_serialized_script_size = 520

  let of_script ?(testnet=false) script =
    let cs = Cstruct.create max_serialized_script_size in
    let cs' = Script.to_cstruct cs script in
    let hash160 = Util.Hash160.compute_cstruct (Cstruct.sub cs 0 cs'.off) in
    Base58.Bitcoin.create
      ~version:(if testnet then Testnet_P2SH else P2SH)
      ~payload:(Util.Hash160.to_string hash160)

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

module KeyPath = struct
  let derivation_of_string d =
    match String.(get d (length d - 1)) with
    | '\'' ->
      let v = String.(sub d 0 (length d - 1)) |> Int32.of_string in
      Int32.(0x8000_0000l lor v)
    | _ ->
      Int32.of_string d

  let string_of_derivation = function
    | i when Int32.(0x8000_0000l land i = 0l) -> Int32.to_string i
    | i -> Int32.to_string i ^ "'"

  type t = Int32.t list

  let of_string_exn s =
    try
      let derivations = String.split ~on:'/' s in
      List.map derivations ~f:derivation_of_string
    with _ ->
      invalid_arg (Printf.sprintf "KeyPath.of_string_exn: got %S" s)

  let of_string s =
    try Some (of_string_exn s) with _ -> None

  let to_string t =
    List.map t ~f:string_of_derivation |>
    String.concat ~sep:"/"

  let pp ppf t =
    Caml.Format.pp_print_string ppf (to_string t)

  let write_be buf pos t =
    let open EndianBytes.BigEndian in
    let len =
      List.fold_left t ~init:0 ~f:begin fun i v ->
        set_int32 buf (pos+i*4) v; i+1
      end in
    pos + len * 4

  let write_be_cstruct cs t =
    let open Cstruct in
    List.fold_left t ~init:cs ~f:begin fun cs v ->
      BE.set_uint32 cs 0 v; Cstruct.shift cs 4
    end
end

module Bip44 = struct
  module CoinType = struct
    type t =
      | Bitcoin
      | Bitcoin_testnet

    let to_int32 = function
      | Bitcoin -> 0l
      | Bitcoin_testnet -> 1l

    let of_int32 = function
      | 0l -> Bitcoin
      | 1l -> Bitcoin_testnet
      | _ -> invalid_arg "Bip44.CoinType.of_int"

    let pp ppf ct =
      Int32.pp ppf (to_int32 ct)
  end

  module Chain = struct
    type t =
      | External
      | Internal

    let to_int32 = function
      | External -> 0l
      | Internal -> 1l

    let of_int32 = function
      | 0l -> External
      | 1l -> Internal
      | _ -> invalid_arg "Bip44.Chain.of_int"

    let pp ppf chain =
      Int32.pp ppf (to_int32 chain)
  end

  module Purpose = struct
    type t = Bip44

    let to_int32 = function
      | Bip44 -> 44l

    let of_int32 = function
      | 44l -> Bip44
      | _ -> invalid_arg "Bip44.Purpose.of_int"

    let pp ppf purpose =
      Int32.pp ppf (to_int32 purpose)
  end

  type t = {
    purpose : Purpose.t ;
    coin_type : CoinType.t ;
    account : int ;
    chain : Chain.t ;
    index : int ;
  }

  let of_hardened i = Int32.(i land 0x7fff_ffffl)
  let to_hardened i = Int32.(i lor 0x8000_0000l)

  let create
      ?(purpose=Purpose.Bip44) ?(coin_type=CoinType.Bitcoin)
      ?(account=0) ?(chain=Chain.External) ?(index=0) () =
    { purpose ; coin_type ; account ; chain ; index }

  let of_keypath = function
    | [ purpose ; coin_type ; account ; chain ; index] ->
      let purpose = Purpose.of_int32 (of_hardened purpose) in
      let coin_type = CoinType.of_int32 (of_hardened coin_type) in
      let account = Int32.to_int_exn (of_hardened account) in
      let chain = Chain.of_int32 chain in
      let index = Int32.to_int_exn index in
      { purpose ; coin_type ; account ; chain ; index }
    | _ -> invalid_arg "Bip44.of_keypath"

  let to_keypath { purpose ; coin_type ; account ; chain ; index } =
    KeyPath.[to_hardened (Purpose.to_int32 purpose) ;
             to_hardened (CoinType.to_int32 coin_type) ;
             to_hardened (Int32.of_int_exn account) ;
             Chain.to_int32 chain ;
             Int32.of_int_exn index ]
end

