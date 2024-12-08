(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the GNU Affero GPL license, see LICENSE.
  ---------------------------------------------------------------------------*)

open Sexplib.Std
open Util
open Libsecp256k1.External
module CS = Bitcoin_cstruct

module Header = struct
  type t =
    { version : int32
    ; prev_block : Hash256.t
    ; merkle_root : Hash256.t
    ; timestamp : Timestamp.t
    ; bits : int32
    ; nonce : int32
    }
  [@@deriving sexp]

  let genesis =
    { version = 1l
    ; prev_block = Hash256.empty
    ; merkle_root =
        Hash256.of_hex_internal
          (`Hex "3BA3EDFD7A7B12B27AC72C3E67768F617FC81BC3888A51323A9FB8AA4B1E5E4A")
    ; timestamp = Timestamp.of_int_sec 1231006505
    ; bits = 0x1d00ffffl
    ; nonce = 2083236893l
    }
  ;;

  let of_cstruct cs =
    let open CS.Header in
    let version = get_t_version cs in
    let prev_block, _ = get_t_prev_block cs |> Hash256.of_cstruct in
    let merkle_root, _ = get_t_merkle_root cs |> Hash256.of_cstruct in
    let timestamp = get_t_timestamp cs |> Timestamp.of_int32_sec in
    let bits = get_t_bits cs in
    let nonce = get_t_nonce cs in
    ( { version; prev_block; merkle_root; timestamp; bits; nonce }
    , Cstruct.shift cs sizeof_t )
  ;;

  let of_cstruct_txcount cs =
    let t, cs = of_cstruct cs in
    t, Cstruct.shift cs 1
  ;;

  let to_cstruct cs { version; prev_block; merkle_root; timestamp; bits; nonce } =
    let open CS.Header in
    set_t_version cs version;
    set_t_prev_block (Hash256.to_string prev_block) 0 cs;
    set_t_merkle_root (Hash256.to_string merkle_root) 0 cs;
    set_t_timestamp cs (Timestamp.to_int32_sec timestamp);
    set_t_bits cs bits;
    set_t_nonce cs nonce;
    Cstruct.shift cs sizeof_t
  ;;

  let size = CS.Header.sizeof_t

  let hash256 t =
    let cs = Cstruct.create size in
    let _ = to_cstruct cs t in
    Hash256.compute_cstruct cs
  ;;

  let compare = Stdlib.compare
  let equal = Stdlib.( = )

  (* let hash t = *)
  (*   let Hash256.Hash s = hash256 t in *)
  (*   let i32 = EndianString.BigEndian.get_int32 s 0 in *)
  (*   Int32.(i32 lsr 1 |> to_int_exn) *)

  let genesis_hash = hash256 genesis
end

module Outpoint = struct
  type t =
    { hash : Hash256.t
    ; i : int
    }
  [@@deriving sexp]

  let pp ppf { hash; i } = Format.fprintf ppf "%a %d" Hash256.pp hash i
  let show { hash; i } = Format.asprintf "%a %d" Hash256.pp hash i
  let create hash i = { hash; i }
  let size = CS.Outpoint.sizeof_t

  let of_cstruct cs =
    let open CS.Outpoint in
    let hash, _ = get_t_hash cs |> Hash256.of_cstruct in
    let i = get_t_index cs |> Int32.to_int in
    { hash; i }, Cstruct.shift cs sizeof_t
  ;;

  let to_cstruct cs { hash = Hash payload; i } =
    let open CS.Outpoint in
    set_t_hash payload 0 cs;
    set_t_index cs (Int32.of_int i);
    Cstruct.shift cs size
  ;;
end

module TxIn = struct
  type t =
    { prev_out : Outpoint.t
    ; script : Script.t
    ; seq : int32
    }
  [@@deriving sexp]

  let pp ppf t = Format.fprintf ppf "%a" Sexplib.Sexp.pp_hum (sexp_of_t t)
  let show t = Sexplib.Sexp.to_string_hum (sexp_of_t t)
  let create ?(seq = 0xffffffffl) ~prev_out ~script () = { prev_out; script; seq }

  let create' ?(seq = 0xffffffffl) ~prev_out_hash ~prev_out_i ~script () =
    let prev_out = Outpoint.create prev_out_hash prev_out_i in
    { prev_out; script; seq }
  ;;

  let size { script; _ } =
    let scriptsize = Script.size script in
    let scriptsizesize = CompactSize.(of_int scriptsize |> size) in
    Outpoint.size + scriptsizesize + scriptsize + 4
  ;;

  let of_cstruct cs =
    let prev_out, cs = Outpoint.of_cstruct cs in
    let scriptsize, cs = CompactSize.of_cstruct_int cs in
    let script, cs = Script.of_cstruct cs ~len:scriptsize in
    let seq = Cstruct.LE.get_uint32 cs 0 in
    { prev_out; script; seq }, Cstruct.shift cs 4
  ;;

  let to_cstruct cs { prev_out; script; seq } =
    let scriptsize = Script.size script in
    let cs = Outpoint.to_cstruct cs prev_out in
    let cs = CompactSize.to_cstruct_int cs scriptsize in
    let cs = Script.to_cstruct cs script in
    Cstruct.LE.set_uint32 cs 0 seq;
    Cstruct.shift cs 4
  ;;

  let remove_script t = { t with script = [] }
end

module TxOut = struct
  type t =
    { value : int64
    ; script : Script.t
    }
  [@@deriving sexp]

  let pp ppf t = Format.fprintf ppf "%a" Sexplib.Sexp.pp_hum (sexp_of_t t)
  let show t = Sexplib.Sexp.to_string_hum (sexp_of_t t)
  let create ~value ~script = { value; script }

  let size { script; _ } =
    let scriptsize = Script.size script in
    let scriptsizesize = CompactSize.(of_int scriptsize |> size) in
    8 + scriptsizesize + scriptsize
  ;;

  let of_cstruct cs =
    let value = Cstruct.LE.get_uint64 cs 0 in
    let scriptsize, cs = CompactSize.of_cstruct_int (Cstruct.shift cs 8) in
    let script, cs = Script.of_cstruct cs ~len:scriptsize in
    { value; script }, cs
  ;;

  let to_cstruct cs { value; script } =
    let scriptsize = Script.size script in
    Cstruct.LE.set_uint64 cs 0 value;
    let cs = CompactSize.to_cstruct_int (Cstruct.shift cs 8) scriptsize in
    Script.to_cstruct cs script
  ;;
end

module Transaction = struct
  module LockTime = struct
    type t =
      | Timestamp of Timestamp.t
      | Block of int
    [@@deriving sexp]

    let timestamp ts = Timestamp ts
    let block height = Block height

    let of_int32 i =
      if i < 500_000_000l
      then Block (Int32.to_int i)
      else Timestamp (Timestamp.of_int32_sec i)
    ;;

    let to_int32 = function
      | Block n -> Int32.of_int n
      | Timestamp ts -> Timestamp.to_int32_sec ts
    ;;

    let of_cstruct cs = of_int32 (Cstruct.LE.get_uint32 cs 0), Cstruct.shift cs 4

    let to_cstruct cs t =
      Cstruct.LE.set_uint32 cs 0 (to_int32 t);
      Cstruct.shift cs 4
    ;;
  end

  type t =
    { version : int
    ; inputs : TxIn.t array
    ; outputs : TxOut.t array
    ; lock_time : LockTime.t
    }
  [@@deriving sexp]

  let nb_inputs { inputs; _ } = Array.length inputs
  let nb_outputs { outputs; _ } = Array.length outputs
  let pp ppf t = Format.fprintf ppf "%a" Sexplib.Sexp.pp_hum (sexp_of_t t)
  let show t = Sexplib.Sexp.to_string_hum (sexp_of_t t)

  let create ?(version = 1) ?(lock_time = LockTime.block 0) ~inputs ~outputs () =
    { version; inputs; outputs; lock_time }
  ;;

  let size { inputs; outputs; _ } =
    8 + ObjArray.(size inputs ~f:TxIn.size + size outputs ~f:TxOut.size)
  ;;

  let of_cstruct cs =
    let version = Cstruct.LE.get_uint32 cs 0 |> Int32.to_int in
    let cs = Cstruct.shift cs 4 in
    let inputs, cs = ObjArray.of_cstruct ~f:TxIn.of_cstruct cs in
    let outputs, cs = ObjArray.of_cstruct ~f:TxOut.of_cstruct cs in
    let lock_time, cs = LockTime.of_cstruct cs in
    { version; inputs; outputs; lock_time }, cs
  ;;

  let to_cstruct cs { version; inputs; outputs; lock_time } =
    Cstruct.LE.set_uint32 cs 0 (Int32.of_int version);
    let cs = Cstruct.shift cs 4 in
    let cs = ObjArray.to_cstruct cs inputs ~f:TxIn.to_cstruct in
    let cs = ObjArray.to_cstruct cs outputs ~f:TxOut.to_cstruct in
    LockTime.to_cstruct cs lock_time
  ;;

  let to_hex t =
    let cs = Cstruct.create (size t) in
    let _ = to_cstruct cs t in
    Hex.of_cstruct cs
  ;;

  let of_hex hex =
    let cs = Hex.to_cstruct hex in
    fst (of_cstruct cs)
  ;;

  let hash256 t =
    let cs = Cstruct.create (size t) in
    let _ = to_cstruct cs t in
    Hash256.compute_cstruct cs
  ;;

  type sighash =
    | All
    | None
    | Single
    | AllAny
    | NoneAny
    | SingleAny

  let int_of_sighash = function
    | All -> 0x01
    | None -> 0x02
    | Single -> 0x03
    | AllAny -> 0x81
    | NoneAny -> 0x82
    | SingleAny -> 0x83
  ;;

  let sign ?prev_out_script t idx sk kind =
    if idx < 0 || idx >= nb_inputs t
    then
      invalid_arg
        (Printf.sprintf "Protocol.Transaction.sign: %d is not a valid input index" idx);
    match kind with
    | All ->
      let inputs =
        Array.mapi
          (fun i input ->
             if i <> idx
             then TxIn.remove_script input
             else (
               match prev_out_script with
               | None -> input
               | Some script -> { input with script }))
          t.inputs
      in
      let t = { t with inputs } in
      let cs = Cstruct.create (size t + 1) in
      let cs = to_cstruct cs t in
      Cstruct.set_uint8 cs 0 (int_of_sighash kind);
      let Util.Hash256.Hash h, _ = Util.Hash256.of_cstruct cs in
      let signature = Sign.sign_exn Util.context ~sk (Bigstring.of_string h) in
      let signature_bytes = Sign.to_bytes ~der:true Util.context signature in
      let signature_length = Bigstring.length signature_bytes in
      let signature_bytes_final = Bigstring.create (signature_length + 1) in
      Bigstring.blit signature_bytes 0 signature_bytes_final 0 signature_length;
      Bigstring.set signature_bytes_final signature_length '\x01';
      Cstruct.of_bigarray signature_bytes_final
    | _ -> invalid_arg "Protocol.Transaction.sign: signature type unsupported"
  ;;

  let sign_bch ?prev_out_script t idx sk kind =
    ignore (prev_out_script, t, idx, sk, kind);
    invalid_arg "Protocol.Transaction.sign_bch: unsupported"
  ;;
end

module Block = struct
  type t =
    { header : Header.t
    ; txns : Transaction.t list
    }
  [@@deriving sexp]

  let pp ppf t = Sexplib.Sexp.pp_hum ppf (sexp_of_t t)
  let show t = Format.asprintf "%a" pp t

  let of_cstruct cs =
    let header, cs = Header.of_cstruct cs in
    let txns, cs = ObjList.of_cstruct ~f:Transaction.of_cstruct cs in
    { header; txns }, cs
  ;;
end
