open Sexplib.Std
open Libsecp256k1.External
open Util

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
  ; inputs : Txin.t array
  ; outputs : Txout.t array
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
  8 + ObjArray.(size inputs ~f:Txin.size + size outputs ~f:Txout.size)
;;

let of_cstruct cs =
  let version = Cstruct.LE.get_uint32 cs 0 |> Int32.to_int in
  let cs = Cstruct.shift cs 4 in
  let inputs, cs = ObjArray.of_cstruct ~f:Txin.of_cstruct cs in
  let outputs, cs = ObjArray.of_cstruct ~f:Txout.of_cstruct cs in
  let lock_time, cs = LockTime.of_cstruct cs in
  { version; inputs; outputs; lock_time }, cs
;;

let to_cstruct cs { version; inputs; outputs; lock_time } =
  Cstruct.LE.set_uint32 cs 0 (Int32.of_int version);
  let cs = Cstruct.shift cs 4 in
  let cs = ObjArray.to_cstruct cs inputs ~f:Txin.to_cstruct in
  let cs = ObjArray.to_cstruct cs outputs ~f:Txout.to_cstruct in
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
           then Txin.remove_script input
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
