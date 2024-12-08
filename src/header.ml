open Sexplib.Std
open Util
module CS = Bitcoin_cstruct

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
  { version; prev_block; merkle_root; timestamp; bits; nonce }, Cstruct.shift cs sizeof_t
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
