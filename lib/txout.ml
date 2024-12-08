open Sexplib.Std
open Util

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
