open Sexplib.Std
open Util

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
