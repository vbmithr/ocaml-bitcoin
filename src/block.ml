open Sexplib.Std
open Util
module CS = Bitcoin_cstruct

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
