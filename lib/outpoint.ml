open Sexplib.Std
open Util
module CS = Bitcoin_cstruct

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
