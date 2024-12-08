open Sexplib.Std
open Util

let bytes_max = 36000
let funcs_max = 50
let seed_mult = 0xfba4c795l

type t = {
  filter : Bitv.t ;
  len : int ;
  nb_funcs : int ;
  tweak : int32 ;
} [@@deriving sexp]

(* let filter_len { filter; _ } =
 *   Bitv.length filter / 8 *)

let to_filter { filter; _ } =
  try Bitv.to_string_le filter with _ ->
    invalid_arg "Bloom.to_string"

let pp_hex ppf t =
  let `Hex filter_hex = Hex.of_string (to_filter t) in
  Format.fprintf ppf "%s" filter_hex

let of_filter filter nb_funcs tweak =
  let len = String.length filter in
  if len > bytes_max ||
     nb_funcs > funcs_max then
    invalid_arg "Bloom.of_filter" ;
  { filter = Bitv.of_string_le filter ; len ; nb_funcs ; tweak }

let create n p tweak =
  let n = Float.of_int n in
  let filter_len_bytes =
    let open Float in
    min
      (-1. /. (log 2. *. log 2.) *. n *. log p /. 8.)
      (of_int bytes_max) |> to_int
  in
  let nb_funcs =
    let open Float in
    min
      (of_int filter_len_bytes *. 8. /. n *. log 2.)
      (of_int funcs_max) |>
    to_int
  in
  { filter = Bitv.create (filter_len_bytes * 8) false ;
    len = filter_len_bytes ;
    nb_funcs ;
    tweak }

let reset t =
  { t with filter = Bitv.(create (length t.filter) false) }

let hash { filter ; tweak ; len; _ } data func_id =
  let res = Cstruct.create 4 in
  let seed = Int32.(add (mul (of_int func_id) seed_mult) tweak) in
  Murmur3.Murmur_cstruct.murmur_x86_32 res data seed ;
  let open Stdint in
  let res = Uint32.of_int32 (Cstruct.LE.get_uint32 res 0) in
  let filter_size = Uint32.of_int (len * 8) in
  let i = Uint32.(rem res filter_size |> to_int) in
  Bitv.set filter i true

let add ({ nb_funcs; _ } as t) data =
  for i = 0 to nb_funcs - 1 do
    hash t data i
  done

let mem t data =
  let empty = reset t in
  add empty data ;
  let bitv_and = Bitv.bw_and empty.filter t.filter in
  Stdlib.(=) bitv_and empty.filter

let _ =
  let data_hex =
    `Hex "019f5b01d4195ecbc9398fbf3c3b1fa9bb3183301d7a1fb3bd174fcfa40a2b65" in
  let data = Hex.to_string data_hex |> Cstruct.of_string in
  let bloom = create 1 0.0001 0l in
  add bloom data ;
  let filter = to_filter bloom in
  let filter2 = of_filter filter bloom.nb_funcs bloom.tweak in
  let `Hex msg = Hex.of_string filter in
  Printf.printf "%s\n%!" msg ;
  assert (filter2 = bloom)
