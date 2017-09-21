open Base
open Murmur3.Murmur_cstruct

let bytes_max = 36000
let funcs_max = 50
let seed_mult = 0xfba4c795l

module Bitv = struct
  open Sexplib.Std
  include Bitv

  let t_of_sexp sexp =
    string_of_sexp sexp |>
    Bitv.L.of_string

  let sexp_of_t t =
    Bitv.L.to_string t |>
    sexp_of_string

  let to_string_le bitv =
    let nb_bytes = Bitv.length bitv / 8 in
    let s = String.create nb_bytes in
    let v = ref 0 in
    for i = 0 to nb_bytes - 1 do
      v := 0 ;
      for j = 0 to 7 do
        if Bitv.get bitv (8 * i + j) then
          v := !v lor (1 lsl j)
      done ;
      EndianString.BigEndian.set_int8 s i !v
    done ;
    s

  let of_string_le s =
    let len = String.length s in
    let bitv = Bitv.create (len * 8) false in
    for i = 0 to len - 1 do
      let v = EndianString.BigEndian.get_int8 s i in
      for j = 0 to 7 do
        if v land (1 lsl j) <> 0 then Bitv.set bitv (8 * i + j) true
      done
    done ;
    bitv
end

type t = {
  filter : Bitv.t ;
  len : int ;
  nb_funcs : int ;
  tweak : Int32.t ;
} [@@deriving sexp]

let filter_len { filter } =
  Bitv.length filter / 8

let to_filter { filter } =
  try Bitv.to_string_le filter with _ ->
    invalid_arg "Bloom.to_string"

let pp ppf t =
  let `Hex filter_hex = Hex.of_string (to_filter t) in
  Caml.Format.fprintf ppf "%s" filter_hex

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

let hash { filter ; tweak ; len } data func_id =
  let res = Cstruct.create 4 in
  let seed = Int32.(of_int_exn func_id * seed_mult + tweak) in
  murmur_x86_32 res data seed ;
  let open Stdint in
  let res = Uint32.of_int32 (Cstruct.LE.get_uint32 res 0) in
  let filter_size = Uint32.of_int (len * 8) in
  let i = Uint32.(rem res filter_size |> to_int) in
  Bitv.set filter i true

let add ({ nb_funcs } as t) data =
  for i = 0 to nb_funcs - 1 do
    hash t data i
  done

let mem t data =
  let empty = reset t in
  let open Int32 in
  add empty data ;
  let bitv_and = Bitv.bw_and empty.filter t.filter in
  Caml.Pervasives.(=) bitv_and empty.filter

let _ =
  let data_hex =
    `Hex "019f5b01d4195ecbc9398fbf3c3b1fa9bb3183301d7a1fb3bd174fcfa40a2b65" in
  let data = Hex.to_string data_hex |> Cstruct.of_string in
  let bloom = create 1 0.0001 0l in
  add bloom data ;
  let filter = to_filter bloom in
  let filter2 = of_filter filter bloom.nb_funcs bloom.tweak in
  let `Hex msg = Hex.of_string filter in
  Caml.Printf.printf "%s\n%!" msg ;
  assert Caml.Pervasives.(filter2 = bloom)
