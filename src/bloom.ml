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
        if v land (1 lsl j) <> 0 then Bitv.set bitv (8*i + j) true
      done
    done ;
    bitv
end

type t = {
  filter : Bitv.t ;
  nb_funcs : int ;
  tweak : Int32.t ;
} [@@deriving sexp]

let filter_len { filter } =
  Bitv.length filter / 8

let to_filter { filter } =
  try Bitv.to_string_le filter with _ ->
    invalid_arg "Bloom.to_string"

let of_filter filter nb_funcs tweak =
  let filter_len = String.length filter in
  if filter_len > bytes_max ||
     nb_funcs > funcs_max then
    invalid_arg "Bloom.of_fliter" ;
  { filter = Bitv.of_string_le filter ; nb_funcs ; tweak }

let create n p tweak =
  let open Float in
  let n = of_int n in
  let filter_len_bytes =
    min
      (-1. /. (log 2. *. log 2.) *. n *. log p /. 8.)
      (of_int bytes_max)
  in
  let nb_funcs = to_int (filter_len_bytes *. 8. /. n *. log 2.) in
  { filter = Bitv.create (to_int (filter_len_bytes *. 8.)) false ;
    nb_funcs ;
    tweak }

let reset t =
  { t with filter = Bitv.(create (length t.filter) false) }

let hash { filter ; tweak } data func_id =
  let open Int32 in
  let res = Cstruct.create 4 in
  let seed = of_int_exn func_id * seed_mult + tweak in
  murmur_x86_32 res data seed ;
  let open Stdint.Uint32 in
  let res = of_int32 (Cstruct.LE.get_uint32 res 0) in
  let filter_size = of_int (Bitv.length filter) in
  let i = rem res filter_size |> to_int in
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
