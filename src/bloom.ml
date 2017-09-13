open Base
open Murmur3.Murmur_cstruct

let bytes_max = 36000.
let funcs_max = 50.
let seed_mult = 0xfba4c795l

type t = {
  bitv : Bitv.t ;
  nb_funcs : int ;
  tweak : Int32.t ;
}

let create ?(tweak=0l) n p =
  let open Float in
  let n = of_int n in
  let filter_len =
    min (-1. /. (log 2.) *. (log 2.) *. n *. log p /. 8.)
      bytes_max in
  let nb_funcs = filter_len *. 8. /. n *. log 2. in
  let filter_len = to_int filter_len in
  let nb_funcs = to_int nb_funcs in
  { bitv = Bitv.create filter_len false ;
    nb_funcs ;
    tweak }

let reset t =
  { t with bitv = Bitv.(create (length t.bitv) false) }

let hash { bitv ; tweak } data func_id =
  let open Int32 in
  let res = Cstruct.create 4 in
  let seed = of_int_exn func_id * seed_mult + tweak in
  murmur_x86_32 res data seed ;
  let i = Cstruct.LE.get_uint32 res 0 % (of_int_exn (Bitv.length bitv)) |> to_int_exn in
  Bitv.set bitv i true

let add ({ nb_funcs } as t) data =
  for i = 0 to nb_funcs - 1 do
    hash t data i
  done

let mem t data =
  let empty = reset t in
  let open Int32 in
  add empty data ;
  let bitv_and = Bitv.bw_and empty.bitv t.bitv in
  Caml.Pervasives.(=) bitv_and empty.bitv
