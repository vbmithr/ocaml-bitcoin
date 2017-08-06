open Util

type t = ()

let of_cstruct cs =
  let len, cs = CompactSize.of_cstruct_int cs in
  (), Cstruct.shift cs len
