(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the GNU Affero GPL license, see LICENSE.
  ---------------------------------------------------------------------------*)

module Cstruct = struct
  include Cstruct

  let to_c_string cs =
    let str = to_string cs in
    String.(sub str 0 (index str '\x00'))
end

module Timestamp = struct
  let of_int32 i =
    match Int32.to_float i |> Ptime.of_float_s with
    | None -> invalid_arg "Timestamp.of_int32"
    | Some ts -> ts
end

module Hash = struct
  type t = Hash of string

  let of_string s =
    if String.length s <> 32 then invalid_arg "Hash.of_string" else Hash s

  let to_string (Hash s) = s

  let of_cstruct cs =
    Hash (Cstruct.copy cs 0 32)
end

module CompactSize = struct
  type t =
    | Int of int
    | Int32 of Int32.t
    | Int64 of Int64.t

  open EndianString.LittleEndian

  let read ?(pos=0) buf =
    match get_uint8 buf pos with
    | 0xFD -> Int (get_uint16 buf (pos+1))
    | 0xFE -> Int32 (get_int32 buf (pos+1))
    | 0xFF -> Int64 (get_int64 buf (pos+1))
    | n -> Int n

  let write ?(pos=0) buf = function
    | Int n when n <= 0xFFFF -> set_int16 buf pos n
    | Int n -> set_int32 buf pos (Int32.of_int n)
    | Int32 n -> set_int32 buf pos n
    | Int64 n -> set_int64 buf pos n

  let of_cstruct cs =
    let open Cstruct in
    match get_uint8 cs 0 with
    | 0xFD -> Int (LE.get_uint16 cs 1), 3
    | 0xFE -> Int32 (LE.get_uint32 cs 1), 5
    | 0xFF -> Int64 (LE.get_uint64 cs 1), 9
    | n -> Int n, 1

  let of_cstruct_int cs =
    match of_cstruct cs with
    | Int i, n -> i, n
    | Int32 i, n -> Int32.to_int i, n
    | Int64 i, n -> Int64.to_int i, n
end
