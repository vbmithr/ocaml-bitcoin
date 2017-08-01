(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the GNU Affero GPL license, see LICENSE.
  ---------------------------------------------------------------------------*)

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
end
