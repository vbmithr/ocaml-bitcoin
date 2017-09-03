(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the GNU Affero GPL license, see LICENSE.
  ---------------------------------------------------------------------------*)

open Base
module Format = Caml.Format

let c_string_of_cstruct cs =
  let str = Cstruct.to_string cs in
  String.(sub str 0 (index_exn str '\x00'))

let bytes_with_msg ~len msg =
  let buf = String.make len '\x00' in
  String.(blit msg 0 buf 0 (Int.min (length buf - 1) (length msg)));
  buf

module Bool = struct
  let of_int = function
    | 1 -> true
    | 0 -> false
    | _ -> invalid_arg "Bool.of_int"

  let to_int = function
    | false -> 0
    | true -> 1
end

module Timestamp = struct
  include Ptime

  let t_of_sexp sexp =
    let open Sexplib.Std in
    let sexp_str = string_of_sexp sexp in
    match of_rfc3339 sexp_str with
    | Ok (t, _, _) -> t
    | _ -> invalid_arg "Timestamp.t_of_sexp"

  let sexp_of_t t =
    let open Sexplib.Std in
    sexp_of_string (to_rfc3339 t)

  let of_int64 i =
    match Int64.to_float i |> Ptime.of_float_s with
    | None -> invalid_arg "Timestamp.of_int64"
    | Some ts -> ts

  let to_int64 t = Int64.of_float (Ptime.to_float_s t)

  let of_int32 i =
    match Int32.to_float i |> Ptime.of_float_s with
    | None -> invalid_arg "Timestamp.of_int64"
    | Some ts -> ts

  let to_int32 t = Int32.of_float (Ptime.to_float_s t)

  include Ptime_clock
end

module Hash = struct
  module T = struct
    type t = Hash of string [@@deriving sexp]

    let compare (Hash a) (Hash b) = String.compare a b

    include (val Comparator.make ~compare ~sexp_of_t)

    let of_string s =
      if String.length s <> 32 then invalid_arg "Hash.of_string" else Hash s

    let to_string (Hash s) = s

    let pp ppf (Hash s) = Format.fprintf ppf "%s" s
    let show = to_string

    let of_cstruct cs =
      Hash (Cstruct.copy cs 0 32),
      Cstruct.shift cs 32
  end
  include T
  module HashSet = Set.M(T)
  module HashMap = Map.M(T)

  type set = HashSet.t
  let set_of_sexp sexp = Set.m__t_of_sexp (module T) sexp
  let sexp_of_set set = Set.sexp_of_m__t (module T) set
  type 'a map = 'a HashMap.t
end

module Chksum = struct
  let compute cs =
    let data = Cstruct.to_bigarray cs in
    Digestif.(Bi.(sub SHA256.Bigstring.(digest (digest data)) 0 4 |> to_string))

  let compute' cs_start cs_end =
    let size = cs_end.Cstruct.off - cs_start.Cstruct.off in
    size, compute (Cstruct.sub cs_start 0 size)

  let verify ~expected data =
    String.equal expected (compute data)

  exception Invalid_checksum

  let verify_exn ~expected data =
    if not (String.equal expected (compute data)) then raise Invalid_checksum
end

module CompactSize = struct
  type t =
    | Int of int
    | Int32 of Int32.t
    | Int64 of Int64.t

  let read ?(pos=0) buf =
    let open EndianString.LittleEndian in
    match get_uint8 buf pos with
    | 0xFD -> Int (get_uint16 buf (pos+1))
    | 0xFE -> Int32 (get_int32 buf (pos+1))
    | 0xFF -> Int64 (get_int64 buf (pos+1))
    | n -> Int n

  let write ?(pos=0) buf t =
    let open EndianString.LittleEndian in
    match t with
    | Int n when n < 0xFD -> set_int8 buf pos n
    | Int n when n < 0x10000 ->
      set_int8 buf pos 0xFD ;
      set_int16 buf (pos+1) n
    | Int n ->
      set_int8 buf pos 0xFE ;
      set_int32 buf (pos+1) (Int32.of_int_exn n)
    | Int32 n ->
      set_int8 buf pos 0xFE ;
      set_int32 buf (pos+1) n
    | Int64 n ->
      set_int8 buf pos 0xFF ;
      set_int64 buf (pos+1) n

  let of_cstruct cs =
    let open Cstruct in
    match get_uint8 cs 0 with
    | 0xFD -> Int (LE.get_uint16 cs 1), shift cs 3
    | 0xFE -> Int32 (LE.get_uint32 cs 1), shift cs 5
    | 0xFF -> Int64 (LE.get_uint64 cs 1), shift cs 9
    | n -> Int n, shift cs 1

  let of_cstruct_int cs =
    match of_cstruct cs with
    | Int i, cs -> i, cs
    | Int32 i, cs -> Int32.to_int_exn i, cs
    | Int64 i, cs -> Int64.to_int_exn i, cs

  let to_cstruct cs t =
    let open Cstruct in
    match t with
    | Int n when n < 0xFD ->
      set_uint8 cs 0 n ;
      shift cs 1
    | Int n when n < 0x10000 ->
      set_uint8 cs 0 0xFD ;
      LE.set_uint16 cs 1 n ;
      shift cs 3
    | Int n ->
      set_uint8 cs 0 0xFE ;
      LE.set_uint32 cs 1 (Int32.of_int_exn n) ;
      shift cs 5
    | Int32 n ->
      set_uint8 cs 0 0xFE ;
      LE.set_uint32 cs 1 n ;
      shift cs 5
    | Int64 n ->
      set_uint8 cs 0 0xFF ;
      LE.set_uint64 cs 1 n ;
      shift cs 9

  let to_cstruct_int cs i = to_cstruct cs (Int i)
end

module VarString = struct
  let of_cstruct cs =
    let length, cs = CompactSize.of_cstruct_int cs in
    Cstruct.(sub cs 0 length |> to_string, shift cs length)

  let to_cstruct cs s =
    let len = String.length s in
    let cs = CompactSize.to_cstruct_int cs len in
    Cstruct.blit_from_string s 0 cs 0 len ;
    Cstruct.shift cs len
end

module ObjList = struct
  let rec inner obj_of_cstruct acc cs = function
    | 0 -> List.rev acc, cs
    | n ->
      let obj, cs = obj_of_cstruct cs in
      inner obj_of_cstruct (obj :: acc) cs (Caml.pred n)

  let of_cstruct cs ~f =
    let nb_addrs, cs = CompactSize.of_cstruct_int cs in
    inner f [] cs nb_addrs

  let to_cstruct cs objs ~f =
    let len = List.length objs in
    let cs = CompactSize.to_cstruct_int cs len in
    Base.List.fold_left objs ~init:cs ~f:begin fun cs o ->
      f cs o
    end
end
