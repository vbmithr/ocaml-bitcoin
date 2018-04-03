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
  let buf = Bytes.make len '\x00' in
  Bytes.From_string.(blit msg 0 buf 0 (Int.min (Bytes.length buf - 1) (String.length msg)));
  Bytes.unsafe_to_string buf

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

  let of_int_sec s =
    match Span.of_int_s s |> of_span with
    | None -> invalid_arg "Timestamp.of_int_sec"
    | Some t -> t

  let to_int_sec t =
    match Span.to_int_s (to_span t) with
    | None -> invalid_arg "Timestamp.to_int_sec"
    | Some s -> s

  let of_int32_sec s =
    of_int_sec (Int32.to_int_exn s)

  let to_int32_sec s=
    Int32.of_int_exn (to_int_sec s)

  let of_int64_sec s =
    of_int_sec (Int64.to_int_exn s)

  let to_int64_sec s =
    Int64.of_int_exn (to_int_sec s)

  (* let of_int64 i = *)
  (* let to_int64 t = Int64.of_float (Ptime.to_float_s t) *)

  let of_int32 i =
    match Int32.to_float i |> Ptime.of_float_s with
    | None -> invalid_arg "Timestamp.of_int64"
    | Some ts -> ts

  let to_int32 t = Int32.of_float (Ptime.to_float_s t)

  include Ptime_clock
end

module Hash (H2 : Digestif.S) (H1 : Digestif.S) = struct
  module T = struct
    type t = Hash of string

    let hash (Hash s) = String.hash s
    let compare (Hash a) (Hash b) = String.compare a b

    let length = H2.digest_size

    let of_string s =
      if String.length s <> length then
        invalid_arg (Printf.sprintf "Hash.of_string: length must be %d" length)
      else Hash s

    let empty = of_string (String.make length '\x00')
    let of_hex_internal h = of_string (Hex.to_string h)

    let of_hex_rpc h =
      Hex.to_string h |> String.rev |> of_string

    let to_cstruct cs (Hash s) =
      Cstruct.blit_from_string s 0 cs 0 length ;
      Cstruct.shift cs length

    let to_string (Hash s) = s

    let pp ppf (Hash s) =
      let `Hex s_hex = Hex.of_string (String.rev s) in
      Format.fprintf ppf "%s" s_hex

    let show t =
      Format.asprintf "%a" pp t

    let sexp_of_t t =
      Sexplib.Std.sexp_of_string (show t)

    let t_of_sexp sexp =
      of_hex_rpc (`Hex (Sexplib.Std.string_of_sexp sexp))

    let of_cstruct cs =
      Hash (Cstruct.copy cs 0 length),
      Cstruct.shift cs length

    let compute_bigarray data =
      Hash Cstruct.((H2.Bigstring.digest (H1.Bigstring.digest data))
                    |> of_bigarray |> to_string)

    let compute_cstruct cs =
      compute_bigarray (Cstruct.to_bigarray cs)

    let compute_string data =
      let data = Bytes.unsafe_of_string_promise_no_mutation data in
      Hash (Bytes.unsafe_to_string (H2.Bytes.digest (H1.Bytes.digest data)))

    let compute_concat (Hash h1) (Hash h2) =
      compute_string (h1 ^ h2)
  end
  include T
  include Comparable.Make(T)
end

module type HASH = sig
  type t = private Hash of string [@@deriving sexp]
  include Comparable.S with type t := t
  val hash : t -> int
  val length : int

  val empty : t
  val of_hex_internal : Hex.t -> t
  val of_hex_rpc : Hex.t -> t

  val pp : Format.formatter -> t -> unit
  val show : t -> string

  val compute_bigarray : Cstruct.buffer -> t
  val compute_cstruct : Cstruct.t -> t
  val compute_string : string -> t
  val compute_concat : t -> t -> t

  val of_string : string -> t
  val of_cstruct : Cstruct.t -> t * Cstruct.t

  val to_string : t -> string
  val to_cstruct : Cstruct.t -> t -> Cstruct.t
end

module Hash160 : HASH = Hash (Digestif.RMD160) (Digestif.SHA256)
module Hash256 : HASH = Hash (Digestif.SHA256) (Digestif.SHA256)

module Chksum = struct
  let compute cs =
    let open Cstruct in
    let data = to_bigarray cs in
    Digestif.SHA256.Bigstring.(digest (digest data)) |>
    of_bigarray |> fun cs -> sub cs 0 4 |> to_string

  let compute' cs_start cs_end =
    let size = cs_end.Cstruct.off - cs_start.Cstruct.off in
    size, compute (Cstruct.sub cs_start 0 size)

  let verify ~expected data =
    String.equal expected (compute data)

  exception Invalid_checksum of string * string

  let verify_exn ~expected data =
    let computed = compute data in
    if not (String.equal expected computed) then
      raise (Invalid_checksum (expected, computed))
end

module CompactSize = struct
  type t =
    | Int of int
    | Int32 of Int32.t
    | Int64 of Int64.t

  let of_int i = Int i
  let of_int32 i = Int32 i
  let of_int64 i = Int64 i

  let size = function
    | Int n when n < 0xFD -> 1
    | Int n when n < 0x10000 -> 3
    | Int n -> 5
    | Int32 n -> 5
    | Int64 n -> 9

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
  let size elts ~f =
    List.fold_left elts
      ~init:(CompactSize.size (Int (List.length elts)))
      ~f:(fun a e -> a + f e)

  let rec inner obj_of_cstruct acc cs = function
    | 0 -> List.rev acc, cs
    | n ->
      let obj, cs = obj_of_cstruct cs in
      inner obj_of_cstruct (obj :: acc) cs (Caml.pred n)

  let of_cstruct cs ~f =
    let nb_objs, cs = CompactSize.of_cstruct_int cs in
    inner f [] cs nb_objs

  let to_cstruct cs objs ~f =
    let len = List.length objs in
    let cs = CompactSize.to_cstruct_int cs len in
    Base.List.fold_left objs ~init:cs ~f:begin fun cs o ->
      f cs o
    end
end

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
    let s = Bytes.create nb_bytes in
    let v = ref 0 in
    for i = 0 to nb_bytes - 1 do
      v := 0 ;
      for j = 0 to 7 do
        if Bitv.get bitv (8 * i + j) then
          v := !v lor (1 lsl j)
      done ;
      EndianString.BigEndian.set_int8 s i !v
    done ;
    Bytes.unsafe_to_string s

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

  let to_bool_list bv =
    Bitv.fold_right (fun v acc -> v :: acc) bv []
end

module Crypto = struct
  let sha256 s =
    Bytes.unsafe_of_string_promise_no_mutation s |>
    Digestif.SHA256.Bytes.digest |> fun b ->
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:b
end

let c = (module Crypto : Base58.CRYPTO)
