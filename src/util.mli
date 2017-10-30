(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the GNU Affero GPL license, see LICENSE.
  ---------------------------------------------------------------------------*)

open Base
module Format = Caml.Format

val c_string_of_cstruct : Cstruct.t -> string
val bytes_with_msg : len:int -> string -> String.t

module Bool : sig
  val of_int : int -> bool
  val to_int : bool -> int
end

module Timestamp : sig
  include module type of Ptime

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t

  val of_int_sec : int -> t
  val to_int_sec : t -> int
  val of_int64_sec : Int64.t -> t
  val to_int64_sec : t -> Int64.t
  val of_int32_sec : Int32.t -> t
  val to_int32_sec : t -> Int32.t

  val now : unit -> t
end

module Hash256 : sig
  type t = private Hash of string [@@deriving sexp]
  include Comparable.S with type t := t
  val hash : t -> int

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

module Chksum : sig
  val compute : Cstruct.t -> string
  val compute' : Cstruct.t -> Cstruct.t -> int * string
  val verify : expected:string -> Cstruct.t -> bool

  exception Invalid_checksum of string * string

  val verify_exn : expected:string -> Cstruct.t -> unit
    (** @raises Invalid_checksum on error. *)
end

module CompactSize : sig
  type t =
    | Int of int
    | Int32 of Int32.t
    | Int64 of Int64.t

  val size : t -> int

  val of_cstruct : Cstruct.t -> t * Cstruct.t
  val of_cstruct_int : Cstruct.t -> int * Cstruct.t
  val to_cstruct : Cstruct.t -> t -> Cstruct.t
  val to_cstruct_int : Cstruct.t -> int -> Cstruct.t
end

module VarString : sig
  val of_cstruct : Cstruct.t -> string * Cstruct.t
  val to_cstruct : Cstruct.t -> string -> Cstruct.t
end

module ObjList : sig
  val size : 'a list -> f:('a -> int) -> int

  val of_cstruct :
    Cstruct.t -> f:(Cstruct.t -> 'a * Cstruct.t) -> 'a list * Cstruct.t

  val to_cstruct :
    Cstruct.t -> 'a list -> f:(Cstruct.t -> 'a -> Cstruct.t) -> Cstruct.t
end

module Bitv : sig
  include module type of Bitv with type t = Bitv.t

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t

  val to_string_le : t -> string
  val of_string_le : string -> t

  val to_bool_list : t -> bool list
end

module KeyPath : sig
  type derivation = N of Int32.t | H of Int32.t
  type t = derivation list

  val of_string : string -> t

  val write_be : Caml.Bytes.t -> int -> t -> int
  val write_be_cstruct : Cstruct.t -> t -> Cstruct.t
end
