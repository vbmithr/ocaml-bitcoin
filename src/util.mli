(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the GNU Affero GPL license, see LICENSE.
  ---------------------------------------------------------------------------*)

val c_string_of_cstruct : Cstruct.t -> string
val bytes_with_msg : len:int -> string -> Bytes.t

module Timestamp : sig
  val of_int32 : Int32.t -> Ptime.t
  val to_int32 : Ptime.t -> Int32.t
end

module Hash : sig
  type t = private Hash of string

  val of_string : string -> t
  val of_cstruct : Cstruct.t -> t * Cstruct.t

  val to_string : t -> string

  module Set : Set.S with type elt = t
  module Map : Map.S with type key = t
end

module Chksum : sig
  val compute : Cstruct.t -> string
  val compute' : Cstruct.t -> Cstruct.t -> int * string
  val verify : expected:string -> Cstruct.t -> bool

  exception Invalid_checksum

  val verify_exn : expected:string -> Cstruct.t -> unit
    (** @raises Invalid_checksum on error. *)
end

module CompactSize : sig
  type t =
    | Int of int
    | Int32 of Int32.t
    | Int64 of Int64.t

  val of_cstruct : Cstruct.t -> t * Cstruct.t
  val of_cstruct_int : Cstruct.t -> int * Cstruct.t
  val to_cstruct : Cstruct.t -> t -> Cstruct.t
  val to_cstruct_int : Cstruct.t -> int -> Cstruct.t
end

module ObjList : sig
  val of_cstruct :
    f:(Cstruct.t -> 'a * Cstruct.t) -> Cstruct.t -> 'a list * Cstruct.t
end
