(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the GNU Affero GPL license, see LICENSE.
  ---------------------------------------------------------------------------*)

module Cstruct : sig
  include module type of Cstruct
  val to_c_string : t -> string
end

module Timestamp : sig
  val of_int32 : Int32.t -> Ptime.t
end

module Hash : sig
  type t = private Hash of string

  val of_string : string -> t
  val of_cstruct : Cstruct.t -> t

  val to_string : t -> string
end

module CompactSize : sig
  type t =
    | Int of int
    | Int32 of Int32.t
    | Int64 of Int64.t

  val of_cstruct : Cstruct.t -> t * int
  (** [of_cstruct cs] is (t, nb_read) where [nb_read] is the number of
      bytes read in [cs]. *)

  val of_cstruct_int : Cstruct.t -> int * int
  (** [of_cstruct_int cs] is (v, nb_read] where [v] is the integer
      encoded in CompactSize format, and [nb_read] is the number of
      bytes read in [cs]. *)
end
