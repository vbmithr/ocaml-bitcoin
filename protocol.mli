(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the GNU Affero GPL license, see LICENSE.
  ---------------------------------------------------------------------------*)

module Hash : sig
  type t = private Hash of string

  val of_string : string -> t
  val of_cstruct : Cstruct.t -> t

  val to_string : t -> string
end

module BlockHeader : sig
  type t = {
    version : Int32.t ;
    prev_block : Hash.t ;
    merkle_root : Hash.t ;
    timestamp : Ptime.t ;
    bits : Int32.t ;
    nonce : Int32.t ;
  }

  val of_cstruct : Cstruct.t -> t
end
