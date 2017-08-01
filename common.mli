(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the GNU Affero GPL license, see LICENSE.
  ---------------------------------------------------------------------------*)

module CompactSize : sig
  type t =
    | Int of int
    | Int32 of Int32.t
    | Int64 of Int64.t
end
