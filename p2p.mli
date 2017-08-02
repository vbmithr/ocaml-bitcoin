(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the GNU Affero GPL license, see LICENSE.
  ---------------------------------------------------------------------------*)

open Util
open Protocol

module Service : sig
  type t =
    | Node_network
end

module Version : sig
  type t = {
    version : int ;
    services : Service.t list ;
    timestamp : Ptime.t ;
    recv_services : Service.t list ;
    recv_ipaddr : Ipaddr.V6.t ;
    recv_port : int ;
    trans_services : Service.t list ;
    trans_ipaddr : Ipaddr.V6.t ;
    trans_port : int ;
    nonce : Int64.t ;
    user_agent : string ;
    start_height : int ;
    relay : bool ;
  }
end

module Address : sig
  type t = {
    timestamp : Ptime.t ;
    services : Service.t list ;
    ipaddr : Ipaddr.V6.t ;
    port : int ;
  }
end

module GetObjects : sig
  type t = {
    version : int ;
    hashes : Hash.Set.t ;
    stop_hash : Hash.t ;
  }
end

module Inv : sig
  type id =
    | Tx
    | Block
    | FilteredBlock

  type t = {
    id : id ;
    hash : Hash.t ;
  }
end

module Message : sig
  type t =
    | Version of Version.t
    | VerAck

    | GetAddr
    | Addr of Address.t list

    | GetBlocks of GetObjects.t
    | GetData of GetObjects.t

    | Block of Block.t

    | Inv of Inv.t list

  val of_cstruct : Cstruct.t -> t * Cstruct.t
end
