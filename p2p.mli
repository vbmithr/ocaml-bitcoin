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

module GetHashes : sig
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

module MerkleBlock : sig
  type t = {
    header : Header.t ;
    txn_count : int ;
    hashes : Hash.Set.t ;
    flags : string ;
  }
end

module FilterLoad : sig
  type flag =
    | Update_none
    | Update_all
    | Update_p2pkh_only

  type t = {
    filter : string ;
    nb_hash_funcs : int ;
    tweak : Int32.t ;
    flag : flag ;
  }
end

module MessageName : sig
  type t =
    | Block
    | GetBlocks
    | GetData
    | GetHeaders
    | Headers
    | Inv
    | MemPool
    | MerkleBlock
    | NotFound
    | Tx
    | Addr
    | Alert
    | FeeFilter
    | FilterAdd
    | FilterClear
    | FilterLoad
    | GetAddr
    | Ping
    | Pong
    | Reject
    | SendHeaders
    | VerAck
    | Version
end

module Reject : sig
    type code =
    | Decode_error
    | Invalid_block of Hash.t
    | Invalid_transaction of Hash.t
    | Block_version_too_old of Hash.t
    | Protocol_too_old
    | Double_spend of Hash.t
    | Too_many_version_messages
    | Non_standard_transaction of Hash.t
    | Dust of Hash.t
    | Fee_too_low of Hash.t
    | Wrong_blockchain of Hash.t

  type t = {
    rejected_message : MessageName.t ;
    code : code ;
    reason : string ;
  }

  val of_cstruct : Cstruct.t -> t * Cstruct.t
end

module Message : sig
  type t =
    | Version of Version.t
    | VerAck

    | GetAddr
    | Addr of Address.t list

    | Ping of Int64.t
    | Pong of Int64.t

    | GetBlocks of GetHashes.t
    | GetData of GetHashes.t
    | GetHeaders of GetHashes.t

    | Block of Block.t
    | MerkleBlock of MerkleBlock.t
    | Headers of Header.t list

    | Inv of Inv.t list
    | NotFound of Inv.t list
    | MemPool
    | SendHeaders

    | Tx of Transaction.t
    | FeeFilter of Int64.t

    | FilterAdd of string
    | FilterClear
    | FilterLoad of FilterLoad.t

    | Reject of Reject.t

  val of_cstruct : Cstruct.t -> t * Cstruct.t
end
