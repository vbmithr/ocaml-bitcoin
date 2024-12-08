(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the GNU Affero GPL license, see LICENSE.
  ---------------------------------------------------------------------------*)

open Util

module Network : sig
  type t =
    | Mainnet
    | Testnet
    | Regtest

  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val port : t -> int
  val seed : t -> string list
  val start_string : t -> string
  val max_nBits : t -> Int32.t
  val of_start_string : string -> t
end

module Service : sig
  type t =
    | Network
    | Getutxo
    | Bloom
end

module Version : sig
  type t =
    { version : int
    ; services : Service.t list
    ; timestamp : Timestamp.t
    ; recv_services : Service.t list
    ; recv_ipaddr : Ipaddr.V6.t
    ; recv_port : int
    ; trans_services : Service.t list
    ; trans_ipaddr : Ipaddr.V6.t
    ; trans_port : int
    ; nonce : Int64.t
    ; user_agent : string
    ; start_height : int
    ; relay : bool
    }

  val create
    :  ?version:int
    -> ?services:Service.t list
    -> ?timestamp:Timestamp.t
    -> ?recv_services:Service.t list
    -> ?recv_ipaddr:Ipaddr.V6.t
    -> recv_port:int
    -> ?trans_services:Service.t list
    -> ?trans_ipaddr:Ipaddr.V6.t
    -> trans_port:int
    -> ?nonce:Int64.t
    -> ?user_agent:string
    -> ?start_height:int
    -> ?relay:bool
    -> unit
    -> t
end

module Address : sig
  type t =
    { timestamp : Timestamp.t
    ; services : Service.t list
    ; ipaddr : Ipaddr.V6.t
    ; port : int
    }
end

module GetHashes : sig
  type t =
    { version : int
    ; hashes : Hash256.t list
    ; stop_hash : Hash256.t
    }

  val create : ?version:int -> ?stop_hash:Hash256.t -> Hash256.t list -> t
  val to_cstruct : Cstruct.t -> t -> Cstruct.t
end

module Inv : sig
  type id =
    | Tx
    | Block
    | FilteredBlock

  type t =
    { id : id
    ; hash : Hash256.t
    }
  [@@deriving sexp]

  val tx : Hash256.t -> t
  val block : Hash256.t -> t
  val filteredblock : Hash256.t -> t
end

module MerkleBlock : sig
  type t =
    { header : Header.t
    ; txn_count : int
    ; hashes : Hash256.t list
    ; flags : Bitv.t
    }
  [@@deriving sexp]
end

module FilterLoad : sig
  type flag =
    | Update_none
    | Update_all
    | Update_p2pkh_only

  type t =
    { filter : Bloom.t
    ; flag : flag
    }

  val of_data
    :  ?false_pos_rate:float
    -> ?tweak:Int32.t
    -> Cstruct.t list
    -> ?nb_elts:int
    -> flag
    -> t
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
    | SendCmpct

  val show : t -> string
  val of_string : string -> t
  val of_cstruct : Cstruct.t -> t
  val to_string : t -> string
end

module MessageHeader : sig
  type t =
    { network : Network.t
    ; msgname : MessageName.t
    ; size : int
    ; checksum : string
    }
  [@@deriving sexp]

  val size : int
  val of_cstruct : Cstruct.t -> t * Cstruct.t
end

module Reject : sig
  module Code : sig
    type t =
      | Decode_error
      | Invalid_block of Hash256.t
      | Invalid_transaction of Hash256.t
      | Block_version_too_old of Hash256.t
      | Protocol_too_old
      | Double_spend of Hash256.t
      | Too_many_version_messages
      | Non_standard_transaction of Hash256.t
      | Dust of Hash256.t
      | Fee_too_low of Hash256.t
      | Wrong_blockchain of Hash256.t
  end

  type t =
    { message : MessageName.t
    ; code : Code.t
    ; reason : string
    }

  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val of_cstruct : Cstruct.t -> t * Cstruct.t
end

module SendCmpct : sig
  type t =
    { compact : bool
    ; version : int
    }
  [@@deriving sexp]
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
    | GetData of Inv.t list
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
    | SendCmpct of SendCmpct.t
  [@@deriving sexp]

  type error = Invalid_checksum of MessageHeader.t

  val of_cstruct : Cstruct.t -> (MessageHeader.t * t, error) Rresult.result * Cstruct.t
  val to_cstruct : network:Network.t -> Cstruct.t -> t -> Cstruct.t
end
