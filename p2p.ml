(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the GNU Affero GPL license, see LICENSE.
  ---------------------------------------------------------------------------*)

module Network = struct
  type t =
    | Mainnet
    | Testnet
    | Regtest

  let port = function
    | Mainnet -> 8333
    | Testnet -> 18333
    | Regtest -> 18444

  let start_string = function
    | Mainnet -> 0xf9beb4d9l
    | Testnet -> 0x0b110907l
    | Regtest -> 0xfabfb5dal

  let max_nBits = function
    | Mainnet -> 0x1d00ffffl
    | Testnet -> 0x1d00ffffl
    | Regtest -> 0x207fffffl

  let of_start_string = function
    | 0xf9beb4d9l -> Mainnet
    | 0x0b110907l -> Testnet
    | 0xfabfb5dal -> Regtest
    | _ -> invalid_arg "Version.of_start_string"
end

module MessageName = struct
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

  let of_string = function
    | "block" -> Block
    | "getblocks" -> GetBlocks
    | "getdata" -> GetData
    | "getheaders" -> GetHeaders
    | "headers" -> Headers
    | "inv" -> Inv
    | "mempool" -> MemPool
    | "merkleblock" -> MerkleBlock
    | "notfound" -> NotFound
    | "tx" -> Tx
    | "addr" -> Addr
    | "alert" -> Alert
    | "feefilter" -> FeeFilter
    | "filteradd" -> FilterAdd
    | "filterclear" -> FilterClear
    | "filterload" -> FilterLoad
    | "getaddr" -> GetAddr
    | "ping" -> Ping
    | "pong" -> Pong
    | "reject" -> Reject
    | "verack" -> VerAck
    | "version" -> Version
    | _ -> invalid_arg "MessageName.of_string"

  let of_cstruct cs =
    let msg = Cstruct.to_string cs in
    of_string String.(sub msg 0 (index msg '\x00'))
end

module Header = struct
  module C = struct
    [%%cstruct type t = {
        start_string : uint32_t ;
        command_name : uint8_t [@len 12] ;
        payload_size : uint32_t ;
        checksum : uint32_t ;
      } [@@little_endian]]
  end

  type t = {
    network : Network.t ;
    msgname : MessageName.t ;
    size : int ;
    checksum : Int32.t ;
  }

  let of_cstruct cs =
    let open C in
    let network = get_t_start_string cs |> Network.of_start_string in
    let msgname = get_t_command_name cs |> MessageName.of_cstruct in
    let size = get_t_payload_size cs |> Int32.to_int in
    let checksum = get_t_checksum cs in
    { network ; msgname ; size ; checksum }
end

module Version = struct
  module C = struct
    module C = struct
      [%%cstruct type t = {
          version : uint32_t ;
          services : uint64_t ;
          timestamp : uint32_t ;
          recv_services : uint64_t ;
          recv_ipaddr : uint8_t [@len 16] ;
          recv_port : uint16_t ;
          trans_services : uint64_t ;
          trans_ipaddr : uint8_t [@len 16] ;
          trans_port : uint16_t ;
          nonce : uint64_t ;
        } [@@little_endian]]
    end
  end
end

