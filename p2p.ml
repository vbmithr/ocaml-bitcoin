(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the GNU Affero GPL license, see LICENSE.
  ---------------------------------------------------------------------------*)

open Util

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
    Cstruct.to_c_string cs |> of_string
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

  module Service = struct
    type t =
      | Node_network

    let of_int64 = function
      | 0L -> []
      | 1L -> [Node_network]
      | _ -> invalid_arg "Service.of_int64"
  end

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

  let of_cstruct cs =
    let open C in
    let version = get_t_version cs |> Int32.to_int in
    let services = get_t_services cs |> Service.of_int64 in
    let timestamp = get_t_timestamp cs |> Timestamp.of_int32 in
    let recv_services = get_t_recv_services cs |> Service.of_int64 in
    let recv_ipaddr = get_t_recv_ipaddr cs |> Cstruct.to_string |> Ipaddr.V6.of_bytes_exn in
    let recv_port = get_t_recv_port cs in
    let trans_services = get_t_trans_services cs |> Service.of_int64 in
    let trans_ipaddr = get_t_trans_ipaddr cs |> Cstruct.to_string |> Ipaddr.V6.of_bytes_exn in
    let trans_port = get_t_trans_port cs in
    let nonce = get_t_nonce cs in
    let cs = Cstruct.shift cs sizeof_t in
    let user_agent_size, nb_read = CompactSize.of_cstruct_int cs in
    let user_agent =
      match user_agent_size with
      | 0 -> ""
      | _ -> Cstruct.(sub cs nb_read user_agent_size |> to_c_string) in
    let cs = Cstruct.shift cs nb_read in
    let start_height = Cstruct.LE.get_uint32 cs 0 |> Int32.to_int in
    let relay =
      match Cstruct.get_uint8 cs 4 with
      | exception _ -> true
      | 0x01 -> true
      | 0x00 -> false
      | _ -> invalid_arg "Version.of_cstruct: unsupported value for relay field" in
    { version ; services ; timestamp ; recv_services ; recv_ipaddr ; recv_port ;
      trans_services ; trans_ipaddr ; trans_port ; nonce ; user_agent ; start_height ;
      relay }
end

