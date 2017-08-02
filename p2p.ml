(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the GNU Affero GPL license, see LICENSE.
  ---------------------------------------------------------------------------*)

open Util
open Protocol

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
    c_string_of_cstruct cs |> of_string
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
    { network ; msgname ; size ; checksum }, Cstruct.shift cs sizeof_t
end

module Service = struct
  type t =
    | Node_network

  let of_int64 = function
    | 0L -> []
    | 1L -> [Node_network]
    | _ -> invalid_arg "Service.of_int64"
end

module Version = struct
  module C = struct
    [%%cstruct type t = {
        version : uint32_t ;
        services : uint64_t ;
        timestamp : uint32_t ;
        recv_services : uint64_t ;
        recv_ipaddr : uint8_t [@len 16] ;
        recv_port : uint8_t [@len 2];
        trans_services : uint64_t ;
        trans_ipaddr : uint8_t [@len 16] ;
        trans_port : uint8_t [@len 2] ;
        nonce : uint64_t ;
      } [@@little_endian]]
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
    let recv_port = Cstruct.BE.get_uint16 (get_t_recv_port cs) 0 in
    let trans_services = get_t_trans_services cs |> Service.of_int64 in
    let trans_ipaddr = get_t_trans_ipaddr cs |> Cstruct.to_string |> Ipaddr.V6.of_bytes_exn in
    let trans_port = Cstruct.BE.get_uint16 (get_t_trans_port cs) 0 in
    let nonce = get_t_nonce cs in
    let cs = Cstruct.shift cs sizeof_t in
    let user_agent_size, cs = CompactSize.of_cstruct_int cs in
    let user_agent =
      match user_agent_size with
      | 0 -> ""
      | _ -> Cstruct.(sub cs 0 user_agent_size |> c_string_of_cstruct) in
    let cs = Cstruct.shift cs user_agent_size in
    let start_height = Cstruct.LE.get_uint32 cs 0 |> Int32.to_int in
    let relay =
      match Cstruct.get_uint8 cs 4 with
      | exception _ -> true
      | 0x01 -> true
      | 0x00 -> false
      | _ -> invalid_arg "Version.of_cstruct: unsupported value for relay field" in
    { version ; services ; timestamp ; recv_services ; recv_ipaddr ; recv_port ;
      trans_services ; trans_ipaddr ; trans_port ; nonce ; user_agent ; start_height ;
      relay },
    Cstruct.shift cs 5
end

module Address = struct
  module C = struct
    [%%cstruct type t = {
        timestamp : uint32_t ;
        services : uint64_t ;
        ipaddr : uint8_t [@len 16] ;
        port : uint8_t [@len 2];
      } [@@little_endian]]
  end

  type t = {
    timestamp : Ptime.t ;
    services : Service.t list ;
    ipaddr : Ipaddr.V6.t ;
    port : int ;
  }

  let of_cstruct cs =
    let open C in
    let timestamp = get_t_timestamp cs |> Timestamp.of_int32 in
    let services = get_t_services cs |> Service.of_int64 in
    let ipaddr = get_t_ipaddr cs |> Cstruct.to_string |> Ipaddr.V6.of_bytes_exn in
    let port = Cstruct.BE.get_uint16 (get_t_port cs) 0 in
    { timestamp ; services ; ipaddr ; port }, Cstruct.shift cs sizeof_t
end

module GetObjects = struct
  type t = {
    version : int ;
    hashes : Hash.Set.t ;
    stop_hash : Hash.t ;
  }

  let rec read_hash acc cs = function
    | 0 -> acc, cs
    | n ->
      let h, cs = Hash.of_cstruct cs in
      read_hash (Hash.Set.add h acc) cs (pred n)

  let of_cstruct cs =
    let open Cstruct in
    let version = LE.get_uint32 cs 0 |> Int32.to_int in
    let cs = shift cs 4 in
    let nb_hashes, cs = CompactSize.of_cstruct_int cs in
    let hashes, cs = read_hash Hash.Set.empty cs nb_hashes in
    let stop_hash, cs = Hash.of_cstruct cs in
    { version ; hashes ; stop_hash }, cs
end

module Inv = struct
  module C = struct
    [%%cstruct type t = {
        id : uint32_t ;
        hash : uint8_t [@len 32] ;
      } [@@little_endian]]
  end

  type id =
    | Tx
    | Block
    | FilteredBlock

  let id_of_int32 = function
    | 1l -> Tx
    | 2l -> Block
    | 3l -> FilteredBlock
    | _ -> invalid_arg "Inv.id_of_int32"

  type t = {
    id : id ;
    hash : Hash.t ;
  }

  let of_cstruct cs =
    let open C in
    let id = get_t_id cs |> id_of_int32 in
    let hash, _ = get_t_hash cs |> Hash.of_cstruct in
    { id ; hash }, Cstruct.shift cs sizeof_t
end

module Message = struct
  type t =
    | Version of Version.t
    | VerAck

    | GetAddr
    | Addr of Address.t list

    | GetBlocks of GetObjects.t
    | GetData of GetObjects.t

    | Block of Block.t

    | Inv of Inv.t list

  let of_cstruct cs =
    let h, cs = Header.of_cstruct cs in
    let payload = Cstruct.sub cs 0 h.size in
    Chksum.verify_exn ~expected:h.checksum payload ;
    match h.msgname with
    | Version ->
      let version, cs = Version.of_cstruct payload in
      Version version, cs
    | VerAck -> VerAck, cs
    | GetAddr -> GetAddr, cs
    | Addr ->
      let addrs, cs = ObjList.of_cstruct ~f:Address.of_cstruct payload in
      Addr addrs, cs
    | GetBlocks ->
      let objs, cs = GetObjects.of_cstruct payload in
      GetBlocks objs, cs
    | GetData ->
      let objs, cs = GetObjects.of_cstruct payload in
      GetData objs, cs
    | Block ->
      let block, cs = Block.of_cstruct payload in
      Block block, cs
    | Inv ->
      let invs, cs = ObjList.of_cstruct ~f:Inv.of_cstruct payload in
      Inv invs, cs
    | _ -> failwith "Unsupported"
end

