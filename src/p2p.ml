(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the GNU Affero GPL license, see LICENSE.
  ---------------------------------------------------------------------------*)

open StdLabels
open Util
open Protocol

module Network = struct
  type t =
    | Mainnet
    | Testnet
    | Regtest

  let seed = function
    | Mainnet -> [
        "seed.bitcoin.sipa.be" ;
        "dnsseed.bluematt.me" ;
      ]
    | Testnet -> [
        "seed.tbtc.petertodd.org" ;
        "testnet-seed.bitcoin.jonasschnelli.ch" ;
      ]
    | _ -> invalid_arg "Network.seed"

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

  let to_string = function
    | Block -> "block"
    | GetBlocks -> "getblocks"
    | GetData -> "getdata"
    | GetHeaders -> "getheaders"
    | Headers -> "headers"
    | Inv -> "inv"
    | MemPool -> "mempool"
    | MerkleBlock -> "merkleblock"
    | NotFound -> "notfound"
    | Tx -> "tx"
    | Addr -> "addr"
    | Alert -> "alert"
    | FeeFilter -> "feefilter"
    | FilterAdd -> "filteradd"
    | FilterClear -> "filterclear"
    | FilterLoad -> "filterload"
    | GetAddr -> "getaddr"
    | Ping -> "ping"
    | Pong -> "pong"
    | Reject -> "reject"
    | SendHeaders -> "sendheaders"
    | VerAck -> "verack"
    | Version -> "version"

  let of_cstruct cs =
    c_string_of_cstruct cs |> of_string
end

module MessageHeader = struct
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

  let length = C.sizeof_t
  let empty_checksum = 0x5df6e0e2l

  let version ~network = {
    network ; msgname = Version ;
    size = 0 ; checksum = 0l ;
  }

  let verack ~network = {
    network ; msgname = VerAck ;
    size = 0 ; checksum = empty_checksum ;
  }

  let of_cstruct cs =
    let open C in
    let network = get_t_start_string cs |> Network.of_start_string in
    let msgname = get_t_command_name cs |> MessageName.of_cstruct in
    let size = get_t_payload_size cs |> Int32.to_int in
    let checksum = get_t_checksum cs in
    { network ; msgname ; size ; checksum }, Cstruct.shift cs sizeof_t

  let to_cstruct cs t =
    let open C in
    set_t_start_string cs (Network.start_string t.network) ;
    set_t_command_name (MessageName.to_string t.msgname) 0 cs ;
    set_t_payload_size cs (Int32.of_int t.size) ;
    set_t_checksum cs t.checksum ;
    Cstruct.shift cs sizeof_t
end

module Service = struct
  type t =
    | Node_network

  let of_int64 = function
    | 0L -> []
    | 1L -> [Node_network]
    | _ -> invalid_arg "Service.of_int64"

  let to_int64 = function
    | Node_network -> 1L

  let to_int64 =
    List.fold_left ~init:0L ~f:begin fun a l ->
      Int64.logor a (to_int64 l)
    end
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

  let create
      ?(version=70015)
      ?(services=[])
      ?(timestamp=Ptime_clock.now ())
      ?(recv_services=[Service.Node_network])
      ?(recv_ipaddr=Ipaddr.V6.localhost)
      ~recv_port
      ?(trans_services=[])
      ?(trans_ipaddr=Ipaddr.V6.localhost)
      ~trans_port
      ?(nonce=Int64.of_int (Random.bits ()))
      ?(user_agent="/OCamlBitcoin:0.1/")
      ?(start_height=0)
      ?(relay=true)
      () =
    { version ; services ; timestamp ; recv_services ;
      recv_ipaddr ; recv_port ; trans_services ; trans_ipaddr ;
      trans_port ; nonce ; user_agent ; start_height ; relay }

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

  let to_cstruct cs msg =
    let open C in
    set_t_version cs (Int32.of_int msg.version) ;
    set_t_services cs (Service.to_int64 msg.services) ;
    set_t_timestamp cs (Timestamp.to_int32 msg.timestamp) ;
    set_t_recv_services cs (Service.to_int64 msg.recv_services) ;
    set_t_recv_ipaddr (Ipaddr.V6.to_bytes msg.recv_ipaddr) 0 cs ;
    Cstruct.BE.set_uint16 (get_t_recv_port cs) 0 msg.recv_port ;
    set_t_trans_services cs (Service.to_int64 msg.trans_services) ;
    set_t_trans_ipaddr (Ipaddr.V6.to_bytes msg.trans_ipaddr) 0 cs ;
    Cstruct.BE.set_uint16 (get_t_trans_port cs) 0 msg.trans_port ;
    set_t_nonce cs msg.nonce ;
    let cs = Cstruct.shift cs sizeof_t in
    let user_agent_len = String.length msg.user_agent in
    let cs = CompactSize.to_cstruct_int cs user_agent_len in
    Cstruct.blit_from_string msg.user_agent 0 cs 0 user_agent_len ;
    let cs = Cstruct.shift cs user_agent_len in
    Cstruct.LE.set_uint32 cs 0 (Int32.of_int msg.start_height) ;
    Cstruct.set_uint8 cs 4 (if msg.relay then 0x01 else 0x00) ;
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

module GetHashes = struct
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

  let of_cstruct_only_hashes cs =
    let open Cstruct in
    let nb_hashes, cs = CompactSize.of_cstruct_int cs in
    let hashes, cs = read_hash Hash.Set.empty cs nb_hashes in
    hashes, cs
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

module PingPong = struct
  let of_cstruct cs =
    Cstruct.(LE.get_uint64 cs 0, shift cs 8)
end

module MerkleBlock = struct
  type t = {
    header : Header.t ;
    txn_count : int ;
    hashes : Hash.Set.t ;
    flags : string ;
  }

  let of_cstruct cs =
    let header, cs = Header.of_cstruct cs in
    let txn_count = Cstruct.LE.get_uint32 cs 0 |> Int32.to_int in
    let cs = Cstruct.shift cs 4 in
    let hashes, cs  = GetHashes.of_cstruct_only_hashes cs in
    let flags_len, cs = CompactSize.of_cstruct_int cs in
    let flags = Cstruct.(sub cs 0 flags_len |> to_string) in
    { header ; txn_count ; hashes ; flags }, Cstruct.shift cs flags_len
end

module FeeFilter = struct
  let of_cstruct cs =
    Cstruct.(LE.get_uint64 cs 0, shift cs 8)
end

module FilterAdd = struct
  let of_cstruct cs =
    let nb_bytes, cs = CompactSize.of_cstruct_int cs in
    Cstruct.(sub cs 0 nb_bytes |> to_string, shift cs nb_bytes)
end

module FilterLoad = struct
  type flag =
    | Update_none
    | Update_all
    | Update_p2pkh_only

  let flag_of_int = function
    | 0 -> Update_none
    | 1 -> Update_all
    | 2 -> Update_p2pkh_only
    | _ -> invalid_arg "FilterLoad.flag_of_int"

  type t = {
    filter : string ;
    nb_hash_funcs : int ;
    tweak : Int32.t ;
    flag : flag ;
  }

  let of_cstruct cs =
    let nb_bytes, cs = CompactSize.of_cstruct_int cs in
    let filter, cs = Cstruct.(sub cs 0 nb_bytes |> to_string, shift cs nb_bytes) in
    let nb_hash_funcs = Cstruct.LE.get_uint32 cs 0 |> Int32.to_int in
    let tweak = Cstruct.LE.get_uint32 cs 4 in
    let flag = Cstruct.get_uint8 cs 8 |> flag_of_int in
    { filter ; nb_hash_funcs ; tweak ; flag }, Cstruct.shift cs 9
end

module Reject = struct
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

  let code_of_cs code rejected_message cs =
    match code, rejected_message with
    | 0x01, _ -> Decode_error, cs
    | 0x10, MessageName.Block ->
      let hash, cs = Hash.of_cstruct cs in
      Invalid_block hash, cs
    | 0x10, Tx ->
      let hash, cs = Hash.of_cstruct cs in
      Invalid_transaction hash, cs
    | 0x11, Block ->
      let hash, cs = Hash.of_cstruct cs in
      Block_version_too_old hash, cs
    | 0x11, Version ->
      Protocol_too_old, cs
    | 0x12, Tx ->
      let hash, cs = Hash.of_cstruct cs in
      Double_spend hash, cs
    | 0x12, Version ->
      Too_many_version_messages, cs
    | 0x40, Tx ->
      let hash, cs = Hash.of_cstruct cs in
      Non_standard_transaction hash, cs
    | 0x41, Tx ->
      let hash, cs = Hash.of_cstruct cs in
      Dust hash, cs
    | 0x42, Tx ->
      let hash, cs = Hash.of_cstruct cs in
      Fee_too_low hash, cs
    | 0x43, Block ->
      let hash, cs = Hash.of_cstruct cs in
      Wrong_blockchain hash, cs
    | _ -> failwith "Unsupported"

  let of_cstruct cs =
    let msg_name_len, cs = CompactSize.of_cstruct_int cs in
    let msg_name = Cstruct.(sub cs 0 msg_name_len |> to_string) in
    let cs = Cstruct.shift cs msg_name_len in
    let rejected_message = MessageName.of_string msg_name in
    let code = Cstruct.get_uint8 cs 0 in
    let cs = Cstruct.shift cs 1 in
    let reason_len, cs = CompactSize.of_cstruct_int cs in
    let reason = Cstruct.(sub cs 0 reason_len |> to_string) in
    let cs = Cstruct.shift cs reason_len in
    let code, cs = code_of_cs code rejected_message cs in
    { rejected_message ; code ; reason }, cs
end

module Message = struct
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
  [@@deriving sexp]

  let of_cstruct cs =
    let h, cs = MessageHeader.of_cstruct cs in
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
    | Ping ->
      let nonce, cs = PingPong.of_cstruct payload in
      Ping nonce, cs
    | Pong ->
      let nonce, cs = PingPong.of_cstruct payload in
      Pong nonce, cs
    | GetBlocks ->
      let objs, cs = GetHashes.of_cstruct payload in
      GetBlocks objs, cs
    | GetData ->
      let objs, cs = GetHashes.of_cstruct payload in
      GetData objs, cs
    | GetHeaders ->
      let objs, cs = GetHashes.of_cstruct payload in
      GetHeaders objs, cs
    | Block ->
      let block, cs = Block.of_cstruct payload in
      Block block, cs
    | MerkleBlock ->
      let mblock, cs = MerkleBlock.of_cstruct payload in
      MerkleBlock mblock, cs
    | Headers ->
      let hdrs, cs = ObjList.of_cstruct ~f:Header.of_cstruct payload in
      Headers hdrs, cs
    | Inv ->
      let invs, cs = ObjList.of_cstruct ~f:Inv.of_cstruct payload in
      Inv invs, cs
    | NotFound ->
      let invs, cs = ObjList.of_cstruct ~f:Inv.of_cstruct payload in
      NotFound invs, cs
    | MemPool -> MemPool, cs
    | SendHeaders -> SendHeaders, cs
    | Tx ->
      let tx, cs = Transaction.of_cstruct payload in
      Tx tx, cs
    | FeeFilter ->
      let fee, cs = FeeFilter.of_cstruct payload in
      FeeFilter fee, cs
    | FilterAdd ->
      let filter, cs = FilterAdd.of_cstruct payload in
      FilterAdd filter, cs
    | FilterClear -> FilterClear, cs
    | FilterLoad ->
      let filter, cs = FilterLoad.of_cstruct payload in
      FilterLoad filter, cs
    | Reject ->
      let reject, cs = Reject.of_cstruct payload in
      Reject reject, cs
    | _ -> failwith "Unsupported"

  let to_cstruct ~network cs = function
    | Version ver ->
      let hdr = MessageHeader.version ~network in
      let payload_cs = Cstruct.shift cs MessageHeader.length in
      let end_cs = Version.to_cstruct payload_cs ver in
      let size, checksum = Chksum.compute' payload_cs end_cs in
      let _ = MessageHeader.to_cstruct cs { hdr with size ; checksum } in
      end_cs
    | VerAck ->
      MessageHeader.(to_cstruct cs (verack ~network))
    | _ -> failwith "Unsupported"
end

