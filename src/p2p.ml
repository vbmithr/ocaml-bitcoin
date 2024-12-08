(*---------------------------------------------------------------------------
   Copyright (c) 2017 Vincent Bernardoff. All rights reserved.
   Distributed under the GNU Affero GPL license, see LICENSE.
  ---------------------------------------------------------------------------*)

open Util
open Protocol
open Sexplib.Std
open StdLabels
module CS = Bitcoin_cstruct

module Network = struct
  type t =
    | Mainnet
    | Testnet
    | Regtest
  [@@deriving sexp]

  let pp ppf = function
    | Mainnet -> Format.pp_print_string ppf "Mainnet"
    | Testnet -> Format.pp_print_string ppf "Testnet"
    | Regtest -> Format.pp_print_string ppf "Regtest"
  ;;

  let show t = Format.asprintf "%a" pp t

  let seed = function
    | Mainnet -> [ "seed.bitcoin.sipa.be"; "dnsseed.bluematt.me" ]
    | Testnet -> [ "seed.tbtc.petertodd.org"; "testnet-seed.bitcoin.jonasschnelli.ch" ]
    | _ -> invalid_arg "Network.seed"
  ;;

  let port = function
    | Mainnet -> 8333
    | Testnet -> 18333
    | Regtest -> 18444
  ;;

  let start_string = function
    | Mainnet -> "\xf9\xbe\xb4\xd9"
    | Testnet -> "\x0b\x11\x09\x07"
    | Regtest -> "\xfa\xbf\xb5\xda"
  ;;

  let max_nBits = function
    | Mainnet -> 0x1d00ffffl
    | Testnet -> 0x1d00ffffl
    | Regtest -> 0x207fffffl
  ;;

  let of_start_string = function
    | "\xf9\xbe\xb4\xd9" -> Mainnet
    | "\x0b\x11\x09\x07" -> Testnet
    | "\xfa\xbf\xb5\xda" -> Regtest
    | s -> invalid_arg ("Network.of_start_string: got " ^ String.escaped s)
  ;;

  let of_cstruct cs = of_start_string (Cstruct.to_string cs)
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
    | SendCmpct
  [@@deriving sexp]

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
    | "sendheaders" -> SendHeaders
    | "verack" -> VerAck
    | "version" -> Version
    | "sendcmpct" -> SendCmpct
    | s -> invalid_arg ("MessageName.of_string: " ^ s)
  ;;

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
    | SendCmpct -> "sendcmpct"
  ;;

  let of_cstruct cs = c_string_of_cstruct cs |> of_string
  let pp ppf s = Format.pp_print_string ppf (to_string s)
  let show = to_string
end

module GetHashes = struct
  type t =
    { version : int
    ; hashes : Hash256.t list
    ; stop_hash : Hash256.t
    }
  [@@deriving sexp]

  let create ?(version = 75015) ?(stop_hash = Hash256.empty) hashes =
    { version; hashes; stop_hash }
  ;;

  let rec read_hash acc cs = function
    | 0 -> List.rev acc, cs
    | n ->
      let h, cs = Hash256.of_cstruct cs in
      read_hash (h :: acc) cs (pred n)
  ;;

  let of_cstruct cs =
    let open Cstruct in
    let version = LE.get_uint32 cs 0 |> Int32.to_int in
    let cs = shift cs 4 in
    let nb_hashes, cs = CompactSize.of_cstruct_int cs in
    let hashes, cs = read_hash [] cs nb_hashes in
    let stop_hash, cs = Hash256.of_cstruct cs in
    { version; hashes; stop_hash }, cs
  ;;

  let of_cstruct_only_hashes cs =
    let nb_hashes, cs = CompactSize.of_cstruct_int cs in
    let hashes, cs = read_hash [] cs nb_hashes in
    hashes, cs
  ;;

  let to_cstruct cs { version; hashes; stop_hash } =
    let open Cstruct in
    LE.set_uint32 cs 0 (Int32.of_int version);
    let nb_hashes = List.length hashes in
    let cs = shift cs 4 in
    let cs = CompactSize.to_cstruct_int cs nb_hashes in
    let cs = List.fold_left hashes ~init:cs ~f:(fun cs h -> Hash256.to_cstruct cs h) in
    Hash256.to_cstruct cs stop_hash
  ;;
end

module MessageHeader = struct
  type t =
    { network : Network.t
    ; msgname : MessageName.t
    ; size : int
    ; checksum : string
    }
  [@@deriving sexp]

  let size = CS.MessageHeader.sizeof_t
  let empty_checksum = "\x5d\xf6\xe0\xe2"
  let version ~network = { network; msgname = Version; size = 0; checksum = "" }
  let verack ~network = { network; msgname = VerAck; size = 0; checksum = empty_checksum }
  let pong ~network = { network; msgname = Pong; size = 8; checksum = "" }
  let getheaders ~network = { network; msgname = GetHeaders; size = 0; checksum = "" }
  let filterload ~network = { network; msgname = FilterLoad; size = 0; checksum = "" }
  let getdata ~network = { network; msgname = GetData; size = 0; checksum = "" }

  let of_cstruct cs =
    let open CS.MessageHeader in
    let network = get_t_start_string cs |> Network.of_cstruct in
    let msgname = get_t_command_name cs |> MessageName.of_cstruct in
    let size = get_t_payload_size cs |> Int32.to_int in
    let checksum = get_t_checksum cs |> Cstruct.to_string in
    { network; msgname; size; checksum }, Cstruct.shift cs sizeof_t
  ;;

  let to_cstruct cs t =
    let open CS.MessageHeader in
    set_t_start_string (Network.start_string t.network) 0 cs;
    set_t_command_name (MessageName.to_string t.msgname |> bytes_with_msg ~len:12) 0 cs;
    set_t_payload_size cs (Int32.of_int t.size);
    set_t_checksum t.checksum 0 cs;
    Cstruct.shift cs sizeof_t
  ;;
end

module Service = struct
  type t =
    | Network
    | Getutxo
    | Bloom
  [@@deriving sexp]

  let of_int64 v =
    let open Int64 in
    List.filter_map
      ~f:(fun a -> a)
      [ (if logand v 1L <> 0L then Some Network else None)
      ; (if logand v 2L <> 0L then Some Getutxo else None)
      ; (if logand v 4L <> 0L then Some Bloom else None)
      ]
  ;;

  let to_int64 = function
    | Network -> 1L
    | Getutxo -> 2L
    | Bloom -> 4L
  ;;

  let to_int64 =
    List.fold_left ~init:0L ~f:(fun a l ->
      let l = to_int64 l in
      Int64.logor a l)
  ;;
end

module Version = struct
  type t =
    { version : int
    ; services : Service.t list
    ; timestamp : Timestamp.t
    ; recv_services : Service.t list
    ; recv_ipaddr : Ipaddr_sexp.V6.t
    ; recv_port : int
    ; trans_services : Service.t list
    ; trans_ipaddr : Ipaddr_sexp.V6.t
    ; trans_port : int
    ; nonce : int64
    ; user_agent : string
    ; start_height : int
    ; relay : bool
    }
  [@@deriving sexp]

  let create
        ?(version = 70015)
        ?(services = [])
        ?(timestamp = Timestamp.now ())
        ?(recv_services = [ Service.Network ])
        ?(recv_ipaddr = Ipaddr.V6.localhost)
        ~recv_port
        ?(trans_services = [])
        ?(trans_ipaddr = Ipaddr.V6.localhost)
        ~trans_port
        ?(nonce = Int64.of_int (Random.bits ()))
        ?(user_agent = "/OCamlBitcoin:0.1/")
        ?(start_height = 0)
        ?(relay = false)
        ()
    =
    { version
    ; services
    ; timestamp
    ; recv_services
    ; recv_ipaddr
    ; recv_port
    ; trans_services
    ; trans_ipaddr
    ; trans_port
    ; nonce
    ; user_agent
    ; start_height
    ; relay
    }
  ;;

  let of_cstruct cs =
    let open CS.Version in
    let version = get_t_version cs |> Int32.to_int in
    let services = get_t_services cs |> Service.of_int64 in
    let timestamp = get_t_timestamp cs |> Timestamp.of_int64_sec in
    let recv_services = get_t_recv_services cs |> Service.of_int64 in
    let recv_ipaddr =
      get_t_recv_ipaddr cs |> Cstruct.to_string |> Ipaddr.V6.of_octets_exn
    in
    let recv_port = Cstruct.BE.get_uint16 (get_t_recv_port cs) 0 in
    let trans_services = get_t_trans_services cs |> Service.of_int64 in
    let trans_ipaddr =
      get_t_trans_ipaddr cs |> Cstruct.to_string |> Ipaddr.V6.of_octets_exn
    in
    let trans_port = Cstruct.BE.get_uint16 (get_t_trans_port cs) 0 in
    let nonce = get_t_nonce cs in
    let cs = Cstruct.shift cs sizeof_t in
    let user_agent, cs = VarString.of_cstruct cs in
    let start_height = Cstruct.LE.get_uint32 cs 0 |> Int32.to_int in
    let relay =
      match Cstruct.get_uint8 cs 4 with
      | exception _ -> true
      | 0x01 -> true
      | 0x00 -> false
      | _ -> invalid_arg "Version.of_cstruct: unsupported value for relay field"
    in
    ( { version
      ; services
      ; timestamp
      ; recv_services
      ; recv_ipaddr
      ; recv_port
      ; trans_services
      ; trans_ipaddr
      ; trans_port
      ; nonce
      ; user_agent
      ; start_height
      ; relay
      }
    , Cstruct.shift cs 5 )
  ;;

  let to_cstruct cs msg =
    let open CS.Version in
    set_t_version cs (Int32.of_int msg.version);
    set_t_services cs (Service.to_int64 msg.services);
    set_t_timestamp cs (Timestamp.to_int64_sec msg.timestamp);
    set_t_recv_services cs (Service.to_int64 msg.recv_services);
    set_t_recv_ipaddr (Ipaddr.V6.to_octets msg.recv_ipaddr) 0 cs;
    Cstruct.BE.set_uint16 (get_t_recv_port cs) 0 msg.recv_port;
    set_t_trans_services cs (Service.to_int64 msg.trans_services);
    set_t_trans_ipaddr (Ipaddr.V6.to_octets msg.trans_ipaddr) 0 cs;
    Cstruct.BE.set_uint16 (get_t_trans_port cs) 0 msg.trans_port;
    set_t_nonce cs msg.nonce;
    let cs = Cstruct.shift cs sizeof_t in
    let cs = VarString.to_cstruct cs msg.user_agent in
    Cstruct.LE.set_uint32 cs 0 (Int32.of_int msg.start_height);
    Cstruct.set_uint8 cs 4 (if msg.relay then 0x01 else 0x00);
    Cstruct.shift cs 5
  ;;
end

module Address = struct
  type t =
    { timestamp : Timestamp.t
    ; services : Service.t list
    ; ipaddr : Ipaddr_sexp.V6.t
    ; port : int
    }
  [@@deriving sexp]

  let of_cstruct cs =
    let open CS.Address in
    let timestamp = get_t_timestamp cs |> Timestamp.of_int32_sec in
    let services = get_t_services cs |> Service.of_int64 in
    let ipaddr = get_t_ipaddr cs |> Cstruct.to_string |> Ipaddr.V6.of_octets_exn in
    let port = Cstruct.BE.get_uint16 (get_t_port cs) 0 in
    { timestamp; services; ipaddr; port }, Cstruct.shift cs sizeof_t
  ;;
end

module Inv = struct
  type id =
    | Tx
    | Block
    | FilteredBlock
  [@@deriving sexp]

  let id_of_int32 = function
    | 1l -> Tx
    | 2l -> Block
    | 3l -> FilteredBlock
    | _ -> invalid_arg "Inv.id_of_int32"
  ;;

  let int32_of_id = function
    | Tx -> 1l
    | Block -> 2l
    | FilteredBlock -> 3l
  ;;

  type t =
    { id : id
    ; hash : Hash256.t
    }
  [@@deriving sexp]

  let size = CS.Inv.sizeof_t
  let tx hash = { id = Tx; hash }
  let block hash = { id = Block; hash }
  let filteredblock hash = { id = FilteredBlock; hash }

  let of_cstruct cs =
    let open CS.Inv in
    let id = get_t_id cs |> id_of_int32 in
    let hash, _ = get_t_hash cs |> Hash256.of_cstruct in
    { id; hash }, Cstruct.shift cs sizeof_t
  ;;

  let to_cstruct cs { id; hash } =
    let open CS.Inv in
    set_t_id cs (int32_of_id id);
    set_t_hash (Hash256.to_string hash) 0 cs;
    Cstruct.shift cs size
  ;;
end

module PingPong = struct
  let of_cstruct cs = Cstruct.(LE.get_uint64 cs 0, shift cs 8)
end

module MerkleBlock = struct
  type t =
    { header : Header.t
    ; txn_count : int
    ; hashes : Hash256.t list
    ; flags : Bitv.t
    }
  [@@deriving sexp]

  let of_cstruct cs =
    let header, cs = Header.of_cstruct cs in
    let txn_count = Cstruct.LE.get_uint32 cs 0 |> Int32.to_int in
    let cs = Cstruct.shift cs 4 in
    let hashes, cs = GetHashes.of_cstruct_only_hashes cs in
    let flags_len, cs = CompactSize.of_cstruct_int cs in
    let flags = Cstruct.(sub cs 0 flags_len |> to_string |> Bitv.of_string_le) in
    { header; txn_count; hashes; flags }, Cstruct.shift cs flags_len
  ;;
end

module FeeFilter = struct
  let of_cstruct cs = Cstruct.(LE.get_uint64 cs 0, shift cs 8)
end

module FilterAdd = struct
  let of_cstruct cs =
    let nb_bytes, cs = CompactSize.of_cstruct_int cs in
    Cstruct.(sub cs 0 nb_bytes |> to_string, shift cs nb_bytes)
  ;;

  (* let to_cstruct cs data =
   *   let datalen = String.length data in
   *   let cs = CompactSize.to_cstruct_int cs datalen in
   *   Cstruct.blit_from_string data 0 cs 0 datalen ;
   *   Cstruct.shift cs datalen *)
end

module FilterLoad = struct
  type flag =
    | Update_none
    | Update_all
    | Update_p2pkh_only
  [@@deriving sexp]

  let flag_of_int = function
    | 0 -> Update_none
    | 1 -> Update_all
    | 2 -> Update_p2pkh_only
    | _ -> invalid_arg "FilterLoad.flag_of_int"
  ;;

  let int_of_flag = function
    | Update_none -> 0
    | Update_all -> 1
    | Update_p2pkh_only -> 2
  ;;

  type t =
    { filter : Bloom.t
    ; flag : flag
    }
  [@@deriving sexp]

  let of_data
        ?(false_pos_rate = 0.0001)
        ?(tweak = Random.int32 Int32.max_int)
        elts
        ?(nb_elts = List.length elts)
        flag
    =
    let filter = Bloom.create nb_elts false_pos_rate tweak in
    List.iter elts ~f:(Bloom.add filter);
    { filter; flag }
  ;;

  let of_cstruct cs =
    let nb_bytes, cs = CompactSize.of_cstruct_int cs in
    let filter, cs = Cstruct.(sub cs 0 nb_bytes |> to_string, shift cs nb_bytes) in
    let nb_funcs = Cstruct.LE.get_uint32 cs 0 |> Int32.to_int in
    let tweak = Cstruct.LE.get_uint32 cs 4 in
    let flag = Cstruct.get_uint8 cs 8 |> flag_of_int in
    let filter = Bloom.of_filter filter nb_funcs tweak in
    { filter; flag }, Cstruct.shift cs 9
  ;;

  let to_cstruct cs { filter; flag } =
    let cs = CompactSize.to_cstruct_int cs filter.len in
    let filter_bytes = Bloom.to_filter filter in
    Cstruct.blit_from_string filter_bytes 0 cs 0 filter.len;
    let cs = Cstruct.shift cs filter.len in
    Cstruct.LE.set_uint32 cs 0 (Int32.of_int filter.nb_funcs);
    let cs = Cstruct.shift cs 4 in
    Cstruct.LE.set_uint32 cs 0 filter.tweak;
    let cs = Cstruct.shift cs 4 in
    Cstruct.set_uint8 cs 0 (int_of_flag flag);
    Cstruct.shift cs 1
  ;;
end

module Reject = struct
  module Code = struct
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
    [@@deriving sexp]

    let pp ppf = function
      | Decode_error -> Format.fprintf ppf "decode error"
      | Invalid_block h -> Format.fprintf ppf "invalid block %a" Hash256.pp h
      | Invalid_transaction h -> Format.fprintf ppf "invalid transaction %a" Hash256.pp h
      | Block_version_too_old h ->
        Format.fprintf ppf "block version too old %a" Hash256.pp h
      | Protocol_too_old -> Format.fprintf ppf "protocol too old"
      | Double_spend h -> Format.fprintf ppf "double spend %a" Hash256.pp h
      | Too_many_version_messages -> Format.fprintf ppf "too many version messages"
      | Non_standard_transaction h ->
        Format.fprintf ppf "non standard transaction %a" Hash256.pp h
      | Dust h -> Format.fprintf ppf "dust %a" Hash256.pp h
      | Fee_too_low h -> Format.fprintf ppf "fee too low %a" Hash256.pp h
      | Wrong_blockchain h -> Format.fprintf ppf "wrong blockchain %a" Hash256.pp h
    ;;

    (* let show t = Format.asprintf "%a" pp t *)
  end

  type t =
    { message : MessageName.t
    ; code : Code.t
    ; reason : string
    }
  [@@deriving sexp]

  let pp ppf { message; code; reason } =
    Format.fprintf ppf "Reject %a (%a) (%s)" MessageName.pp message Code.pp code reason
  ;;

  let show t = Format.asprintf "%a" pp t

  let code_of_cs code rejected_message cs =
    let open Code in
    match code, rejected_message with
    | 0x01, _ -> Decode_error, cs
    | 0x10, MessageName.Block ->
      let hash, cs = Hash256.of_cstruct cs in
      Invalid_block hash, cs
    | 0x10, Tx ->
      let hash, cs = Hash256.of_cstruct cs in
      Invalid_transaction hash, cs
    | 0x11, Block ->
      let hash, cs = Hash256.of_cstruct cs in
      Block_version_too_old hash, cs
    | 0x11, Version -> Protocol_too_old, cs
    | 0x12, Tx ->
      let hash, cs = Hash256.of_cstruct cs in
      Double_spend hash, cs
    | 0x12, Version -> Too_many_version_messages, cs
    | 0x40, Tx ->
      let hash, cs = Hash256.of_cstruct cs in
      Non_standard_transaction hash, cs
    | 0x41, Tx ->
      let hash, cs = Hash256.of_cstruct cs in
      Dust hash, cs
    | 0x42, Tx ->
      let hash, cs = Hash256.of_cstruct cs in
      Fee_too_low hash, cs
    | 0x43, Block ->
      let hash, cs = Hash256.of_cstruct cs in
      Wrong_blockchain hash, cs
    | _ -> failwith "Unsupported"
  ;;

  let of_cstruct cs =
    let msg_name_len, cs = CompactSize.of_cstruct_int cs in
    let msg_name = Cstruct.(sub cs 0 msg_name_len |> to_string) in
    let cs = Cstruct.shift cs msg_name_len in
    let message = MessageName.of_string msg_name in
    let code = Cstruct.get_uint8 cs 0 in
    let cs = Cstruct.shift cs 1 in
    let reason_len, cs = CompactSize.of_cstruct_int cs in
    let reason = Cstruct.(sub cs 0 reason_len |> to_string) in
    let cs = Cstruct.shift cs reason_len in
    let code, cs = code_of_cs code message cs in
    { message; code; reason }, cs
  ;;
end

module SendCmpct = struct
  type t =
    { compact : bool
    ; version : int
    }
  [@@deriving sexp]

  let of_cstruct cs =
    let open CS.SendCmpct in
    let compact = get_t_b cs |> Bool.of_int in
    let version = get_t_version cs |> Int64.to_int in
    { compact; version }, Cstruct.shift cs sizeof_t
  ;;
end

module Message = struct
  type t =
    | Version of Version.t
    | VerAck
    | GetAddr
    | Addr of Address.t list
    | Ping of int64
    | Pong of int64
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
    | FeeFilter of int64
    | FilterAdd of string
    | FilterClear
    | FilterLoad of FilterLoad.t
    | Reject of Reject.t
    | SendCmpct of SendCmpct.t
  [@@deriving sexp]

  type error = Invalid_checksum of MessageHeader.t

  let of_cstruct cs =
    let h, cs = MessageHeader.of_cstruct cs in
    let payload = Cstruct.sub cs 0 h.size in
    match Chksum.verify ~expected:h.checksum payload with
    | false -> Error (Invalid_checksum h), cs
    | true ->
      let msg, cs =
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
          let invs, cs = ObjList.of_cstruct ~f:Inv.of_cstruct payload in
          GetData invs, cs
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
          let hdrs, cs = ObjList.of_cstruct ~f:Header.of_cstruct_txcount payload in
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
        | SendCmpct ->
          let sendcmpct, cs = SendCmpct.of_cstruct payload in
          SendCmpct sendcmpct, cs
        | _ -> failwith "Unsupported"
      in
      Ok (h, msg), cs
  ;;

  let to_cstruct ~network cs = function
    | Version ver ->
      let hdr = MessageHeader.version ~network in
      let payload_cs = Cstruct.shift cs MessageHeader.size in
      let end_cs = Version.to_cstruct payload_cs ver in
      let size, checksum = Chksum.compute' payload_cs end_cs in
      let _ = MessageHeader.to_cstruct cs { hdr with size; checksum } in
      end_cs
    | VerAck -> MessageHeader.(to_cstruct cs (verack ~network))
    | Pong i ->
      let hdr = MessageHeader.pong ~network in
      let payload_cs = Cstruct.shift cs MessageHeader.size in
      Cstruct.LE.set_uint64 payload_cs 0 i;
      let end_cs = Cstruct.shift payload_cs 8 in
      let size, checksum = Chksum.compute' payload_cs end_cs in
      let _ = MessageHeader.to_cstruct cs { hdr with size; checksum } in
      end_cs
    | GetHeaders hashes ->
      let hdr = MessageHeader.getheaders ~network in
      let payload_cs = Cstruct.shift cs MessageHeader.size in
      let end_cs = GetHashes.to_cstruct payload_cs hashes in
      let size, checksum = Chksum.compute' payload_cs end_cs in
      let _ = MessageHeader.to_cstruct cs { hdr with size; checksum } in
      end_cs
    | FilterLoad filterload ->
      let hdr = MessageHeader.filterload ~network in
      let payload_cs = Cstruct.shift cs MessageHeader.size in
      let end_cs = FilterLoad.to_cstruct payload_cs filterload in
      let size, checksum = Chksum.compute' payload_cs end_cs in
      let _ = MessageHeader.to_cstruct cs { hdr with size; checksum } in
      end_cs
    | GetData invs ->
      let hdr = MessageHeader.getdata ~network in
      let payload_cs = Cstruct.shift cs MessageHeader.size in
      let end_cs = ObjList.to_cstruct payload_cs invs ~f:Inv.to_cstruct in
      let size, checksum = Chksum.compute' payload_cs end_cs in
      let _ = MessageHeader.to_cstruct cs { hdr with size; checksum } in
      end_cs
    | _ -> failwith "Unsupported"
  ;;
end
