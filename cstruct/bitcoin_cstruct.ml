module Header = struct
  [%%cstruct type t = {
      version: uint32_t ;
      prev_block : uint8_t [@len 32] ;
      merkle_root : uint8_t [@len 32] ;
      timestamp : uint32_t ;
      bits : uint32_t ;
      nonce : uint32_t ;
    } [@@little_endian]]
end

module Outpoint = struct
  [%%cstruct type t = {
      hash : uint8_t [@len 32] ;
      index : uint32_t ;
    } [@@little_endian]]
end

module MessageHeader = struct
  [%%cstruct type t = {
      start_string : uint8_t [@len 4] ;
      command_name : uint8_t [@len 12] ;
      payload_size : uint32_t ;
      checksum : uint8_t [@len 4] ;
    } [@@little_endian]]
end

module Version = struct
  [%%cstruct type t = {
      version : uint32_t ;
      services : uint64_t ;
      timestamp : uint64_t ;
      recv_services : uint64_t ;
      recv_ipaddr : uint8_t [@len 16] ;
      recv_port : uint8_t [@len 2];
      trans_services : uint64_t ;
      trans_ipaddr : uint8_t [@len 16] ;
      trans_port : uint8_t [@len 2] ;
      nonce : uint64_t ;
    } [@@little_endian]]
end

module Address = struct
  [%%cstruct type t = {
      timestamp : uint32_t ;
      services : uint64_t ;
      ipaddr : uint8_t [@len 16] ;
      port : uint8_t [@len 2];
    } [@@little_endian]]
end

module Inv = struct
  [%%cstruct type t = {
      id : uint32_t ;
      hash : uint8_t [@len 32] ;
    } [@@little_endian]]
end

module SendCmpct = struct
  [%%cstruct type t = {
      b : uint8_t ;
      version : uint64_t ;
    } [@@little_endian]]
end
