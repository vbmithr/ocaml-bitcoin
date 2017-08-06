open Util

module Opcode = struct
  type t =
    | Op_zero
    | Op_data of int
    | Op_pushdata1
    | Op_pushdata2
    | Op_pushdata4
    | Op_1negate
    | Op_1
    | Op_2
    | Op_3
    | Op_4
    | Op_5
    | Op_6
    | Op_7
    | Op_8
    | Op_9
    | Op_10
    | Op_11
    | Op_12
    | Op_13
    | Op_14
    | Op_15
    | Op_16
    | Op_nop
    | Op_if
    | Op_notif
    | Op_else
    | Op_endif
    | Op_verify
    | Op_return
    | Op_toaltstack
    | Op_fromaltstack
    | Op_ifdup
    | Op_depth
    | Op_drop
    | Op_dup
    | Op_nip
    | Op_over
    | Op_pick
    | Op_roll
    | Op_rot
    | Op_swap
    | Op_tuck
    | Op_2drop
    | Op_2dup
    | Op_3dup
    | Op_2over
    | Op_2rot
    | Op_2swap
    | Op_size
    | Op_equal
    | Op_equalverify
    | Op_1add
    | Op_1sub
    | Op_negate
    | Op_abs
    | Op_not
    | Op_0notequal
    | Op_add
    | Op_sub
    | Op_booland
    | Op_boolor
    | Op_numequal
    | Op_numequalverify
    | Op_numnotequal
    | Op_lessthan
    | Op_greaterthan
    | Op_lessthanorequal
    | Op_greaterthanorequal
    | Op_min
    | Op_max
    | Op_within
    | Op_ripemd160
    | Op_sha1
    | Op_sha256
    | Op_hash160
    | Op_hash256
    | Op_codeseparator
    | Op_checksig
    | Op_checksigverify
    | Op_checkmultisig
    | Op_checkmultisigverify
    | Op_checklocktimeverify
    | Op_checksequenceverify
    | Op_pubkeyhash
    | Op_pubkey
    | Op_invalidopcode
    | Op_reserved
    | Op_ver
    | Op_verif
    | Op_vernotif
    | Op_reserved1
    | Op_reserved2
    | Op_nop1
    | Op_nop4
    | Op_nop5
    | Op_nop6
    | Op_nop7
    | Op_nop8
    | Op_nop9
    | Op_nop10

  let to_int = function
    | Op_zero -> 0
    | Op_data n -> if n < 1 || n > 75 then failwith "Script.to_int" else n
    | Op_pushdata1 -> 76
    | Op_pushdata2 -> 77
    | Op_pushdata4 -> 78
    | Op_1negate -> 79
    | Op_1 -> 81
    | Op_2 -> 82
    | Op_3 -> 83
    | Op_4 -> 84
    | Op_5 -> 85
    | Op_6 -> 86
    | Op_7 -> 87
    | Op_8 -> 88
    | Op_9 -> 89
    | Op_10 -> 90
    | Op_11 -> 91
    | Op_12 -> 92
    | Op_13 -> 93
    | Op_14 -> 94
    | Op_15 -> 95
    | Op_16 -> 86
    | Op_nop -> 97
    | Op_if -> 99
    | Op_notif -> 100
    | Op_else -> 103
    | Op_endif -> 104
    | Op_verify -> 105
    | Op_return -> 106
    | Op_toaltstack -> 107
    | Op_fromaltstack -> 108
    | Op_ifdup -> 115
    | Op_depth -> 116
    | Op_drop -> 117
    | Op_dup -> 118
    | Op_nip -> 119
    | Op_over -> 120
    | Op_pick -> 121
    | Op_roll -> 122
    | Op_rot -> 123
    | Op_swap -> 124
    | Op_tuck -> 125
    | Op_2drop -> 109
    | Op_2dup -> 110
    | Op_3dup -> 111
    | Op_2over -> 112
    | Op_2rot -> 113
    | Op_2swap -> 114
    | Op_size -> 130
    | Op_equal -> 135
    | Op_equalverify -> 136
    | Op_1add -> 139
    | Op_1sub -> 140
    | Op_negate -> 143
    | Op_abs -> 144
    | Op_not -> 145
    | Op_0notequal -> 146
    | Op_add -> 147
    | Op_sub -> 148
    | Op_booland -> 154
    | Op_boolor -> 155
    | Op_numequal -> 156
    | Op_numequalverify -> 157
    | Op_numnotequal -> 158
    | Op_lessthan -> 159
    | Op_greaterthan -> 160
    | Op_lessthanorequal -> 161
    | Op_greaterthanorequal -> 162
    | Op_min -> 163
    | Op_max -> 164
    | Op_within -> 165
    | Op_ripemd160 -> 166
    | Op_sha1 -> 167
    | Op_sha256 -> 168
    | Op_hash160 -> 168
    | Op_hash256 -> 170
    | Op_codeseparator -> 171
    | Op_checksig -> 172
    | Op_checksigverify -> 173
    | Op_checkmultisig -> 174
    | Op_checkmultisigverify -> 175
    | Op_checklocktimeverify -> 177
    | Op_checksequenceverify -> 178
    | Op_pubkeyhash -> 253
    | Op_pubkey -> 254
    | Op_invalidopcode -> 255
    | Op_reserved -> 80
    | Op_ver -> 98
    | Op_verif -> 101
    | Op_vernotif -> 102
    | Op_reserved1 -> 137
    | Op_reserved2 -> 138
    | Op_nop1 -> 176
    | Op_nop4 -> 179
    | Op_nop5 -> 180
    | Op_nop6 -> 181
    | Op_nop7 -> 182
    | Op_nop8 -> 183
    | Op_nop9 -> 184
    | Op_nop10 -> 185

  let of_int = function
    | 0 -> Op_zero
    | n when n > 0 && n < 76  -> Op_data n
    | 76 -> Op_pushdata1
    | 77 -> Op_pushdata2
    | 78 -> Op_pushdata4
    | 79 -> Op_1negate
    | 80 -> Op_reserved
    | 81 -> Op_1
    | 82 -> Op_2
    | 83 -> Op_3
    | 84 -> Op_4
    | 85 -> Op_5
    | 86 -> Op_6
    | 87 -> Op_7
    | 88 -> Op_8
    | 89 -> Op_9
    | 90 -> Op_10
    | 91 -> Op_11
    | 92 -> Op_12
    | 93 -> Op_13
    | 94 -> Op_14
    | 95 -> Op_15
    | 96 -> Op_16
    | 97 -> Op_nop
    | 98 -> Op_ver
    | 99 -> Op_if
    | 100 -> Op_notif
    | 101 -> Op_verif
    | 102 -> Op_vernotif
    | 103 -> Op_else
    | 104 -> Op_endif
    | 105 -> Op_verify
    | 106 -> Op_return
    | 107 -> Op_toaltstack
    | 108 -> Op_fromaltstack
    | 115 -> Op_ifdup
    | 116 -> Op_depth
    | 117 -> Op_drop
    | 118 -> Op_dup
    | 119 -> Op_nip
    | 120 -> Op_over
    | 121 -> Op_pick
    | 122 -> Op_roll
    | 123 -> Op_rot
    | 124 -> Op_swap
    | 125 -> Op_tuck
    | 109 -> Op_2drop
    | 110 -> Op_2dup
    | 111 -> Op_3dup
    | 112 -> Op_2over
    | 113 -> Op_2rot
    | 114 -> Op_2swap
    | 130 -> Op_size
    | 135 -> Op_equal
    | 136 -> Op_equalverify
    | 137 -> Op_reserved1
    | 138 -> Op_reserved2
    | 139 -> Op_1add
    | 140 -> Op_1sub
    | 143 -> Op_negate
    | 144 -> Op_abs
    | 145 -> Op_not
    | 146 -> Op_0notequal
    | 147 -> Op_add
    | 148 -> Op_sub
    | 154 -> Op_booland
    | 155 -> Op_boolor
    | 156 -> Op_numequal
    | 157 -> Op_numequalverify
    | 158 -> Op_numnotequal
    | 159 -> Op_lessthan
    | 160 -> Op_greaterthan
    | 161 -> Op_lessthanorequal
    | 162 -> Op_greaterthanorequal
    | 163 -> Op_min
    | 164 -> Op_max
    | 165 -> Op_within
    | 166 -> Op_ripemd160
    | 167 -> Op_sha1
    | 168 -> Op_sha256
    | 169 -> Op_hash160
    | 170 -> Op_hash256
    | 171 -> Op_codeseparator
    | 172 -> Op_checksig
    | 173 -> Op_checksigverify
    | 174 -> Op_checkmultisig
    | 175 -> Op_checkmultisigverify
    | 176 -> Op_nop1
    | 177 -> Op_checklocktimeverify
    | 178 -> Op_checksequenceverify
    | 179 -> Op_nop4
    | 180 -> Op_nop5
    | 181 -> Op_nop6
    | 182 -> Op_nop7
    | 183 -> Op_nop8
    | 184 -> Op_nop9
    | 185 -> Op_nop10
    | 253 -> Op_pubkeyhash
    | 254 -> Op_pubkey
    | 255 -> Op_invalidopcode
    | n -> invalid_arg ("Opcode.of_int: got " ^ (string_of_int n))

  let of_cstruct cs =
    Cstruct.(get_uint8 cs 0 |> of_int, shift cs 1)
end

type elt =
  | O of Opcode.t
  | D of string

type t = elt list

let read_all cs =
  let rec inner acc data_len cs =
    if cs.Cstruct.len = 0 then List.rev acc
    else if cs.len = 0 && data_len <> 0 then invalid_arg "Script.read_all"
    else if data_len > 0 then
      inner
        (D Cstruct.(sub cs 0 data_len |> to_string) :: acc)
        0
        (Cstruct.shift cs data_len)
    else
      let elt, cs = Opcode.of_cstruct cs in
      match elt with
      | Op_data n -> inner acc n cs
      | Op_pushdata1 ->
        let data_len = Cstruct.get_uint8 cs 0 in
        inner acc data_len (Cstruct.shift cs 1)
      | Op_pushdata2 ->
        let data_len = Cstruct.LE.get_uint16 cs 0 in
        inner acc data_len (Cstruct.shift cs 2)
      | Op_pushdata4 ->
        let data_len = Cstruct.LE.get_uint32 cs 0 |> Int32.to_int in
        inner acc data_len (Cstruct.shift cs 4)
      | op ->
        inner (O op :: acc) 0 cs
  in
  inner [] 0 cs

let of_cstruct cs =
  let len, cs = CompactSize.of_cstruct_int cs in
  read_all (Cstruct.sub cs 0 len),
  Cstruct.shift cs len
