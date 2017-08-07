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
    | Op_cat
    | Op_substr
    | Op_left
    | Op_right
    | Op_size
    | Op_invert
    | Op_and
    | Op_or
    | Op_xor
    | Op_equal
    | Op_equalverify
    | Op_1add
    | Op_1sub
    | Op_2mul
    | Op_2div
    | Op_negate
    | Op_abs
    | Op_not
    | Op_0notequal
    | Op_add
    | Op_sub
    | Op_mul
    | Op_div
    | Op_mod
    | Op_lshift
    | Op_rshift
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
    | Op_cat -> 126
    | Op_substr -> 127
    | Op_left -> 128
    | Op_right -> 129
    | Op_size -> 130
    | Op_invert -> 131
    | Op_and -> 132
    | Op_or -> 133
    | Op_xor -> 134
    | Op_equal -> 135
    | Op_equalverify -> 136
    | Op_1add -> 139
    | Op_1sub -> 140
    | Op_2mul -> 141
    | Op_2div -> 142
    | Op_negate -> 143
    | Op_abs -> 144
    | Op_not -> 145
    | Op_0notequal -> 146
    | Op_add -> 147
    | Op_sub -> 148
    | Op_mul -> 149
    | Op_div -> 150
    | Op_mod -> 151
    | Op_lshift -> 152
    | Op_rshift -> 153
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
    | 126 -> Op_cat
    | 127 -> Op_substr
    | 128 -> Op_left
    | 129 -> Op_right
    | 130 -> Op_size
    | 131 -> Op_invert
    | 132 -> Op_and
    | 133 -> Op_or
    | 134 -> Op_xor
    | 135 -> Op_equal
    | 136 -> Op_equalverify
    | 137 -> Op_reserved1
    | 138 -> Op_reserved2
    | 139 -> Op_1add
    | 140 -> Op_1sub
    | 141 -> Op_2mul
    | 142 -> Op_2div
    | 143 -> Op_negate
    | 144 -> Op_abs
    | 145 -> Op_not
    | 146 -> Op_0notequal
    | 147 -> Op_add
    | 148 -> Op_sub
    | 149 -> Op_mul
    | 150 -> Op_div
    | 151 -> Op_mod
    | 152 -> Op_lshift
    | 153 -> Op_rshift
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

  let to_cstruct cs opcode =
    Cstruct.set_uint8 cs 0 (to_int opcode) ;
    Cstruct.shift cs 1
end

module Element = struct
  type t =
    | O of Opcode.t
    | D of Cstruct.t

  let to_cstruct cs = function
    | O opcode ->
      Opcode.to_cstruct cs opcode
    | D buf ->
      Cstruct.blit buf buf.off cs 0 buf.len ;
      Cstruct.shift cs buf.len
end

type t = Element.t list

let read_all cs =
  let open Element in
  let rec inner acc data_len cs =
    if cs.Cstruct.len = 0 then List.rev acc
    else if cs.len = 0 && data_len <> 0 then invalid_arg "Script.read_all"
    else if data_len > 0 then
      inner
        (D (Cstruct.sub cs 0 data_len) :: acc)
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

let to_cstruct cs elts =
  let open Element in
  let len = Base.List.fold_left elts ~init:0 ~f:begin fun a -> function
      | O _ -> Caml.succ a
      | D cs -> a + cs.len
    end in
  let cs = CompactSize.to_cstruct_int cs len in
  Base.List.fold_left elts ~init:cs ~f:begin fun cs elt ->
    Element.to_cstruct cs elt
  end

module Run = struct
  type stack_elt =
    | Int of Int32.t
    | Bytes of Cstruct.t

  let is_zero = function
    | Int 0l -> true
    | Bytes cs when cs.len = 0 -> true
    | Bytes cs when cs.len = 1 ->
      Stdint.Int8.(of_bytes_little_endian (Cstruct.to_string cs) 0 = zero)
    | Bytes cs when cs.len = 2 ->
      Stdint.Int16.(of_bytes_little_endian (Cstruct.to_string cs) 0 = zero)
    | Bytes cs when cs.len = 3 ->
      Stdint.Int24.(of_bytes_little_endian (Cstruct.to_string cs) 0 = zero)
    | Bytes cs when cs.len = 4 ->
      Stdint.Int32.(of_bytes_little_endian (Cstruct.to_string cs) 0 = zero)
    | Bytes cs when cs.len = 8 ->
      Stdint.Int64.(of_bytes_little_endian (Cstruct.to_string cs) 0 = zero)
    | _ -> false

  let eval_exn code =
    let rec drop stack altstack n current = function
      | Element.O Op_if :: rest -> drop stack altstack n (succ current) rest
      | O Op_notif :: rest -> drop stack altstack n (succ current) rest
      | O Op_else :: rest when current > n -> drop stack altstack n current rest
      | O Op_else :: rest when n = current -> eval_main n stack altstack rest
      | O Op_endif :: rest when current > n -> drop stack altstack n (pred current) rest
      | O Op_endif :: rest when current = n -> eval_main n stack altstack rest
      | _ :: rest -> drop stack altstack n current rest
      | [] -> invalid_arg "Run.eval: unfinished if sequence"
    and eval_main iflevel stack altstack code =
      match code, stack with
      | Element.D buf :: rest, _ -> eval_main iflevel (Bytes buf :: stack) altstack rest
      | O Op_zero :: _, _ -> invalid_arg "Run.eval: Op_zero"
      | O (Op_data _) :: _, _ -> invalid_arg "Run.eval: Op_data"
      | O Op_pushdata1 :: _, _ -> invalid_arg "Run.eval: Op_pushdata1"
      | O Op_pushdata2 :: _, _ -> invalid_arg "Run.eval: Op_pushdata2"
      | O Op_pushdata4 :: _, _ -> invalid_arg "Run.eval: Op_pushdata4"
      | O Op_1negate :: rest, _ -> eval_main iflevel (Int (-1l) :: stack) altstack rest
      | O Op_1 :: rest, _ -> eval_main iflevel (Int 1l :: stack) altstack rest
      | O Op_2 :: rest, _ -> eval_main iflevel (Int 2l :: stack) altstack rest
      | O Op_3 :: rest, _ -> eval_main iflevel (Int 3l :: stack) altstack rest
      | O Op_4 :: rest, _ -> eval_main iflevel (Int 4l :: stack) altstack rest
      | O Op_5 :: rest, _ -> eval_main iflevel (Int 5l :: stack) altstack rest
      | O Op_6 :: rest, _ -> eval_main iflevel (Int 6l :: stack) altstack rest
      | O Op_7 :: rest, _ -> eval_main iflevel (Int 7l :: stack) altstack rest
      | O Op_8 :: rest, _ -> eval_main iflevel (Int 8l :: stack) altstack rest
      | O Op_9 :: rest, _ -> eval_main iflevel (Int 9l :: stack) altstack rest
      | O Op_10 :: rest, _ -> eval_main iflevel (Int 10l :: stack) altstack rest
      | O Op_11 :: rest, _ -> eval_main iflevel (Int 11l :: stack) altstack rest
      | O Op_12 :: rest, _ -> eval_main iflevel (Int 12l :: stack) altstack rest
      | O Op_13 :: rest, _ -> eval_main iflevel (Int 13l :: stack) altstack rest
      | O Op_14 :: rest, _ -> eval_main iflevel (Int 14l :: stack) altstack rest
      | O Op_15 :: rest, _ -> eval_main iflevel (Int 15l :: stack) altstack rest
      | O Op_16 :: rest, _ -> eval_main iflevel (Int 16l :: stack) altstack rest
      | O Op_nop :: rest, _ -> eval_main iflevel stack altstack rest
      | O Op_if :: rest, [] -> invalid_arg "Run.eval: if with empty stack"
      | O Op_notif :: rest, [] -> invalid_arg "Run.eval: notif with empty stack"
      | O Op_if :: rest, v :: _ ->
        if is_zero v then
          drop stack altstack (succ iflevel) (succ iflevel) rest
        else
          eval_main (succ iflevel) stack altstack rest
      | O Op_notif :: rest, v :: _ ->
        if is_zero v then
          eval_main (succ iflevel) stack altstack rest
        else
          drop stack altstack (succ iflevel) (succ iflevel) rest
      | O Op_else :: rest, _ ->
        if iflevel = 0 then invalid_arg "Run.eval: unconsistent else"
        else drop stack altstack iflevel iflevel rest
      | O Op_endif :: rest, _ ->
        let iflevel = pred iflevel in
        if iflevel < 0 then invalid_arg "Run.eval: unconsistent endif"
        else eval_main iflevel stack altstack rest
      | O Op_verify :: rest, [] ->
        invalid_arg "Run.eval: op_verify without a top stack element"
      | O Op_verify :: rest, v :: _ -> not (is_zero v), stack, rest
      | O Op_return :: rest, _ -> false, stack, rest
      | O Op_toaltstack :: rest, [] ->
        invalid_arg "Run.eval: op_toaltstack without a top stack element"
      | O Op_toaltstack :: rest, v :: stack ->
        eval_main iflevel stack (v :: altstack) rest
      | O Op_fromaltstack :: rest, stack -> begin
        match altstack with
        | [] -> invalid_arg "Run.eval: op_fromaltstack without a top stack element"
        | v :: altstack -> eval_main iflevel (v :: stack) altstack rest
      end
      | O Op_ifdup :: rest, [] ->
        invalid_arg "Run.eval: op_ifdup without a top stack element"
      | O Op_ifdup :: rest, v :: _ when is_zero v ->
        eval_main iflevel stack altstack rest
      | O Op_ifdup :: rest, v :: _ ->
        eval_main iflevel (v :: stack) altstack rest
      | O Op_depth :: rest, _ ->
        eval_main iflevel (Int (List.length stack |> Int32.of_int) :: stack) altstack rest
      | O Op_drop :: rest, [] ->
        invalid_arg "Run.eval: op_drop without a top stack element"
      | O Op_drop :: rest, v :: stack ->
        eval_main iflevel stack altstack rest
      | O Op_dup :: rest, [] ->
        invalid_arg "Run.eval: op_dup without a top stack element"
      | O Op_dup :: rest, v :: _ ->
        eval_main iflevel (v :: stack) altstack rest
      | O Op_nip :: rest, [] ->
        invalid_arg "Run.eval: op_nip without a top stack element"
      | O Op_nip :: rest, [v] ->
        invalid_arg "Run.eval: op_nip without at least two elements"
      | O Op_nip :: rest, x :: y :: stack ->
        eval_main iflevel (x :: stack) altstack rest
      | O Op_over :: rest, [] ->
        invalid_arg "Run.eval: op_over without a top stack element"
      | O Op_over :: rest, [v] ->
        invalid_arg "Run.eval: op_over without at least two elements"
      | O Op_over :: rest, _ :: x :: _ ->
        eval_main iflevel (x :: stack) altstack rest
      | _ -> invalid_arg "Run.eval: unsupported"
    in
    eval_main 0 [] [] code
end