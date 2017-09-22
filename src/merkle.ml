open Util
open P2p

type t =
  | Empty
  | Node of t * Hash256.t * t

let node l h r = Node (l, h, r)
let leaf h = Node (Empty, h, Empty)

let compute a b = match a,b with
  | Node (_, h1, _), Node (_, h2, _) ->
    Hash256.compute_concat h1 h2
  | _ -> invalid_arg "Merkle.compute"

let depth n =
  let rec inner acc n =
    if n = 0 then acc else inner (succ acc) (n/2)
  in
  inner 0 (if n mod 2 = 0 then n else succ n)

let verify max_depth hashes flags =
  let rec inner depth hashes flags =
    match flags, depth, hashes with
    | false :: flags, _, h :: hashes ->
      node Empty h Empty, hashes, flags
    | true :: flags, _, _ when depth < max_depth ->
      let l, hashes, flags = inner (succ depth) hashes flags in
      let r, hashes, flags = inner (succ depth) hashes flags in
      node l (compute l r) r, hashes, flags
    | true :: flags, _, h :: hashes ->
      leaf h, hashes, flags
    | _ -> invalid_arg "Merkle.verify"
  in
  inner 0 hashes flags

let verify { MerkleBlock.header ; txn_count ; hashes ; flags } =
  let flags = Bitv.to_bool_list flags in
  let depth = depth txn_count in
  match verify depth hashes flags with
  | Node (_, h, _), _, _ -> Hash256.equal header.merkle_root h
  | _ -> invalid_arg "Merkle.verify"
