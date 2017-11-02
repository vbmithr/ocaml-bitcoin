open Bitcoin

let assert_equal a b = assert (a = b)

module Util = struct
  open Util
  let test_keyPath_of_string () =
    let kp = KeyPath.of_string "44'/1'/0'/0/0" in
    assert_equal kp [H 44l; H 1l; H 0l; N 0l; N 0l]

  let verify_size () =
    assert_equal (String.length (Hash160.(compute_string "" |> to_string))) Hash160.length

  let runtest = [
    "KeyPath.of_string", `Quick, test_keyPath_of_string ;
    "Hash160.{of_string,to_string}", `Quick, verify_size ;
  ]
end

let rawTx = `Hex "0100000002ba0eb35fa910ccd759ff46b5233663e96017e8dfaedd315407dc5be45d8c260f000000001976a9146ce472b3cfced15a7d50b6b0cd75a3b042554e8e88acfdffffff69c84956a9cc0ec5986091e1ab229e1a7ea6f4813beb367c01c8ccc708e160cc000000001976a9146ce472b3cfced15a7d50b6b0cd75a3b042554e8e88acfdffffff01a17c0100000000001976a914efd0919fc05311850a8382b9c7e80abcd347343288ac00000000"
let keyPath = "44'/1'/0'/0/0"
let rawPrevTxs = [ `Hex "010000000324c6fae955eae55c27639e5537d00e6ef11559c26f9c36c6770030b38702b19b0d0000006b483045022100c369493b6caa7016efd537eedce8d9e44fe14c345cd5edbb8bdca5545daf4cbe022053ac076f1c04f2f10f107f2890d5d95513547690b9a27d647d1c1ea68f6f3512012102f812962645e606a97728876d93324f030c1fe944d58466960104d810e8dc4945ffffffff24c6fae955eae55c27639e5537d00e6ef11559c26f9c36c6770030b38702b19b0a0000006b48304502210094f901df086a6499f24f678eef305e81eed11d730696cfa23cf1a9e2208ab98302205e628d259e2450d71d67ad54a58b0f58d6b643b70957c8a72e8df1293b2eb9be012102f812962645e606a97728876d93324f030c1fe944d58466960104d810e8dc4945ffffffff24c6fae955eae55c27639e5537d00e6ef11559c26f9c36c6770030b38702b19b0c0000006a47304402205c59502f9075f764dad17d59da9eb5429e969e2608ab579e3185f639dfda2eee0220614d2101e2c17612dc59a247f6f5cbdefcd7ea8f74654caa08b11f42873e586201210268a925507fd7e84295e172b3eea2f056c166ddc874fcda45864d872725094225ffffffff0150c30000000000001976a9146ce472b3cfced15a7d50b6b0cd75a3b042554e8e88ac00000000" ;
                   `Hex "0100000001df5401686b5608195037e8978f6775db0c59d6cee8bb82aa25f4d8635481f56f010000006a47304402201d43a31c9d0f23f2bf2d39ae6d03ff217cb8bf7ddc7c5b1725f6f2f98d855b0c0220459426150782b01ca75958428e34f5e345e85ccae4333025eeb9baef85b3f9fc0121024bb68261bac7e49c99ad1e52fb5e91f09973d45f5d24715c9e64582a24856cc3ffffffff0260ea0000000000001976a9146ce472b3cfced15a7d50b6b0cd75a3b042554e8e88ac91351900000000001976a914efd0919fc05311850a8382b9c7e80abcd347343288ac00000000"
                 ]
let testTx =
  `Hex "01000000017b1eabe0209b1fe794124575ef807057c77ada2138ae4f\
        a8d6c4de0398a14f3f00000000494830450221008949f0cb400094ad\
        2b5eb399d59d01c14d73d8fe6e96df1a7150deb388ab893502207965\
        6090d7f6bac4c9a94e0aad311a4268e082a725f8aeae0573fb12ff86\
        6a5f01ffffffff01f0ca052a010000001976a914cbc20a7664f2f69e\
        5355aa427045bc15e7c6c77288ac00000000"

module Script = struct
  open Script

  let check_opcode i =
      let a = Opcode.of_int i in
      let b = Opcode.to_int a in
      if b <> i then begin
        failwith (Printf.sprintf "Problem at index %d" i)
      end

  let test_opcodes () =
    for i = 0 to 185 do check_opcode i done ;
    for i = 253 to 255 do check_opcode i done

  let runtest = [
    "Opcode.{of,to}_int", `Quick, test_opcodes ;
  ]
end

module Transaction = struct
  open Protocol

  let test_transaction () =
    let print_tx ((`Hex tx_hex) as tx) =
      let tx_cstruct = Hex.to_cstruct tx in
      let tx, _ = Transaction.of_cstruct tx_cstruct in
      let len = Transaction.size tx in
      let buf = Cstruct.create len in
      let (_:Cstruct.t) = Transaction.to_cstruct buf tx in
      let tx_trip, _ = Transaction.of_cstruct buf in
      (* let `Hex tx_hex' = Hex.of_cstruct buf in *)
      if not (Cstruct.equal tx_cstruct buf) then begin
        Printf.printf "%s\n\n%!" (Sexplib.Sexp.to_string_hum (Transaction.sexp_of_t tx)) ;
        Printf.printf "%s\n%!" (Sexplib.Sexp.to_string_hum (Transaction.sexp_of_t tx_trip)) ;
        failwith "trip did not succeed"
      end
    in
    List.iter print_tx (rawTx :: rawPrevTxs)

  let runtest = [
    "Transaction.of_cstruct", `Quick, test_transaction ;
  ]
end

let () =
  Alcotest.run "bitcoin" [
    "Util", Util.runtest ;
    "Script", Script.runtest ;
    "Transaction", Transaction.runtest ;
  ]
