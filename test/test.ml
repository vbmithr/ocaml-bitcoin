open Alcotest
open Bitcoin

module TestUtil = struct
  open Util

  let verify_size () =
    check
      int
      "size"
      Hash160.length
      (String.length Hash160.(compute_string "" |> to_string))
  ;;

  let runtest = [ test_case "Hash160.{of_string,to_string}" `Quick verify_size ]
end

let rawTx =
  `Hex
    "0100000002ba0eb35fa910ccd759ff46b5233663e96017e8dfaedd315407dc5be45d8c260f000000001976a9146ce472b3cfced15a7d50b6b0cd75a3b042554e8e88acfdffffff69c84956a9cc0ec5986091e1ab229e1a7ea6f4813beb367c01c8ccc708e160cc000000001976a9146ce472b3cfced15a7d50b6b0cd75a3b042554e8e88acfdffffff01a17c0100000000001976a914efd0919fc05311850a8382b9c7e80abcd347343288ac00000000"
;;

let keyPath = "44'/1'/0'/0/0"

let rawPrevTxs =
  [ `Hex
      "010000000324c6fae955eae55c27639e5537d00e6ef11559c26f9c36c6770030b38702b19b0d0000006b483045022100c369493b6caa7016efd537eedce8d9e44fe14c345cd5edbb8bdca5545daf4cbe022053ac076f1c04f2f10f107f2890d5d95513547690b9a27d647d1c1ea68f6f3512012102f812962645e606a97728876d93324f030c1fe944d58466960104d810e8dc4945ffffffff24c6fae955eae55c27639e5537d00e6ef11559c26f9c36c6770030b38702b19b0a0000006b48304502210094f901df086a6499f24f678eef305e81eed11d730696cfa23cf1a9e2208ab98302205e628d259e2450d71d67ad54a58b0f58d6b643b70957c8a72e8df1293b2eb9be012102f812962645e606a97728876d93324f030c1fe944d58466960104d810e8dc4945ffffffff24c6fae955eae55c27639e5537d00e6ef11559c26f9c36c6770030b38702b19b0c0000006a47304402205c59502f9075f764dad17d59da9eb5429e969e2608ab579e3185f639dfda2eee0220614d2101e2c17612dc59a247f6f5cbdefcd7ea8f74654caa08b11f42873e586201210268a925507fd7e84295e172b3eea2f056c166ddc874fcda45864d872725094225ffffffff0150c30000000000001976a9146ce472b3cfced15a7d50b6b0cd75a3b042554e8e88ac00000000"
  ; `Hex
      "0100000001df5401686b5608195037e8978f6775db0c59d6cee8bb82aa25f4d8635481f56f010000006a47304402201d43a31c9d0f23f2bf2d39ae6d03ff217cb8bf7ddc7c5b1725f6f2f98d855b0c0220459426150782b01ca75958428e34f5e345e85ccae4333025eeb9baef85b3f9fc0121024bb68261bac7e49c99ad1e52fb5e91f09973d45f5d24715c9e64582a24856cc3ffffffff0260ea0000000000001976a9146ce472b3cfced15a7d50b6b0cd75a3b042554e8e88ac91351900000000001976a914efd0919fc05311850a8382b9c7e80abcd347343288ac00000000"
  ]
;;

let testTx =
  `Hex
    "01000000017b1eabe0209b1fe794124575ef807057c77ada2138ae4fa8d6c4de0398a14f3f00000000494830450221008949f0cb400094ad2b5eb399d59d01c14d73d8fe6e96df1a7150deb388ab8935022079656090d7f6bac4c9a94e0aad311a4268e082a725f8aeae0573fb12ff866a5f01ffffffff01f0ca052a010000001976a914cbc20a7664f2f69e5355aa427045bc15e7c6c77288ac00000000"
;;

let hex = testable Hex.pp ( = )
let cstruct = testable Cstruct.hexdump_pp Cstruct.equal

module TestScript = struct
  let script = testable Script.pp ( = )

  let scripts =
    List.map
      Cstruct.of_hex
      [ "004730440220689033c6b759eafaeb2cec9840889b11d91bbd5c0bf7ca1cc5c1aeb472d6ef830220031592f971bf2e7e28d61b8211e1cbdac2fb2d845c5e4966051525e585bedbc9014830450221009427f4b53eae2b422985a719d6d4a7ffd855d05b5bac30721576165191f6bd4102204390383f80df68bd6f235b21a04c03190a608e73f417ae9432bb19ce081fc348014c69522102ab6a688dac39dbf7720e8acc35dd60c9859ac9fe028153bb86cbcd49efe5298a2102afec872249e4cb6d7defa91d2bacba96124acb35ace1f1e791e216381abace9721039b35123f8e66a2f226230d4fa59fb6e6c5c0a5195f5d404a72b73566e08fcb5753ae"
      ]
  ;;

  let round () =
    List.iter
      (fun cs ->
         let s, _ = Script.of_cstruct cs in
         Format.eprintf "%a@." Script.pp s;
         let cs' = Script.serialize s in
         let s', _ = Script.of_cstruct cs' in
         check script "type equality" s s';
         check cstruct "string equality" cs cs')
      scripts
  ;;

  open Script

  let check_opcode i =
    let a = Opcode.of_int i in
    let b = Opcode.to_int a in
    if b <> i then failwith (Printf.sprintf "Problem at index %d" i)
  ;;

  let test_opcodes () =
    for i = 0 to 185 do
      check_opcode i
    done;
    for i = 253 to 255 do
      check_opcode i
    done
  ;;

  let runtest =
    [ test_case "Opcode.{of,to}_int" `Quick test_opcodes; test_case "trip" `Quick round ]
  ;;
end

module TestTransaction = struct
  open Protocol

  let transaction = testable Transaction.pp ( = )

  let hash256 =
    let open Util.Hash256 in
    testable pp equal
  ;;

  let txs =
    [ ( Util.Hash256.of_hex_rpc
          (`Hex "0ae0a4865e68a12d4a54c8293329fd8a56ff2a2c72167a7aa828d8f1b68f4367")
      , `Hex
          "0100000001b5b6d3c4cbe2152001da0fe745202b5ae1676bf5616907c2b2661ea8a928f75b00000000fdfd00004730440220689033c6b759eafaeb2cec9840889b11d91bbd5c0bf7ca1cc5c1aeb472d6ef830220031592f971bf2e7e28d61b8211e1cbdac2fb2d845c5e4966051525e585bedbc9014830450221009427f4b53eae2b422985a719d6d4a7ffd855d05b5bac30721576165191f6bd4102204390383f80df68bd6f235b21a04c03190a608e73f417ae9432bb19ce081fc348014c69522102ab6a688dac39dbf7720e8acc35dd60c9859ac9fe028153bb86cbcd49efe5298a2102afec872249e4cb6d7defa91d2bacba96124acb35ace1f1e791e216381abace9721039b35123f8e66a2f226230d4fa59fb6e6c5c0a5195f5d404a72b73566e08fcb5753aeffffffff02400d0300000000001976a914e825af66403780479d8bfa4cf2e956623ed7f34a88acb3545c020000000017a91471c6a5ec5d76727767e3da0ac36e1f13db459f268700000000"
      )
    ]
  ;;

  let trip () =
    List.iter
      (fun (h, tx_hex) ->
         let t = Transaction.of_hex tx_hex in
         let h' = Transaction.hash256 t in
         check hash256 "hash" h h';
         let tx_hex' = Transaction.to_hex t in
         let t' = Transaction.of_hex tx_hex' in
         check transaction "trip_t" t t';
         check hex "trip_t_string" tx_hex tx_hex')
      txs
  ;;

  let test_transaction () =
    let print_tx (`Hex _tx_hex as tx) =
      let tx_cstruct = Hex.to_cstruct tx in
      let tx, _ = Transaction.of_cstruct tx_cstruct in
      let len = Transaction.size tx in
      let buf = Cstruct.create len in
      let (_ : Cstruct.t) = Transaction.to_cstruct buf tx in
      let tx_trip, _ = Transaction.of_cstruct buf in
      (* let `Hex tx_hex' = Hex.of_cstruct buf in *)
      if not (Cstruct.equal tx_cstruct buf)
      then (
        Printf.printf "%s\n\n%!" (Sexplib.Sexp.to_string_hum (Transaction.sexp_of_t tx));
        Printf.printf
          "%s\n%!"
          (Sexplib.Sexp.to_string_hum (Transaction.sexp_of_t tx_trip));
        failwith "trip did not succeed")
    in
    List.iter print_tx (rawTx :: rawPrevTxs)
  ;;

  let runtest =
    [ test_case "trip" `Quick trip
    ; test_case "Transaction.of_cstruct" `Quick test_transaction
    ]
  ;;
end

(* module Wallet = struct
 *   let test_keyPath_of_string () =
 *     let open Wallet.KeyPath in
 *     let kp = of_string_exn "44'/1'/0'/0/0" in
 *     assert_equal kp [to_hardened 44l; to_hardened 1l; to_hardened 0l; 0l; 0l]
 * 
 *   let runtest = [
 *     "KeyPath.of_string", `Quick, test_keyPath_of_string ;
 *   ]
 * end *)

let () =
  run
    "bitcoin"
    [ "Util", TestUtil.runtest
    ; "Script", TestScript.runtest
    ; "Transaction", TestTransaction.runtest (* "Wallet", Wallet.runtest ; *)
    ]
;;
