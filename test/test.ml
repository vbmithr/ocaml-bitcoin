open Bitcoin

module Util = struct
  open Util
  let test_keyPath_of_string () =
    let kp = KeyPath.of_string "44'/1'/0'/0/0" in
    assert (kp = [H 44l; H 1l; H 0l; N 0l; N 0l])

  let runtest = Util.[
      "KeyPath.of_string", `Quick, test_keyPath_of_string ;
    ]
end

let () =
  Alcotest.run "bitcoin" [
    "Util", Util.runtest ;
  ]
