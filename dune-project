(lang dune 3.16)

(name bitcoin)

(generate_opam_files true)

(source
 (github vbmithr/ocaml-bitcoin))

(authors "Vincent Bernardoff <vb@luminar.eu.org>")

(maintainers "Vincent Bernardoff")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name bitcoin)
 (synopsis "Bitcoin library")
 (description "")
 (depends
   ocaml
   dune
   fmt
   cstruct
   cstruct-sexp
   sexplib
   rresult
   stdint
   ipaddr
   ipaddr-sexp
   ptime
   hex
   bitv
   murmur3
   digestif
   secp256k1-internal
   base58
  (alcotest :with-test)
 )
 (tags (crypto bitcoin)))

(package
 (name bitcoin-node)
 (synopsis "Bitcoin node implementation with Async")
 (description "")
 (depends
   ocaml
   dune
   bitcoin
   core
   async
   uri
   base58
 )
 (tags (crypto bitcoin)))
