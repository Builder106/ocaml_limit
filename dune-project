(lang dune 3.16)

(name ocaml_lob)

(generate_opam_files true)

(source
 (github yinkavaughan/ocaml-limit-order-book))

(authors "Yinka Vaughan")

(maintainers "Yinka Vaughan")

(license MIT)

(documentation https://github.com/yinkavaughan/ocaml-limit-order-book)

(package
 (name ocaml_lob)
 (synopsis
  "High-performance limit order book and matching engine in OCaml 5")
 (description
  "A zero-allocation, O(log n) matching engine featuring iceberg orders, \
   pre-trade risk checks, and OCaml 5 multicore support via domain-per-symbol \
   architecture.")
 (depends
  (ocaml
   (>= 5.0))
  dune
  (alcotest
   (and
    :with-test
    (>= 1.7.0)))))
