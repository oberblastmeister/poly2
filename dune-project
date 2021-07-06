(lang dune 2.8)
(name poly2)
(using menhir 2.1)

(generate_opam_files true)

(package
  (name poly2)
  (depends
    (core (= v0.14.1))
    (ppx_deriving (= 5.2.1))
    (fmt (= 0.8.9+dune))
    (alcotest (= 1.4.0))
    (qcheck (= 0.17))))
