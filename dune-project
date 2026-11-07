(lang dune 3.23)
(name cameleer2html)

(generate_opam_files true)

(source
 (github CameleerBook/Cameleer2Html))

(authors "Pedro Gasparinho")

(maintainers "Pedro Gasparinho")

(license LICENSE)

(package
 (name cameleer2html)
 (synopsis "An HTML generator for OCaml programs with GOSPEL annotations")
 (description "An HTML generator for OCaml programs with GOSPEL annotations")
 (depends
  (ocaml (>= 4.14)) (dune (>= 3.23))))