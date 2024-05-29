(lang dune 3.7)

(name sqitch_tool)

(generate_opam_files true)

(source
 (github brokenpylons/sqitch_tool))

(authors "Žiga Leber")

(maintainers "Žiga Leber")

(license MIT)

(documentation https://github.com/brokenpylons/sqitch_tool)

(package
 (name sqitch_tool)
 (synopsis "a squitch utility")
 (description "A utility for refactoring sqitch changes.")
 (depends ocaml dune fmt uutf fileutils cmdliner))
