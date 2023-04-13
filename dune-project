(lang dune 2.1)

(name Simplifier)

(generate_opam_files true)

; (source
;  (github username/reponame))

(authors "Israel Fuller, Eric Petron, Riley Thompson")

; (maintainers "Maintainer Name")
; 
; (license LICENSE)
; 
; (documentation https://url/to/documentation)

(package
 (name Simplifier)
 (synopsis "A solver for differential equations")
 (description "Simplifies expressions by applying rewrite rules from left to right. This is intended to help solve differential equations.")
 (depends ocaml dune)
 ;(tags
 ; (topics "to describe" your project))
  )

(using menhir 2.0)
; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
