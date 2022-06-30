(lang dune 2.9)

(name minic)

(using menhir 2.1)

(generate_opam_files true)

(source
 (github tretre91/Mini-C))

(authors "Tr\195\169vis Morvany")

(package
 (name libminic)
 (version 1.0.0)
 (synopsis "Collection of functions used by the Mini-C compiler")
 (description "")
 (depends
  (menhir
   (>= 2.1))))

(package
 (name minic)
 (version 1.0.0)
 (synopsis "A C to WebAssembly compiler")
 (description "")
 (depends
  (libminic
   (>= 1.0))
  (cmdliner
   (>= 1.0))))
