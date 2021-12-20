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
 (synopsis
  "Biblioth\195\168que de fonctions utiles pour manipuler des programmes Mini-C")
 (description "")
 (depends
  (menhir
   (>= 2.1))))

(package
 (name minic)
 (version 1.0.0)
 (synopsis
  "Programme pour v\195\169rifier et interpr\195\168ter du code Mini-C")
 (description "")
 (depends
  (libminic
   (>= 1.0))))
