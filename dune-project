(lang dune 3.17)

(name lens)

(generate_opam_files true)

(source
 (github Mroik/ocaml-lens))

(authors "Mroik <mroik@delayed.space")

(maintainers "Mroik <mroik@delayed.space")

(license WTFPL)

(package
 (name lens)
 (synopsis "An attempt to replicate APL's ⍢(under) operator.")
 (description "An attempt to replicate APL's ⍢(under) operator.")
 (depends ocaml)
 (tags
  (lens)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
