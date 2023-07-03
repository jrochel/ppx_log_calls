(lang dune 3.7)

(name ppx_log_calls)

(package
 (name ppx_log_calls)
 (version 0.1)
 (synopsis "Ppx deriver for logging function calls for debugging purposes")
 (description "By annotating function definitions with a %log extension point
               calls to these functions will be logged. The functions arguments
               are pretty-printed as well if explicit type annotations are given.
               The naming of pretty-printing functions is compatible with the
               ppx_deriving.show deriver.")
 (authors Jan Rochel)
 (maintainers "jan.rochel@cea.fr")
 (depends (ppxlib (>= 0.30.0))))


(generate_opam_files true)
