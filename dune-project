(lang dune 3.5)
(name lab-tools)
(version 3d39fae-dirty)

(generate_opam_files true)

(package
 (name lab-tools)
 (synopsis "FIXME:")
 (description "FIXME:")
 (authors "Mac Lee")
 (depends
  (ocaml (= "4.14.0"))
  (dune (>= "1.5"))
  (conf-rust-2021 (= "1"))
  (containers (= "3.10"))
  (cmdliner (= "1.1.0"))
  (vg (= "0.9.4"))
  (csv (= "2.4"))
  (bos (= "0.2.1"))
  (gg (= "1.0.0"))
  (ppx_blob (= "0.7.2"))))
