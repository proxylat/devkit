(lang dune 3.24)

(name devkit)

(generate_opam_files true)

(source (github anomalyco/devkit))
(license MIT)
(authors "devkit contributors")
(maintainers "devkit contributors")

(package
 (name devkit)
 (synopsis "Cross package-manager inventory dashboard")
 (description "Scan your PC for installed software across package managers, merge the result with a devkit.toml manifest, and export a restore-ready snapshot.")
 (depends
  (ocaml (>= 5.2))
  (dune (>= 3.24))
  (cmdliner (>= 1.2))
  (yojson (>= 2.0))
  (digestif (>= 1.2))
  (decompress (>= 1.5))
  (sexplib (>= 0.17))
   (re (>= 1.11))
    (toml (>= 7.0))
    (lambda-term (>= 3.0))
    (lwt (>= 5.0))
  (alcotest (and (>= 1.7) :with-test))))
