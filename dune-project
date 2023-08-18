(lang dune 3.9)

(name hoke)

(generate_opam_files true)
(source (github patricoferris/hoke))
(license MIT)
(authors "Patrick Ferris")
(maintainers "patrick@sirref.org")

(package
 (name hoke)
 (synopsis "A simple daemon for OBuilder")
 (tags ("sandbox" "docker-build"))
 (description "A simple daemon that uses capnp to allow clients to send build jobs and run shell on a remote obuilder instance.")
 (depends
   obuilder
   capnp-rpc-unix
   capnp-rpc-lwt
   index
   yojson
   ppx_deriving_yojson))