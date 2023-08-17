open Lwt.Infix
module Restorer = Capnp_rpc_net.Restorer

let ( / ) = Filename.concat
let or_fail = function Ok v -> v | Error (`Msg m) -> failwith m

let export ~secrets_dir ~vat ~name id =
  let path = secrets_dir / (name ^ ".cap") in
  Capnp_rpc_unix.Cap_file.save_service vat id path |> or_fail;
  Logs.app (fun f -> f "Wrote capability reference to %S" path)

let create_builder store_spec conf =
  let open Obuilder in
  store_spec >>= fun (Obuilder.Store_spec.Store ((module Store), store)) ->
  let module Builder = Obuilder.Builder (Store) (Sandbox) (Docker) in
  Sandbox.create ~state_dir:(Store.state_dir store / "sandbox") conf
  >|= fun sandbox ->
  let builder = Builder.v ~store ~sandbox in
  Hoke.Builder.Builder ((module Builder), builder)

let daemon capnp services builder store secrets_dir =
  let admin_id = Capnp_rpc_unix.Vat_config.derived_id capnp "admin" in
  let restore = Restorer.of_table services in
  let admin = Admin.v restore store in
  Restorer.Table.add services admin_id admin;
  let builder_id = Capnp_rpc_unix.Vat_config.derived_id capnp "builder" in
  let builder = Hoke.Client.v builder in
  Restorer.Table.add services builder_id builder;
  Capnp_rpc_unix.serve capnp ~restore >>= fun vat ->
  export ~secrets_dir ~vat ~name:"admin" admin_id;
  Logs.info (fun f -> f "Hoke running...");
  fst @@ Lwt.wait ()

open Cmdliner

let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Logs.Src.set_level Obuilder.log_src level;
  Logs.set_reporter (Logs_fmt.reporter ());
  ()

let setup_log =
  let docs = Manpage.s_common_options in
  Term.(
    const setup_log $ Fmt_cli.style_renderer ~docs () $ Logs_cli.level ~docs ())

let store = Obuilder.Store_spec.cmdliner

let daemon =
  let doc = "run the hoke daemon" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "The hoke daemon provides a simple way to hook up an OBuilder instance \
         to a capnp services that can allow users to submit builder \
         specifications and also run a shell inside an already built \
         container.";
    ]
  in
  let info = Cmd.info ~man "daemon" ~doc in
  let daemon () capnp store sandbox =
    let d =
      create_builder store sandbox >>= fun builder ->
      let make_sturdy = Capnp_rpc_unix.Vat_config.sturdy_uri capnp in
      let load ~validate:_ =
        Restorer.grant (Hoke.Client.v builder) |> Lwt.return
      in
      let loader = Store.create ~make_sturdy ~load "hoke.index" in
      let services = Restorer.Table.of_loader (module Store) loader in
      daemon capnp services builder loader.store ".secrets"
    in
    Lwt_main.run d
  in
  Cmd.v info
    Term.(
      const daemon $ setup_log $ Capnp_rpc_unix.Vat_config.cmd $ store
      $ Obuilder.Sandbox.cmdliner)

let cmds = [ daemon ]

let () =
  let doc = "Hoke" in
  let man =
    [
      `S Manpage.s_authors;
      `P "Patrick Ferris";
      `P
        "Thank you to the maintainers of the various capnp OCurrent services, \
         without which I would never haver worked out how to build hoke.";
      `S Manpage.s_bugs;
      `P "Email bug reports to <patrick@sirref.org>.";
    ]
  in
  let info = Cmd.info ~doc ~man "hoke" in
  exit (Cmd.eval @@ Cmd.group info cmds)
