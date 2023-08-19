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
  let restore = Restorer.of_table services in
  let builder_id = Capnp_rpc_unix.Vat_config.derived_id capnp "builder" in
  let builder =
    let sr = Capnp_rpc_net.Restorer.Table.sturdy_ref services builder_id in
    Hoke.Client.v ~sr builder
  in
  Restorer.Table.add services builder_id builder;
  let admin_id = Capnp_rpc_unix.Vat_config.derived_id capnp "admin" in
  let admin =
    let sr = Capnp_rpc_net.Restorer.Table.sturdy_ref services admin_id in
    Admin.v sr restore store
  in
  Restorer.Table.add services admin_id admin;
  Capnp_rpc_unix.serve capnp ~restore >>= fun vat ->
  export ~secrets_dir ~vat ~name:"admin" admin_id;
  let v = Capnp_rpc_unix.Vat_config.sturdy_uri capnp admin_id in
  Logs.info (fun f -> f "E: %a" Uri.pp v);
  Logs.info (fun f -> f "Hoke running...");
  fst @@ Lwt.wait ()

let run cap_path fn =
  try
    Lwt_main.run
      (let vat = Capnp_rpc_unix.client_only_vat () in
       let sr = Capnp_rpc_unix.Cap_file.load vat cap_path |> or_fail in
       Capnp_rpc_unix.with_cap_exn sr fn)
  with Failure msg ->
    Printf.eprintf "%s\n%!" msg;
    exit 1

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

let connect_addr =
  Arg.required
  @@ Arg.opt Arg.(some file) None
  @@ Arg.info ~doc:"Path of submission.cap file from ocluster-scheduler."
       ~docv:"ADDR" [ "c"; "connect" ]

let git =
  Arg.value
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"A git remote to use as the context of a build." ~docv:"GIT"
       [ "g"; "git" ]

let id =
  Arg.required
  @@ Arg.opt Arg.(some string) None
  @@ Arg.info ~doc:"The obuilder ID of the image to shell." ~docv:"ID" [ "id" ]

let client_name =
  Arg.required
  @@ Arg.pos 0 Arg.(some string) None
  @@ Arg.info ~doc:"The name of the new client to add." ~docv:"NAME" []

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
      let load ~validate:_ ~sturdy_ref =
        let sr = Capnp_rpc_lwt.Sturdy_ref.cast sturdy_ref in
        Restorer.grant (Hoke.Client.v ~sr builder) |> Lwt.return
      in
      let loader = Store.create ~make_sturdy ~load "hoke.index" in
      let services = Restorer.Table.of_loader (module Store) loader in
      daemon capnp services builder loader.store "./secrets"
    in
    Lwt_main.run d
  in
  Cmd.v info
    Term.(
      const daemon $ setup_log $ Capnp_rpc_unix.Vat_config.cmd $ store
      $ Obuilder.Sandbox.cmdliner)

let build =
  let doc = "send a build spec to the daemon" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Send an Obuilder spec file to a daemon along with an optional git \
         context.";
    ]
  in
  let info = Cmd.info ~man "build" ~doc in
  let build () capnp ctx =
    run capnp @@ fun service ->
    let spec = In_channel.input_all In_channel.stdin in
    Client.build service ~ctx ~spec >|= fun t ->
    match t with
    | Ok t ->
        Logs.info (fun f -> f "[%a]: %s" Fmt.(styled `Green string) "SUCCESS" t)
    | Error _ ->
        Logs.err (fun f -> f "An error occurred");
        exit (-1)
  in
  Cmd.v info Term.(const build $ setup_log $ connect_addr $ git)

let add =
  let doc = "add a new client" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "Add a new client and get a capablity back to use for that client to \
         submit builds and connect to the daemon.";
    ]
  in
  let info = Cmd.info ~man "add" ~doc in
  let build () capnp name =
    let open Capnp_rpc_lwt in
    run capnp @@ fun service ->
    let cap = Hoke.Admin.add_client service name in
    Capability.with_ref cap @@ fun client ->
    Persistence.save_exn client >|= fun uri -> Fmt.pr "%a" Uri.pp uri
  in
  Cmd.v info Term.(const build $ setup_log $ connect_addr $ client_name)

let shell =
  let doc = "run a shell from a previously built container" in
  let man =
    [
      `S Manpage.s_description;
      `P "Connect to a previously built image and have a hoke about.";
    ]
  in
  let info = Cmd.info ~man "shell" ~doc in
  let build () capnp id = run capnp @@ fun service -> Client.shell service id in
  Cmd.v info Term.(const build $ setup_log $ connect_addr $ id)

let cmds = [ daemon; add; build; shell ]

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
