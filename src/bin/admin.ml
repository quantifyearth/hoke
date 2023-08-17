open Lwt.Infix
module Secret = Capnp_rpc_net.Restorer.Id

let add_client t restorer name =
  match Store.lookup t name with
  | Some _ -> Fmt.failwith "Client %s already exists!" name
  | None -> (
      let secret = Secret.generate () in
      Store.add_client t name (Secret.to_string secret);
      Capnp_rpc_net.Restorer.restore restorer secret >|= function
      | Ok v -> Ok v
      | Error exn -> Error (`Capnp (`Exception exn)))

let remove_client t name =
  Store.remove t name;
  Lwt.return_ok ()

let list_clients t = Store.list t |> Lwt.return

let v restorer t =
  let add_client = add_client t restorer in
  let remove_client = remove_client t in
  let list_clients () = list_clients t in
  Hoke.Admin.v ~add_client ~remove_client ~list_clients
