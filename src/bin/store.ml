let () = Mirage_crypto_rng_lwt.initialize (module Mirage_crypto_rng.Fortuna)
let hash_size = 256

module Fixed_string = Index.Key.String_fixed (struct
  let length = 256
end)

module I = Index_unix.Make (Fixed_string) (Fixed_string) (Index.Cache.Noop)
module Secret = Capnp_rpc_net.Restorer.Id

type t = {
  store : I.t;
  make_sturdy : Secret.t -> Uri.t;
  load :
    validate:(unit -> bool) ->
    sturdy_ref:[ `Generic ] Capnp_rpc_lwt.Sturdy_ref.t ->
    Capnp_rpc_net.Restorer.resolution Lwt.t;
}

let create ~make_sturdy ~load path =
  let store = I.v ~log_size:4096 path in
  { store; make_sturdy; load }

let pad_name name =
  let diff = hash_size - String.length name in
  if diff >= 0 then String.make diff ' ' ^ name else failwith "Name too long!"

let add_client t name =
  let name = String.trim name in
  let secret = Secret.generate () in
  let hash = Secret.digest `SHA256 secret in
  let name = pad_name name in
  let store_secret = pad_name hash in
  I.replace t name store_secret;
  I.replace t store_secret name;
  I.merge t;
  secret

let lookup t name =
  let name = pad_name name in
  try Some (I.find t name) with Not_found -> None

let lookup_by_hash t digest =
  try Some (I.find t (pad_name digest)) with Not_found -> None

let remove t name =
  let name = String.trim name in
  let padded_name = pad_name name in
  I.filter t (fun (k, _) -> k = padded_name);
  I.merge t

let list t =
  let lst = ref [] in
  I.iter (fun k _ -> lst := String.trim k :: !lst) t;
  List.stable_sort String.compare !lst

module type T = Capnp_rpc_net.Restorer.LOADER

let hash _ = `SHA256
let make_sturdy t = t.make_sturdy

let validate t digest () =
  match lookup t.store digest with None -> false | Some _ -> true

let load t self digest =
  Logs.info (fun f -> f "Looking up %s" digest);
  match lookup_by_hash t.store digest with
  | None ->
      Logs.info (fun f -> f "Nothing found :(");
      Lwt.return Capnp_rpc_net.Restorer.unknown_service_id
  | Some _ ->
      t.load ~validate:(validate t digest)
        ~sturdy_ref:(Capnp_rpc_lwt.Sturdy_ref.cast self)
