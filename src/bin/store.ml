let () = Mirage_crypto_rng_lwt.initialize (module Mirage_crypto_rng.Fortuna)

module Hash = struct
  module H = Digestif.SHA256

  let size = H.digest_size
  let digest s = H.digest_string s |> H.to_raw_string
end

module Fixed_string = Index.Key.String_fixed (struct
  let length = Hash.size
end)

module I = Index_unix.Make (Fixed_string) (Fixed_string) (Index.Cache.Noop)
module Secret = Capnp_rpc_net.Restorer.Id

type t = {
  store : I.t;
  make_sturdy : Secret.t -> Uri.t;
  load : validate:(unit -> bool) -> Capnp_rpc_net.Restorer.resolution Lwt.t;
}

let create ~make_sturdy ~load path =
  let store = I.v ~log_size:4096 path in
  { store; make_sturdy; load }

(* Hopefully this is never a secret *)
let none = Hash.digest "NONE!!!"

let pad_name name =
  let diff = Hash.size - String.length name in
  if diff >= 0 then String.make diff ' ' ^ name else failwith "Name too long!"

let add_client t name secret =
  let name = String.trim name in
  let name = pad_name name in
  let uri = Hash.digest secret in
  I.replace t name uri;
  I.merge t

let lookup t name =
  let name = pad_name name in
  match try Some (I.find t name) with Not_found -> None with
  | Some digest when digest = none -> None
  | d -> d

let remove t name =
  let name = String.trim name in
  let name = pad_name name in
  I.replace t (Hash.digest name) none;
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

let load t _ digest =
  match lookup t.store digest with
  | None -> Lwt.return Capnp_rpc_net.Restorer.unknown_service_id
  | Some _ -> t.load ~validate:(validate t digest)
