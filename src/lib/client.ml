open Lwt.Infix
open Capnp_rpc_lwt

let ( >>!= ) = Lwt_result.bind

type t = Raw.Service.Client.t
type obuilder_spec = Obuilder_spec.t

let obuilder_spec_of_yojson = function
  | `String s ->
      Sexplib.Sexp.of_string s |> Obuilder_spec.t_of_sexp |> Result.ok
  | _ -> Error "Failed to parse obuilder spec"

let obuilder_spec_to_yojson spec =
  Obuilder_spec.sexp_of_t spec |> Sexplib.Sexp.to_string |> fun v -> `String v

type ctx = No_context | Git of string [@@deriving yojson]
type spec = Obuilder of obuilder_spec [@@deriving yojson]

let or_fail = function Ok v -> v | Error r -> failwith r

let with_ctx log _ fn =
  Lwt_io.with_temp_dir @@ fun src_dir ->
  let ctx = Obuilder.Context.v ~log ~src_dir () in
  fn ctx

let to_capnp_error = function
  | Ok v -> Ok v
  | Error `Cancelled -> Error (`Capnp `Cancelled)
  | Error (`Msg s) -> Error (`Capnp (`Exception (Capnp_rpc.Exception.v s)))

let logger log tag msg =
  let module Log = Raw.Client.Logger.Log in
  let logger s =
    let request, params = Capability.Request.create Log.Params.init_pointer in
    Log.Params.msg_set params s;
    Lwt.async @@ fun () ->
    Capnp_rpc_lwt.Capability.call_for_unit_exn log
      Raw.Client.Logger.Log.method_id request
  in

  match tag with
  | `Heading ->
      Fmt.str "%a@." Fmt.(styled (`Fg (`Hi `Blue)) string) msg |> logger
  | `Note -> Fmt.str "%a@." Fmt.(styled (`Fg `Yellow) string) msg |> logger
  | `Output -> Fmt.str "%s@." msg |> logger

let read_all fd : string Lwt.t =
  let open Lwt.Infix in
  let bytes = Bytes.create 1024 in
  (* Slow and bad copying... *)
  let buf = Buffer.create 1024 in
  let rec loop () =
    Lwt_unix.read fd bytes 0 1024 >>= fun v ->
    match v with
    | 0 | (exception End_of_file) -> Lwt.return @@ Buffer.contents buf
    | i ->
        Buffer.add_bytes buf (Bytes.sub bytes 0 i);
        loop ()
  in
  loop ()

let rec write_all fd buf ofs len =
  let open Lwt.Infix in
  assert (len >= 0);
  if len = 0 then Lwt.return_unit
  else
    Lwt_unix.write fd buf ofs len >>= fun n ->
    write_all fd buf (ofs + n) (len - n)

let shell (Builder.Builder ((module B), b)) id =
  let open Lwt.Infix in
  Logs.info (fun f -> f "Starting new runc shell");
  let fd_passer =
    Lwt_unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  Lwt_unix.setsockopt fd_passer Unix.SO_REUSEADDR true;
  let unix_sock = Filename.temp_file "runc-" ".sock" in
  Lwt_unix.bind fd_passer (Unix.ADDR_UNIX unix_sock) >>= fun () ->
  Lwt_unix.listen fd_passer 5;
  let established, _exited = B.shell ~unix_sock b id in
  established >>= fun () ->
  (* Do IOVecs need to be non-empty? *)
  let destination_buffer = Bytes.create 4096 in
  let io_vectors = Lwt_unix.IO_vectors.create () in
  Lwt_unix.IO_vectors.append_bytes io_vectors destination_buffer 0 4096;
  Lwt_unix.accept fd_passer >>= fun (socket, _) ->
  Lwt_unix.recv_msg ~socket ~io_vectors >>= fun (_, fds) ->
  let console_fd_unix = List.hd fds in
  let console_fd = Lwt_unix.of_unix_file_descr console_fd_unix in
  let stdout msg =
    write_all console_fd (Bytes.of_string msg) 0 (String.length msg)
  in
  let stdin () = read_all console_fd in
  Lwt.return (Process.local ~stdin ~stdout ~stderr:(fun _ -> Lwt.return_unit))

let v (Builder.Builder ((module B), v) as builder) =
  let module X = Raw.Service.Client in
  X.local
  @@ object
       inherit X.service

       method build_impl params release_param_caps =
         let open X.Build in
         let ctx =
           Params.ctx_get params |> Yojson.Safe.from_string |> ctx_of_yojson
           |> or_fail
         in
         let spec =
           Params.ctx_get params |> Yojson.Safe.from_string |> spec_of_yojson
           |> or_fail
         in
         match Params.logger_get params with
         | None -> Service.fail "No logger!"
         | Some log ->
             release_param_caps ();
             let spec = match spec with Obuilder spec -> spec in
             Service.return_lwt @@ fun () ->
             Capability.with_ref log @@ fun log ->
             let res =
               let log = logger log in
               with_ctx log ctx @@ fun ctx ->
               B.build v ctx spec >>!= fun id ->
               let response, results =
                 Service.Response.create Results.init_pointer
               in
               Results.id_set results id;
               Lwt.return_ok response
             in
             res >|= to_capnp_error

       method shell_impl params release_param_caps =
         let open X.Shell in
         let id = Params.id_get params in
         release_param_caps ();
         Service.return_lwt @@ fun () ->
         (* Probably need to do better resource management here,
            but we move fast and sweep these problems under the rug. *)
         let res =
           shell builder id >>= fun proc ->
           let response, results =
             Service.Response.create Results.init_pointer
           in
           Results.process_set results (Some proc);
           Lwt.return_ok response
         in
         res >|= to_capnp_error
     end

(* module X = Raw.Client.Client

   let stdout t ~data =
     let open X.Shell in
     let request, params = Capability.Request.create Params.init_pointer in
     Params.id_set params data;
     Capability.call_for_unit_exn t method_id request

   let stderr t ~data =
     let open X.Stderr in
     let request, params = Capability.Request.create Params.init_pointer in
     Params.data_set params data;
     Capability.call_for_unit_exn t method_id request

   let stdin t =
     let open X.Stdin in
     let request = Capability.Request.create_no_args () in
     Capability.call_for_unit_exn t method_id request >>= fun result ->
     Lwt.return_ok @@ Results.data_get result *)
