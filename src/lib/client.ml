open Lwt.Infix
open Capnp_rpc_lwt

let ( >>!= ) = Lwt_result.bind

type t = Raw.Service.Client.t
type obuilder_spec = Obuilder_spec.t

let obuilder_spec_of_yojson = function
  | `String s ->
      Sexplib.Sexp.of_string s |> Obuilder_spec.t_of_sexp |> Result.ok
  | _ -> Error "Failed to parse obuilder spec"

let msg_err = function Ok v -> Ok v | Error s -> Error (`Msg s)

let cancelled_err = function
  | Ok v -> Ok v
  | Error `Cancelled -> Error (`Msg "Cancelled")
  | Error (`Msg m) -> Error (`Msg m)

let obuilder_spec_to_yojson spec =
  Obuilder_spec.sexp_of_t spec |> Sexplib.Sexp.to_string |> fun v -> `String v

type ctx = No_context | Git of string [@@deriving yojson]
type spec = Obuilder of obuilder_spec [@@deriving yojson]

let or_fail = function Ok v -> v | Error r -> failwith r

let pp_timestamp f x =
  let open Unix in
  let tm = localtime x in
  Fmt.pf f "%04d-%02d-%02d %02d:%02d.%02d" (tm.tm_year + 1900) (tm.tm_mon + 1)
    tm.tm_mday tm.tm_hour tm.tm_min tm.tm_sec

let with_ctx ~switch log _ fn =
  Lwt_io.with_temp_dir @@ fun src_dir ->
  let log_to log_data tag msg =
    match tag with
    | `Heading -> Job.Log_data.info log_data "\n\027[01;34m%s\027[0m" msg
    | `Note ->
        Job.Log_data.info log_data "\027[01;2m\027[01;35m%a %s\027[0m"
          pp_timestamp (Unix.gettimeofday ()) msg
    | `Output -> Job.Log_data.write log_data msg
  in
  let log = log_to log in
  let ctx = Obuilder.Context.v ~log ~switch ~src_dir () in
  fn ctx

let to_capnp_error = function
  | Ok v -> Ok v
  | Error `Cancelled -> Error (`Capnp `Cancelled)
  | Error (`Msg s) -> Error (`Capnp (`Exception (Capnp_rpc.Exception.v s)))

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
    Lwt_unix.socket ~cloexec:false Unix.PF_UNIX Unix.SOCK_STREAM 0
  in
  Lwt_unix.setsockopt fd_passer Unix.SO_REUSEADDR true;
  let unix_sock =
    let f = Filename.temp_file "runc-" ".sock" in
    Unix.unlink f;
    f
  in
  Logs.info (fun f -> f "New runc socket at %s" unix_sock);
  Lwt_unix.bind fd_passer (Unix.ADDR_UNIX unix_sock) >>= fun () ->
  Lwt_unix.listen fd_passer 5;
  Logs.info (fun f -> f "Starting a shell");
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
  let stdin msg =
    write_all console_fd (Bytes.of_string msg) 0 (String.length msg)
  in
  let stdout () = read_all console_fd in
  Lwt.return (Process.local ~stdin ~stdout ~stderr:stdout)

let v ?sr (Builder.Builder ((module B), v) as builder) =
  let module X = Raw.Service.Client in
  let make =
    match sr with
    | Some sr -> Capnp_rpc_lwt.Persistence.with_sturdy_ref sr
    | None -> Fun.id
  in
  make X.local
  @@ object
       inherit X.service

       method build_impl params release_param_caps =
         let open X.Build in
         let ctx =
           Params.ctx_get params |> Yojson.Safe.from_string |> ctx_of_yojson
           |> msg_err
         in
         let spec =
           Params.spec_get params |> Yojson.Safe.from_string |> spec_of_yojson
           |> msg_err
         in
         release_param_caps ();
         Service.return_lwt @@ fun () ->
         match (ctx, spec) with
         | Ok ctx, Ok (Obuilder spec) ->
             let res =
               let switch = Lwt_switch.create () in
               let log = Job.Log_data.create () in
               let outcome, set_outcome = Lwt.wait () in
               let job =
                 Job.local ~switch ~outcome
                   ~stream_log_data:(Job.Log_data.stream log)
               in
               let response, results =
                 Service.Response.create Results.init_pointer
               in
               Results.job_set results (Some job);
               Capability.inc_ref job;
               with_ctx ~switch log ctx @@ fun ctx ->
               Lwt.async (fun () ->
                   B.build v ctx spec >|= fun v ->
                   Lwt.wakeup_later set_outcome (cancelled_err v));
               Lwt.return_ok response
             in
             res >|= to_capnp_error
         | Error (`Msg e), _ ->
             Lwt.return @@ Error (`Capnp (`Exception (Capnp_rpc.Exception.v e)))
         | _, Error (`Msg e) ->
             Lwt.return @@ Error (`Capnp (`Exception (Capnp_rpc.Exception.v e)))

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

module X = Raw.Client.Client

let build t ctx spec =
  let open X.Build in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.ctx_set params (ctx_to_yojson ctx |> Yojson.Safe.to_string);
  Params.spec_set params (spec_to_yojson spec |> Yojson.Safe.to_string);
  Capability.call_for_caps t method_id request Results.job_get_pipelined

let shell t id =
  let open X.Shell in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.id_set params id;
  Capability.call_for_caps t method_id request Results.process_get_pipelined
