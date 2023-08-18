open Lwt.Infix
open Capnp_rpc_lwt

type t = Raw.Service.Job.t Capability.t

let ( >>!= ) = Lwt_result.bind

(* Turns off [switch] if [cancel] is called or the job is released. *)
let local ~switch ~outcome ~stream_log_data =
  let module X = Raw.Service.Job in
  X.local
  @@ object
       inherit X.service

       method log_impl params release_param_caps =
         let open X.Log in
         release_param_caps ();
         let start = Params.start_get params in
         Service.return_lwt @@ fun () ->
         stream_log_data ~start >|= fun (log, next) ->
         let response, results = Service.Response.create Results.init_pointer in
         Results.log_set results log;
         Results.next_set results next;
         Ok response

       method result_impl _params release_param_caps =
         let open X.Result in
         release_param_caps ();
         Service.return_lwt @@ fun () ->
         outcome >|= function
         | Error (`Msg m) ->
             Error (`Capnp (`Exception (Capnp_rpc.Exception.v m)))
         | Ok output ->
             let response, results =
               Service.Response.create Results.init_pointer
             in
             Results.output_set results output;
             Ok response

       method! release = Lwt.async (fun () -> Lwt_switch.turn_off switch)

       method cancel_impl _params release_param_caps =
         release_param_caps ();
         Lwt.async (fun () -> Lwt_switch.turn_off switch);
         let response = Service.Response.create_empty () in
         Service.return response
     end

module X = Raw.Client.Job

let log t start =
  let open X.Log in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.start_set params start;
  Capability.call_for_value t method_id request
  |> Lwt_result.map @@ fun x -> (Results.log_get x, Results.next_get x)

let result t =
  let open X.Result in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_value t method_id request >>!= fun response ->
  Lwt_result.return (Results.output_get response)

let cancel t =
  let open X.Cancel in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_unit t method_id request

module Log_data = struct
  open Lwt.Infix

  let max_chunk_size = 10240L

  type t = {
    data : Buffer.t;
    mutable cond : [ `Running of unit Lwt_condition.t | `Finished ];
  }

  let create () =
    { data = Buffer.create 10240; cond = `Running (Lwt_condition.create ()) }

  let rec stream t ~start =
    let len = Int64.of_int (Buffer.length t.data) in
    let start = if start < 0L then max 0L (Int64.add len start) else start in
    let avail = Int64.sub len start in
    if avail < 0L then Fmt.failwith "Start value out of range!";
    if avail = 0L then
      match t.cond with
      | `Running cond -> Lwt_condition.wait cond >>= fun () -> stream t ~start
      | `Finished -> Lwt.return ("", start)
    else
      let chunk = min avail max_chunk_size in
      let next = Int64.add start chunk in
      let start = Int64.to_int start in
      let chunk = Int64.to_int chunk in
      Lwt.return (Buffer.sub t.data start chunk, next)

  let write t data =
    match t.cond with
    | `Running cond ->
        Buffer.add_string t.data data;
        Lwt_condition.broadcast cond ()
    | `Finished -> Fmt.failwith "Attempt to write to log after close: %S" data

  let copy_from_stream t src =
    let rec aux () =
      Lwt_io.read ~count:4096 src >>= function
      | "" -> Lwt.return_unit
      | data ->
          write t data;
          aux ()
    in
    aux ()

  let close t =
    match t.cond with
    | `Running cond ->
        t.cond <- `Finished;
        Lwt_condition.broadcast cond ()
    | `Finished -> Fmt.failwith "Log already closed!"

  let info t fmt = Fmt.kstr (write t) (fmt ^^ "@.")
end
