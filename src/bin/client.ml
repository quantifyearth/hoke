open Lwt.Infix

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

let rec tail job start =
  Hoke.Job.log job start >>= function
  | Error (`Capnp e) ->
      Fmt.failwith "Error tailing logs: %a" Capnp_rpc.Error.pp e
  | Ok ("", _) -> Lwt.return_unit
  | Ok (data, next) ->
      output_string stdout data;
      flush stdout;
      tail job next

let build submission_cap ~ctx ~spec =
  let ctx =
    match ctx with
    | Some s -> Hoke.Client.Git s
    | None -> Hoke.Client.No_context
  in
  let spec =
    let spec = Obuilder_spec.t_of_sexp (Sexplib.Sexp.of_string spec) in
    Hoke.Client.Obuilder spec
  in
  let job = Hoke.Client.build submission_cap ctx spec in
  let res = Hoke.Job.result job in 
  tail job 0L >>= fun () ->
  res

let shell submission_cap id =
  let process = Hoke.Client.shell submission_cap id in
  let stdin = Hoke.Process.stdin process in
  let stdout = Hoke.Process.stdout process in
  let rec stdout_loop () =
    stdout () >>= fun s ->
    write_all Lwt_unix.stdout (Bytes.of_string s) 0 (String.length s)
    >>= stdout_loop
  in
  let rec stdin_loop () =
    read_all Lwt_unix.stdin >>= fun data -> stdin ~data >>= stdin_loop
  in
  Lwt.choose [ stdout_loop (); stdin_loop () ]
