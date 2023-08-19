open Lwt.Infix

let b = Bytes.create 128

let debug s =
  Out_channel.with_open_gen
    [ Open_creat; Open_append; Open_binary ]
    0o644 "./debug.txt"
  @@ fun oc -> Out_channel.output_string oc s

let read () =
  Lwt.catch
    (fun () ->
      Lwt_unix.read Lwt_unix.stdin b 0 128 >|= fun i ->
      Some (Bytes.sub_string b 0 i))
    (function End_of_file -> Lwt.return None | exn -> Lwt.fail exn)

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
  tail job 0L >>= fun () -> res

let shell submission_cap id =
  let process = Hoke.Client.shell submission_cap id in
  let stdin = Hoke.Process.stdin process in
  let stdout = Hoke.Process.stdout process in
  let saved_tio = Unix.tcgetattr Unix.stdin in
  let tio = { saved_tio with (* input modes *)
                             c_istrip = false } in
  Unix.tcsetattr Unix.stdin TCSADRAIN tio;
  let buff = Buffer.create 8 in
  let rec stdout_loop () =
    stdout () >>= fun s ->
    (* Stdout seems to echo back the command, remove it and clear buffer *)
    if String.equal s (Buffer.contents buff ^ "\r\n") then (
      Buffer.clear buff;
      stdout_loop ())
    else
      write_all Lwt_unix.stdout (Bytes.of_string s) 0 (String.length s)
      >>= stdout_loop
  in
  let chars, add_char = Lwt_stream.create () in
  let rec stdin_loop () : unit Lwt.t =
    read () >>= function
    | None -> stdin_loop ()
    | Some data ->
        String.iter (fun c -> add_char (Some c)) data;
        stdin_loop ()
  in
  let send () =
    let data = Buffer.contents buff in
    stdin ~data
  in
  let rec process_loop () =
    LTerm_unix.parse_event chars >>= function
    | LTerm_event.Key { code = Up; _ } ->
        Buffer.add_string buff "\x1b[A";
        send () >>= fun () ->
        Buffer.clear buff;
        process_loop ()
    | LTerm_event.Key { code = Down; _ } -> send () >>= process_loop
    | LTerm_event.Key { code = Left; _ } -> send () >>= process_loop
    | LTerm_event.Key { code = Right; _ } -> send () >>= process_loop
    | LTerm_event.Key { code = Enter; _ } -> send () >>= process_loop
    | LTerm_event.Key { code = Char c; _ } ->
        Buffer.add_char buff (Uchar.to_char c);
        process_loop ()
    | _ -> process_loop ()
  in
  Lwt.choose [ stdout_loop (); stdin_loop (); process_loop () ] >|= fun v ->
  Unix.tcsetattr Unix.stdin TCSADRAIN saved_tio;
  v
