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
  (* set raw mode *)
  let tio =
    {
      saved_tio with
      (* input modes *)
      c_ignpar = true;
      c_istrip = false;
      c_inlcr = false;
      c_igncr = false;
      c_ixon = false;
      (* c_ixany = false; *)
      (* c_iuclc = false; *)
      c_ixoff = false;
      (* output modes *)
      c_opost = false;
      (* control modes *)
      c_isig = false;
      c_icanon = false;
      c_echo = false;
      c_echoe = false;
      c_echok = false;
      c_echonl = false;
      (* c_iexten = false; *)

      (* special characters *)
      c_vmin = 1;
      c_vtime = 0;
    }
  in
  Unix.tcsetattr Unix.stdin TCSADRAIN tio;
  let rec stdout_loop () =
    stdout () >>= fun s ->
    write_all Lwt_unix.stdout (Bytes.of_string s) 0 (String.length s)
    >>= stdout_loop
  in
  let rec stdin_loop () : unit Lwt.t =
    read () >>= function
    | None -> stdin_loop ()
    | Some data -> stdin ~data >>= stdin_loop
  in
  Stdlib.at_exit (fun () -> Unix.tcsetattr Unix.stdin TCSADRAIN saved_tio);
  Lwt.choose [ stdout_loop (); stdin_loop () ]
