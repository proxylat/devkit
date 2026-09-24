(** devkit CLI.

     Commands: default (dashboard), import, add, append [--new], export
     [-o]. The default command opens the interactive TUI on a terminal
     (plain-text dashboard when piped, or on Win32 where notty has no
     backend). *)

open Devkit

let real_fs : App.fs =
  { read_file =
      (fun path ->
        try
          let ic = open_in path in
          let n = in_channel_length ic in
          let s = really_input_string ic n in
          close_in ic;
          Some s
        with
        | _ -> None)
  ; append_file =
      (fun path text ->
        try
          let oc = open_out_gen [ Open_append; Open_creat ] 0o644 path in
          output_string oc text;
          close_out oc;
          Ok ()
        with
        | e -> Error (Printexc.to_string e))
  ; write_file =
      (fun path text ->
        try
          let oc = open_out path in
          output_string oc text;
          close_out oc;
          Ok ()
        with
        | e -> Error (Printexc.to_string e))
  }
;;

let make_env () : App.env =
  let fetch = Fetch.curl_fetch Proc.default_runner in
  { run = Proc.default_runner; fs = real_fs; bio = Bootstrap.real_io fetch }
;;

(** Custom tools from [tools.sexp] (cwd) + config dir. A broken file
    exits with an error: silently ignoring the user's tools would be worse. *)
let load_tools () : Plugin.tool list =
  match
    Plugin.load_many
      ~read:real_fs.read_file
      ~reserved:Inventory.reserved_names
      (Plugin.default_paths ())
  with
  | Ok tools -> tools
  | Error e ->
    prerr_endline ("devkit: " ^ e);
    exit 1
;;

let print_lines lines = List.iter print_endline lines

let default_term =
  Cmdliner.Term.(
    const (fun () ->
      let tools = load_tools () in
      let env = make_env () in
      (* Piped output stays plain text; a terminal gets the TUI. Notty
         has no Windows backend, so Win32 terminals get plain text too
         (the TUI returns with lambda-term). *)
      if Unix.isatty Unix.stdout && Sys.os_type <> "Win32"
      then Tui_front.run ~tools env
      else print_string (App.default_view ~tools env))
    $ const ())
;;

let default_info =
  Cmdliner.Cmd.info
    "devkit"
    ~doc:
      "Scan your PC for installed software and manage packages. With devkit.toml: shows \
       status. Without: shows everything installed."
;;

let import_cmd =
  let file = Cmdliner.Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE") in
  let run path =
    match App.import_view ~tools:(load_tools ()) (make_env ()) path with
    | Error e ->
      prerr_endline ("devkit: " ^ e);
      exit 1
    | Ok s -> print_string s
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "import" ~doc:"Import and install packages from a file")
    Cmdliner.Term.(const run $ file)
;;

let add_cmd =
  let ids = Cmdliner.Arg.(non_empty & pos_all string [] & info [] ~docv:"ID") in
  let run ids = print_lines (App.run_add ~tools:(load_tools ()) (make_env ()) ids) in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "add" ~doc:"Append specific installed apps to devkit.toml")
    Cmdliner.Term.(const run $ ids)
;;

let append_cmd =
  let ids = Cmdliner.Arg.(value & pos_all string [] & info [] ~docv:"ID") in
  let is_new =
    Cmdliner.Arg.(value & flag & info [ "new" ] ~doc:"append all newly-detected apps")
  in
  let run ids is_new =
    let tools = load_tools () in
    print_lines
      (if is_new
       then App.run_append_new ~tools (make_env ())
       else App.run_append ~tools (make_env ()) ids)
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info
       "append"
       ~doc:"Append newly-detected apps (or given ids) to devkit.toml")
    Cmdliner.Term.(const run $ ids $ is_new)
;;

let export_cmd =
  let output =
    Cmdliner.Arg.(
      value
      & opt string Manifest.filename
      & info [ "o"; "output" ] ~docv:"FILE" ~doc:"output file path")
  in
  let run output =
    print_lines (App.run_export ~tools:(load_tools ()) (make_env ()) output)
  in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info
       "export"
       ~doc:"Scan PC and write all installed software to devkit.toml")
    Cmdliner.Term.(const run $ output)
;;

let () =
  let group =
    Cmdliner.Cmd.group
      ~default:default_term
      default_info
      [ import_cmd; add_cmd; append_cmd; export_cmd ]
  in
  exit (Cmdliner.Cmd.eval group)
;;
