(** Winget location + bootstrap download.

    Resolution order: [# winget:] override → portable candidates walking
    up 4 levels from the cwd, then the exe dir → exe-adjacent [winget/]
    subfolder → PATH lookup of ["winget"] (+ [.exe] on Win32) → [cmd /c where
    winget] alias resolution with a [--version] probe →
    [%LOCALAPPDATA%/…/WindowsApps/winget.exe] with probe → cached
    [AppInstaller.exe] → download.

    Design notes:

    - [winget_runs] needs only one spawn: {!spawn} always returns combined
      output, so one [--version] call decides.
    - Failures come back as values. {!ensure} returns the error (with cert
      advice appended when it fits) and lets the CLI layer decide what to
      display. *)

(** [spawn prog args] runs the command, returning combined output and
    whether it exited zero. Unlike {!Proc.runner} the output survives
    failure: the installer needs winget's text even on non-zero exit. *)
type spawn = string -> string list -> string * bool

(** Real backend on top of [Unix.open_process_args_in]. *)
let real_spawn : spawn =
  fun prog args ->
  try
    let cmd = Array.of_list (prog :: args) in
    let ic = Unix.open_process_args_in prog cmd in
    let buf = Buffer.create 256 in
    (try
       while true do
         Buffer.add_channel buf ic 4096
       done
     with
     | End_of_file -> ());
    let out = String.trim (Buffer.contents buf) in
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> out, true
    | _ -> out, false
  with
  | _ -> "", false
;;

(** Injected OS surface. Tests fake the filesystem/spawns; production uses
    {!real_io}. *)
type io =
  { getenv : string -> string option
  ; is_file : string -> bool
  ; spawn : spawn
  ; cwd : unit -> (string, string) result
  ; exe_dir : unit -> (string, string) result
  ; mkdir_p : string -> (unit, string) result
  ; cache_dir : unit -> string
  ; latest_winget_cli : unit -> (Gh.release, string) result
  ; download : url:string -> (string, string) result
  ; unzip : zip:string -> dir:string -> (unit, string) result
  }

(** Production wiring: curl fetcher, digestif verifier, central-dir zip
    reader. [cache_dir] is [%LOCALAPPDATA%] on Windows,
    [$XDG_CACHE_HOME/~/.cache] elsewhere, + [devkit/winget]. *)
let real_io (fetch : Fetch.fetch) : io =
  { getenv = Sys.getenv_opt
  ; is_file = Sys.file_exists
  ; spawn = real_spawn
  ; cwd =
      (fun () ->
        try Ok (Unix.getcwd ()) with
        | e -> Error (Printexc.to_string e))
  ; exe_dir = (fun () -> Ok (Filename.dirname Sys.executable_name))
  ; mkdir_p =
      (fun dir ->
        try
          Zipx.mkdir_p dir;
          Ok ()
        with
        | e -> Error (Printexc.to_string e))
  ; cache_dir =
      (fun () ->
        let base =
          match Sys.getenv_opt "XDG_CACHE_HOME" with
          | Some d when d <> "" -> d
          | _ ->
            (match Sys.getenv_opt "HOME" with
             | Some h when h <> "" -> Filename.concat h ".cache"
             | _ ->
               (match Sys.getenv_opt "LOCALAPPDATA" with
                | Some l when l <> "" -> Filename.concat l "cache"
                | _ -> "."))
        in
        Filename.concat (Filename.concat base "devkit") "winget")
  ; latest_winget_cli = (fun () -> Gh.latest_release fetch "microsoft/winget-cli")
  ; download = (fun ~url -> Hash.download_and_verify fetch Hash.No_expected url)
  ; unzip = (fun ~zip ~dir -> Zipx.extract ~zip_path:zip ~dest_dir:dir)
  }
;;

let portable_names = [ "winget.exe"; "AppInstaller.exe" ]
let portable_subs = [ []; [ "winget" ]; [ "build"; "winget" ] ]

(** [find_portable ~is_file base] returns the first portable winget under
    [base], checking each level's 6 candidates (2 names × 3 subfolders)
    while walking up 4 levels. *)
let find_portable ~is_file (base : string) : string =
  let candidates dir =
    List.concat_map
      (fun sub ->
         List.map
           (fun name -> List.fold_left Filename.concat dir (sub @ [ name ]))
           portable_names)
      portable_subs
  in
  let rec level dir n =
    if n = 0
    then ""
    else (
      match List.find_opt is_file (candidates dir) with
      | Some p -> p
      | None ->
        let parent = Filename.dirname dir in
        if parent = dir then "" else level parent (n - 1))
  in
  level base 4
;;

(** PATH lookup for [name]. The separator is [;] on Win32 ([?] injected
    for tests) and [:] elsewhere: splitting on both mangles drive-letter
    dirs like [C:\…]. On Win32 each dir is also probed for [name.exe],
    since the file on disk carries the extension. *)
let find_on_path ?(os = Sys.os_type) ~getenv ~is_file (name : string) : string =
  let raw =
    match getenv "PATH" with
    | None -> ""
    | Some p -> p
  in
  let win = os = "Win32" || os = "Cygwin" in
  let sep = if os = "Win32" then ';' else ':' in
  let dirs = List.filter (fun d -> d <> "") (String.split_on_char sep raw) in
  let names = if win then [ name; name ^ ".exe" ] else [ name ] in
  let hit =
    List.find_map
      (fun d ->
         List.find_map
           (fun n ->
              let p = Filename.concat d n in
              if is_file p then Some p else None)
           names)
      dirs
  in
  Option.value hit ~default:""
;;

(** True when [path] answers [--version] with exit 0, or prints anything
    at all (some builds emit the version to stderr with a non-zero exit;
    those count too). *)
let winget_runs ~spawn (path : string) : bool =
  if path = ""
  then false
  else (
    let out, ok = spawn path [ "--version" ] in
    ok || String.trim out <> "")
;;

(** Resolve the Store app-execution alias through [cmd], which understands
    it where [stat]/PATH lookup cannot. First probed-runnable line
    mentioning [winget] wins. *)
let resolve_via_cmd ~spawn : string =
  let out, ok = spawn "cmd" [ "/c"; "where"; "winget" ] in
  if not ok
  then ""
  else (
    let rec go = function
      | [] -> ""
      | line :: rest ->
        let line = String.trim line in
        if line = ""
        then go rest
        else if
          Strutil.contains_substring "winget" (String.lowercase_ascii line)
          && winget_runs ~spawn line
        then line
        else go rest
    in
    go (String.split_on_char '\n' out))
;;

(** Full resolution chain. Returns [""] when nothing runs. *)
let resolve (io : io) : string =
  let from_cwd =
    match io.cwd () with
    | Error _ -> ""
    | Ok d -> find_portable ~is_file:io.is_file d
  in
  if from_cwd <> ""
  then from_cwd
  else (
    let from_exe =
      match io.exe_dir () with
      | Error _ -> ""
      | Ok exe ->
        let p = find_portable ~is_file:io.is_file exe in
        if p <> ""
        then p
        else (
          (* Exe-adjacent [winget/] subfolder: subsumed by the walk-up in
             practice, kept as a fallback. *)
          let wing = Filename.concat (Filename.concat exe "winget") in
          let w = wing "winget.exe" in
          if io.is_file w
          then w
          else (
            let a = wing "AppInstaller.exe" in
            if io.is_file a then a else ""))
    in
    if from_exe <> ""
    then from_exe
    else (
      let looked = find_on_path ~getenv:io.getenv ~is_file:io.is_file "winget" in
      if looked <> ""
      then looked
      else (
        let via = resolve_via_cmd ~spawn:io.spawn in
        if via <> ""
        then via
        else (
          let local =
            match io.getenv "LOCALAPPDATA" with
            | None | Some "" -> ""
            | Some la ->
              Filename.concat
                (Filename.concat (Filename.concat la "Microsoft") "WindowsApps")
                "winget.exe"
          in
          if local <> "" && winget_runs ~spawn:io.spawn local
          then local
          else (
            let cached = Filename.concat (io.cache_dir ()) "AppInstaller.exe" in
            if io.is_file cached then cached else "")))))
;;

let cert_markers =
  [ "x509"; "unknown authority"; "certificate"; "80072f0d"; "InternetOpenUrl"; "CERT_" ]
;;

(** True when [msg] indicates a TLS/certificate failure, including
    winget's WinHTTP code [0x80072f0d]. *)
let is_cert_error (msg : string) : bool =
  List.exists (fun sub -> Strutil.contains_substring sub msg) cert_markers
;;

let cert_advice =
  "This looks like a TLS/proxy certificate error (0x80072f0d).\n"
  ^ "Import your proxy's CA into the Windows Trusted Root store\n"
  ^ "so winget's WinHTTP can validate it."
;;

(** First asset whose name ends in [.msixbundle] (case-insensitive). *)
let find_msixbundle (rel : Gh.release) : Gh.asset option =
  let suf = ".msixbundle" in
  List.find_opt
    (fun (a : Gh.asset) ->
       let n = String.lowercase_ascii a.Gh.name in
       String.length n >= String.length suf
       && String.sub n (String.length n - String.length suf) (String.length suf) = suf)
    rel.Gh.assets
;;

(** Pull the x64 [.msix] out of a bundle into a temp file, then unzip it
    into [dest_dir]. Entry match: contains [x64], ends [.msix],
    case-insensitive, first wins. *)
let extract_bundle (io : io) (bundle_path : string) (dest_dir : string)
  : (unit, string) result
  =
  let read_all path =
    try
      let ic = open_in_bin path in
      let n = in_channel_length ic in
      let b = Bytes.create n in
      really_input ic b 0 n;
      close_in ic;
      Ok b
    with
    | Sys_error msg -> Error ("open bundle: " ^ msg)
  in
  match read_all bundle_path with
  | Error _ as e -> e
  | Ok b ->
    (match Zipx.list_entries b with
     | Error _ as e -> e
     | Ok entries ->
       let is_x64_msix (e : Zipx.entry) =
         let n = String.lowercase_ascii e.Zipx.name in
         Strutil.contains_substring "x64" n
         && String.length n >= 5
         && String.sub n (String.length n - 5) 5 = ".msix"
       in
       (match List.find_opt is_x64_msix entries with
        | None -> Error "no x64 .msix found in bundle"
        | Some e ->
          (match Zipx.extract_entry b e with
           | Error _ as err -> err
           | Ok payload ->
             let tmp = Filename.temp_file "devkit-msix-" ".msix" in
             let written =
               try
                 let oc = open_out_bin tmp in
                 output_bytes oc payload;
                 close_out oc;
                 Ok ()
               with
               | Sys_error msg -> Error msg
             in
             (match written with
              | Error _ as err ->
                (try Sys.remove tmp with
                 | _ -> ());
                err
              | Ok () ->
                let r = io.unzip ~zip:tmp ~dir:dest_dir in
                (try Sys.remove tmp with
                 | _ -> ());
                r))))
;;

(** Download the latest winget-cli bundle and unpack it into the cache
    dir. The temp download is removed afterwards. *)
let download_winget (io : io) : (string, string) result =
  match io.latest_winget_cli () with
  | Error e -> Error ("fetch winget-cli release: " ^ e)
  | Ok rel ->
    (match find_msixbundle rel with
     | None -> Error ("no .msixbundle found in winget-cli " ^ rel.Gh.tag_name)
     | Some asset ->
       (match io.download ~url:asset.Gh.browser_download_url with
        | Error e -> Error ("download: " ^ e)
        | Ok path ->
          let cleanup () =
            (try Sys.remove path with
             | _ -> ());
            try Unix.rmdir (Filename.dirname path) with
            | _ -> ()
          in
          let r =
            match io.mkdir_p (io.cache_dir ()) with
            | Error e -> Error ("cache dir: " ^ e)
            | Ok () ->
              (match extract_bundle io path (io.cache_dir ()) with
               | Error e -> Error ("extract: " ^ e)
               | Ok () ->
                 let exe = Filename.concat (io.cache_dir ()) "AppInstaller.exe" in
                 (* The user asked for winget, so the error names it. *)
                 if io.is_file exe
                 then Ok exe
                 else Error "winget.exe not found after extraction")
          in
          cleanup ();
          r))
;;

(** Human byte count. *)
let format_size (bytes : int64) : string =
  if bytes >= 1_073_741_824L
  then Printf.sprintf "%.1f GB" (Int64.to_float bytes /. 1_073_741_824.0)
  else if bytes >= 1_048_576L
  then Printf.sprintf "%.1f MB" (Int64.to_float bytes /. 1_048_576.0)
  else if bytes >= 1_024L
  then Printf.sprintf "%.1f KB" (Int64.to_float bytes /. 1_024.0)
  else Printf.sprintf "%Ld B" bytes
;;

(** Memoized {!resolve}: the (slow, window-spawning) probe runs once; the
    miss ([None]→[""]) is cached too. Fresh instances via {!make_path_cache}
    keep tests isolated. *)
let make_path_cache () =
  let cell : string option ref = ref None in
  let get (io : io) : string =
    match !cell with
    | Some p -> p
    | None ->
      let p = resolve io in
      cell := Some p;
      p
  in
  get, fun () -> cell := None
;;

let cached_path, reset_path_cache = make_path_cache ()

(** Locate winget, downloading it when nothing resolves. A non-empty
    [~override_path] (the [# winget:] directive) is returned blindly: no
    existence check. The download only ever runs on Windows ([~os]
    injectable for tests): elsewhere winget cannot exist, so resolving
    straight to an error instead of burning seconds on a doomed download. *)
let ensure_full ~override_path ~resolve_path ?(os = Sys.os_type) (io : io)
  : (string, string) result
  =
  if override_path <> ""
  then Ok override_path
  else (
    let w = resolve_path io in
    if w <> ""
    then Ok w
    else if os <> "Win32"
    then Error "winget is only available on Windows"
    else (
      match download_winget io with
      | Ok _ as ok -> ok
      | Error e -> if is_cert_error e then Error (e ^ "\n" ^ cert_advice) else Error e))
;;

let ensure ~override_path (io : io) : (string, string) result =
  ensure_full ~override_path ~resolve_path:cached_path io
;;
