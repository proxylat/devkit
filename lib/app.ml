(** CLI orchestration: the [run*] commands.

    Default view, import, add, append, append-new and export, plus
    [append_selected], manifest loading, scan fallback and the [winget
    show] enrichment. Everything runs against an injected environment so
    tests fake scan results, winget output and the filesystem without
    spawning processes.

    Deliberate omissions:
    - No silent manifest rewrite: it would destroy user edits, so it
      stays out.
    - No dead append helper: only wired commands ship.
    - [--new] appends every newly-detected app.
    - [winget show] enrichment runs on up to 8 domains (see
      [Dashboard.build_sections]); [ensure] never downloads off Windows.
    - pm order: [winget; npm; pipx; uv; cargo; gitlab; forgejo; registry]. *)

open Manifest
open Dashboard

let pm_order = [ "winget"; "npm"; "pipx"; "uv"; "cargo"; "gitlab"; "forgejo"; "registry" ]

type fs =
  { read_file : string -> string option
  ; append_file : string -> string -> (unit, string) result
  ; write_file : string -> string -> (unit, string) result
  }

type env =
  { run : Proc.runner
  ; fs : fs
  ; bio : Bootstrap.io
  ; fetch : Fetch.fetch
  }

let lower s = String.lowercase_ascii s

(** Missing/unparseable file yields empty sections and the default
    sources. Skipped rows are reported on stderr, so a typo'd id never
    vanishes silently. *)
let load_manifest (fs : fs) (path : string) : section list * string * string list =
  match fs.read_file path with
  | None -> [], "", Manifest.default_sources
  | Some text ->
    let r = Manifest.parse text in
    List.iter (fun w -> prerr_endline ("devkit: warning: " ^ w)) r.warnings;
    r.sections, r.winget_path, r.sources
;;

(** Best-effort winget info from the scan when [winget list] fails. *)
let info_from_scan (apps : app list) : Winget_parse.info Winget_parse.IdMap.t =
  List.fold_left
    (fun m (a : app) ->
       if a.pm = "winget" && a.name <> ""
       then
         Winget_parse.IdMap.add
           (lower a.name)
           { Winget_parse.id = a.name
           ; name = a.name
           ; version = a.version
           ; available = ""
           }
           m
       else m)
    Winget_parse.IdMap.empty
    apps
;;

(** [winget show --id <id>] → repo's latest version, "" on failure. *)
let winget_show (bio : Bootstrap.io) (winget : string) (id : string) : string =
  if winget = ""
  then ""
  else (
    let out, ok = bio.spawn winget [ "show"; "--id"; id; "--accept-source-agreements" ] in
    if not ok
    then ""
    else (
      let lines = String.split_on_char '\n' out in
      let rec find = function
        | [] -> ""
        | ln :: rest ->
          let t = String.trim ln in
          let pre = "Version:" in
          if
            String.length t >= String.length pre
            && String.sub t 0 (String.length pre) = pre
          then (
            let v =
              String.trim
                (String.sub t (String.length pre) (String.length t - String.length pre))
            in
            if v <> "" then v else find rest)
          else find rest
      in
      find lines))
;;

(** Vendor truth for [upstream] rows: [(provider, repo)] → latest
    release tag. [None] on any failure, so the merge keeps the PM's reading. *)
let upstream_ver_of_fetch (fetch : Fetch.fetch) (prov : Provider.t) (repo : string)
  : string option
  =
  match Provider.latest fetch prov repo with
  | Ok rel when rel.Gh.tag_name <> "" -> Some rel.Gh.tag_name
  | _ -> None
;;

(** Export groups by {!pm_order} plus any custom tool names in file
    order, so custom tools are never dropped from the manifest. *)
let order_for (tools : Plugin.tool list) : string list =
  pm_order
  @ List.filter
      (fun n -> not (List.mem n pm_order))
      (List.map (fun (t : Plugin.tool) -> t.name) tools)
;;

(** [resolve_sources]: validate raw [sources] names against the known
    PMs ([order_for tools]), matching case-insensitively but returning
    canonical spellings so the scan's exact-match lookup works. An
    empty list means the key was absent: the defaults. An unknown name
    is a hard error — the scan then skips every spawn so nothing
    half-runs on a typo'd source. *)
let resolve_sources (raw : string list) ~(tools : Plugin.tool list)
  : (string list, string) result
  =
  let known = order_for tools in
  let find n = List.find_opt (fun k -> lower k = lower n) known in
  match List.find_opt (fun n -> find n = None) raw with
  | Some bad ->
    Error (Printf.sprintf "unknown source: %s (known: %s)" bad (String.concat ", " known))
  | None ->
    let srcs = List.filter_map find raw in
    Ok (if srcs = [] then Manifest.default_sources else srcs)
;;

type scan =
  { apps : app list
  ; info : Winget_parse.info Winget_parse.IdMap.t option
  ; winget : string
  ; winget_error : string
  ; sources_error : string
  }

(** [DEVKIT_TIMING=1] prints per-phase startup timings to stderr, so a
    slow start can be blamed on a phase instead of guessed at. *)
let timed (label : string) (f : unit -> 'a) : 'a =
  match Sys.getenv_opt "DEVKIT_TIMING" with
  | None | Some "" -> f ()
  | Some _ ->
    let t0 = Unix.gettimeofday () in
    let r = f () in
    Printf.eprintf "[timing] %s: %.0fms\n%!" label ((Unix.gettimeofday () -. t0) *. 1000.0);
    r
;;

(** One scan + update check; the memoized [winget list] fetch is shared by
    both, so a command spawns winget once per 8s window. A failed ensure
    degrades to winget-less operation ([""] + the error in
    [winget_error]) instead of hiding the cause. *)
let scan
      (e : env)
      ?(os : string = Sys.os_type)
      ~(override_path : string)
      ~(extra : Plugin.tool list)
      ~(sources : string list)
      ()
  : scan
  =
  match resolve_sources sources ~tools:extra with
  | Error err ->
    { apps = []; info = None; winget = ""; winget_error = ""; sources_error = err }
  | Ok srcs ->
    let winget, winget_error =
      timed "ensure" (fun () ->
        match Bootstrap.ensure ~override_path e.bio with
        | Ok w -> w, ""
        | Error err -> "", err)
    in
    let fetch, _ = Proc.winget_list e.run in
    let apps =
      timed "scan_all" (fun () ->
        Inventory.scan_all
          e.run
          ~os
          ~winget:(fun () -> fetch winget)
          ~extra
          ~sources:srcs
          ())
    in
    let info =
      timed "info" (fun () ->
        match fetch winget with
        | None ->
          let m = info_from_scan apps in
          if Winget_parse.IdMap.is_empty m then None else Some m
        | Some out ->
          let m = Winget_parse.parse_list_table out in
          if Winget_parse.IdMap.is_empty m
          then (
            let fb = info_from_scan apps in
            if Winget_parse.IdMap.is_empty fb then None else Some fb)
          else Some m)
    in
    { apps; info; winget; winget_error; sources_error = "" }
;;

let to_dashboard_apps (apps : app list) : Dashboard.app list =
  List.map
    (fun (a : app) -> { Dashboard.name = a.name; version = a.version; pm = a.pm })
    apps
;;

(** Default view: scan, enrich, merge with the manifest, render plain
    text. Returns the rendered dashboard (the TUI takes over on a tty).
    An empty scan appends what the winget resolution found, so a bare
    dashboard never hides the cause. *)
let default_view (e : env) ~(tools : Plugin.tool list) : string =
  (* Slow spawns (winget list) run after this; stderr stays visible
     past the TUI's alternate screen, so the wait looks alive. *)
  prerr_endline "scanning installed software...";
  flush stderr;
  let sections, winget_path, sources = load_manifest e.fs Manifest.filename in
  let s = scan e ~override_path:winget_path ~extra:tools ~sources () in
  let show id =
    let v = winget_show e.bio s.winget id in
    if v = "" then None else Some v
  in
  let upstream_ver = upstream_ver_of_fetch e.fetch in
  let dash =
    timed "build" (fun () ->
      build_sections
        ~show
        ~upstream_ver
        ~run:(Some e.run)
        (to_dashboard_apps s.apps)
        sections
        s.info)
  in
  let body = render dash in
  if s.sources_error <> ""
  then (
    prerr_endline ("devkit: error: " ^ s.sources_error);
    body ^ "\n  error: " ^ s.sources_error)
  else if s.apps <> []
  then body
  else (
    let winget =
      if s.winget <> ""
      then "winget: " ^ s.winget
      else if s.winget_error <> ""
      then "winget error: " ^ s.winget_error
      else "winget: not found"
    in
    body ^ "\n  no installed software found\n  " ^ winget)
;;

let import_view (e : env) ?(tools : Plugin.tool list = []) (path : string)
  : (string, string) result
  =
  match e.fs.read_file path with
  | None -> Error ("open: " ^ path)
  | Some text ->
    let r = Manifest.parse text in
    List.iter (fun w -> prerr_endline ("devkit: warning: " ^ w)) r.warnings;
    let s = scan e ~override_path:r.winget_path ~extra:tools ~sources:r.sources () in
    if s.sources_error <> ""
    then Error s.sources_error
    else (
      let show id =
        let v = winget_show e.bio s.winget id in
        if v = "" then None else Some v
      in
      let upstream_ver = upstream_ver_of_fetch e.fetch in
      Ok
        (render
           (build_sections
              ~show
              ~upstream_ver
              ~run:(Some e.run)
              (to_dashboard_apps s.apps)
              r.sections
              s.info)))
;;

(** [appendSelected]: persists items under a "Newly detected" section,
    grouped in pm order. Existing entries are never touched. *)
let append_selected (fs : fs) (path : string) (items : item list) : (unit, string) result =
  if items = []
  then Ok ()
  else (
    let ordered =
      List.concat_map
        (fun pm -> List.filter (fun it -> Manifest.type_string it.typ = pm) items)
        pm_order
    in
    let text =
      "\n"
      ^ Manifest.to_string
          { sections = [ { name = "Newly detected"; items = ordered } ]
          ; winget_path = ""
          ; warnings = []
          ; sources = []
          }
    in
    fs.append_file path text)
;;

let is_new_section (sec : section) : bool =
  let n = lower sec.name in
  n = "newly detected" || n = "pending updates"
;;

(** StatusNew items from Newly-detected (+ Pending-updates when [extra]). *)
let new_items ?(extra : bool = false) (sections : section list) : item list =
  List.concat_map
    (fun (sec : section) ->
       if
         lower sec.name = "newly detected" || (extra && lower sec.name = "pending updates")
       then List.filter (fun it -> it.status = New) sec.items
       else [])
    sections
;;

(** [runAdd]: appends given ids (must be installed, not already known). *)
let run_add (e : env) ?(tools : Plugin.tool list = []) (ids : string list) : string list =
  let sections, _, sources = load_manifest e.fs Manifest.filename in
  let s = scan e ~override_path:"" ~extra:tools ~sources () in
  if s.sources_error <> ""
  then [ "  error: " ^ s.sources_error ]
  else (
    let installed = Hashtbl.create 64 in
    (match s.info with
     | Some m -> Winget_parse.IdMap.iter (fun id _ -> Hashtbl.replace installed id true) m
     | None -> ());
    List.iter
      (fun (a : app) ->
         if a.pm = "winget" && a.name <> ""
         then Hashtbl.replace installed (lower a.name) true)
      s.apps;
    let known = Hashtbl.create 64 in
    List.iter
      (fun (sec : section) ->
         List.iter (fun it -> Hashtbl.replace known (lower it.value) true) sec.items)
      sections;
    let msgs = ref [] in
    let emit m = msgs := m :: !msgs in
    let to_append = ref [] in
    List.iter
      (fun raw ->
         let id = lower raw in
         if Hashtbl.mem known id
         then emit (Printf.sprintf "  skip (already in manifest): %s" raw)
         else if not (Hashtbl.mem installed id)
         then emit (Printf.sprintf "  skip (not installed): %s" raw)
         else (
           let ver, avail =
             match s.info with
             | Some m ->
               (match Winget_parse.IdMap.find_opt id m with
                | Some inf -> inf.version, inf.available
                | None -> "", "")
             | None -> "", ""
           in
           to_append
           := { (make_item Winget raw) with
                installed_version = ver
              ; available_version = avail
              ; status = New
              }
              :: !to_append))
      ids;
    (match List.rev !to_append with
     | [] -> emit "  nothing to append"
     | items ->
       (match append_selected e.fs Manifest.filename items with
        | Error e -> emit ("  error: " ^ e)
        | Ok () ->
          emit
            (Printf.sprintf
               "  appended %d app(s) to %s"
               (List.length items)
               Manifest.filename)));
    List.rev !msgs)
;;

(** [runAppend]: no ids → every newly-detected winget-installable app;
    with ids → behaves like [runAdd]. *)
let run_append
      (e : env)
      ?(tools : Plugin.tool list = [])
      ?(os : string = Sys.os_type)
      (ids : string list)
  : string list
  =
  if ids <> []
  then run_add e ~tools ids
  else (
    let sections, _, sources = load_manifest e.fs Manifest.filename in
    let s = scan e ~os ~override_path:"" ~extra:tools ~sources () in
    if s.sources_error <> ""
    then [ "  error: " ^ s.sources_error ]
    else (
      let dash =
        build_sections ~run:(Some e.run) (to_dashboard_apps s.apps) sections s.info
      in
      let items = new_items ~extra:true dash in
      (* Registry rows are discovery-only: they stay in the table but
         never persist — an id guess would rot on the next rename. *)
      let keepable = List.filter (fun it -> it.typ <> Registry) items in
      let skipped = List.length items - List.length keepable in
      let note =
        if skipped = 0
        then []
        else [ Printf.sprintf "  %d registry row(s) need manual ids" skipped ]
      in
      if keepable = []
      then [ "  nothing new to append" ] @ note
      else (
        match append_selected e.fs Manifest.filename keepable with
        | Error e -> [ "  error: " ^ e ] @ note
        | Ok () ->
          [ Printf.sprintf
              "  appended %d app(s) to %s"
              (List.length keepable)
              Manifest.filename
          ]
          @ note)))
;;

(** [runAppendNew] (--new): newly-detected apps only (no pending updates). *)
let run_append_new (e : env) ~(tools : Plugin.tool list) ?(os : string = Sys.os_type) ()
  : string list
  =
  let sections, _, sources = load_manifest e.fs Manifest.filename in
  let s = scan e ~os ~override_path:"" ~extra:tools ~sources () in
  if s.sources_error <> ""
  then [ "  error: " ^ s.sources_error ]
  else (
    let dash =
      build_sections ~run:(Some e.run) (to_dashboard_apps s.apps) sections s.info
    in
    let items = new_items dash in
    let keepable = List.filter (fun it -> it.typ <> Registry) items in
    let skipped = List.length items - List.length keepable in
    let note =
      if skipped = 0
      then []
      else [ Printf.sprintf "  %d registry row(s) need manual ids" skipped ]
    in
    if keepable = []
    then [ "  no newly detected apps" ] @ note
    else (
      match append_selected e.fs Manifest.filename keepable with
      | Error e -> [ "  error: " ^ e ] @ note
      | Ok () ->
        [ Printf.sprintf
            "  appended %d app(s) to %s"
            (List.length keepable)
            Manifest.filename
        ]
        @ note))
;;

(** [runExport]: scan → write manifest grouped in pm order, plus a
    [winget import]-compatible JSON next to it (same basename, [.json]
    extension). Only winget-tracked apps land in the JSON; the rest live
    in the manifest alone. Scans with [~sources:[]] (the defaults): a
    full snapshot independent of the manifest's key, filtered only by
    [~only]/[~except]. *)
let json_sibling (output : string) : string =
  if Filename.check_suffix output ".toml"
  then Filename.chop_suffix output ".toml" ^ ".json"
  else output ^ ".json"
;;

(** Export groups by {!pm_order} plus any custom tool names in file
    order, so custom tools are never dropped from the manifest. *)
let order_for (tools : Plugin.tool list) : string list =
  pm_order
  @ List.filter
      (fun n -> not (List.mem n pm_order))
      (List.map (fun (t : Plugin.tool) -> t.name) tools)
;;

let run_export
      (e : env)
      ?(tools : Plugin.tool list = [])
      ?(os : string = Sys.os_type)
      ?(only : string list = [])
      ?(except : string list = [])
      (output : string)
  : string list
  =
  let s = scan e ~os ~override_path:"" ~extra:tools ~sources:[] () in
  if s.sources_error <> ""
  then [ "  error: " ^ s.sources_error ]
  else (
    let keep pm = (only = [] || List.mem pm only) && not (List.mem pm except) in
    (* Registry rows are discovery-only: no stable id to write back. *)
    let reg_skipped =
      List.filter (fun (a : app) -> a.pm = "registry" && keep a.pm) s.apps
    in
    let apps = List.filter (fun (a : app) -> keep a.pm && a.pm <> "registry") s.apps in
    let note =
      if reg_skipped = []
      then []
      else
        [ Printf.sprintf
            "  %d registry app(s) skipped (need manual ids)"
            (List.length reg_skipped)
        ]
    in
    if apps = []
    then [ "  no installed software found" ] @ note
    else (
      let sections =
        List.filter_map
          (fun pm ->
             let apps = List.filter (fun (a : app) -> a.pm = pm) apps in
             match apps with
             | [] -> None
             | _ ->
               Some
                 { name = pm
                 ; items =
                     List.map
                       (fun (a : app) ->
                          { (make_item (Manifest.item_type_of_string a.pm) a.name) with
                            installed_version = a.version
                          })
                       apps
                 })
          (order_for tools)
      in
      match
        e.fs.write_file
          output
          (Manifest.to_string { sections; winget_path = ""; warnings = []; sources = [] })
      with
      | Error e -> [ "  error: " ^ e ]
      | Ok () ->
        let json_path = json_sibling output in
        let winget_apps = List.filter (fun (a : app) -> a.pm = "winget") apps in
        let base = Printf.sprintf "wrote %s (%d apps)" output (List.length apps) in
        (* An empty Packages list violates the schema (minItems 1) and
           winget import would reject it, so the file is skipped instead. *)
        if winget_apps = []
        then [ base; "  no winget apps, json skipped" ] @ note
        else (
          match e.fs.write_file json_path (Winget_json.to_string apps) with
          | Error e -> [ base; "  winget json skipped: " ^ e ] @ note
          | Ok () ->
            [ base
            ; Printf.sprintf
                "wrote %s (%d winget apps, winget import -i ready)"
                json_path
                (List.length winget_apps)
            ]
            @ note)))
;;
