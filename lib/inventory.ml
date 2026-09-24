(** Inventory scanners: installed-software discovery per package manager.

    Every scanner takes a {!Proc.runner} and every parser is pure over
    captured output, so tests feed fixtures without spawning processes.

    Parser edge cases (each tested):
    - pipx versions lose their trailing comma.
    - cargo versions lose the ["vX.Y.Z:"] trailing colon.
    - uv ([uv tool list]) scans like pipx. *)

open Dashboard

let fields (line : string) : string list =
  let parts = String.split_on_char ' ' line in
  let parts = List.concat_map (String.split_on_char '\t') parts in
  List.filter (fun s -> s <> "" && s <> "\r") parts
;;

(** Trim [cut] characters from both ends. Mirrors [strings.Trim]. *)
let trim_cutset (cut : string) (s : string) : string =
  let is_cut c = String.contains cut c in
  let n = String.length s in
  let i = ref 0 in
  while !i < n && is_cut s.[!i] do
    incr i
  done;
  let j = ref (n - 1) in
  while !j >= !i && is_cut s.[!j] do
    decr j
  done;
  if !j < !i then "" else String.sub s !i (!j - !i + 1)
;;

let lines_of output = String.split_on_char '\n' output

(** npm tree-drawing prefixes, all U+2500-block (E2 94 xx) plus space. *)
let is_npm_glyph_lead s i =
  let n = String.length s in
  i + 2 < n + 1
  && Char.code s.[i] = 0xE2
  && Char.code s.[i + 1] = 0x94
  &&
  let t = Char.code s.[i + 2] in
  t >= 0x80 && t <= 0xAC
;;

let s_is_space s i = s.[i] = ' ' || s.[i] = '\t' || s.[i] = '\r'

let strip_npm_prefix (line : string) : string =
  let n = String.length line in
  let i = ref 0 in
  let cont = ref true in
  while !cont && !i < n do
    if s_is_space line !i
    then incr i
    else if is_npm_glyph_lead line !i
    then i := !i + 3
    else cont := false
  done;
  String.trim (String.sub line !i (n - !i))
;;

(** [npm list -g --depth=0]: tree rows, split on the LAST [@] so scoped
    packages ([@scope/name@1.2.3]) survive. *)
let parse_npm (output : string) : app list =
  List.filter_map
    (fun raw ->
       let line = strip_npm_prefix (String.trim raw) in
       if line = "" || not (String.contains line '@')
       then None
       else (
         match String.rindex_opt line '@' with
         | None -> None
         | Some at ->
           let name = String.trim (String.sub line 0 at) in
           let version =
             String.trim (String.sub line (at + 1) (String.length line - at - 1))
           in
           if name = "" || version = "" then None else Some { name; version; pm = "npm" }))
    (lines_of output)
;;

(** [pipx list --short]: [name version …] rows. *)
let parse_pipx (output : string) : app list =
  List.filter_map
    (fun raw ->
       match fields (String.trim raw) with
       | [] -> None
       | [ name ] ->
         let name = trim_cutset "," name in
         if name = "" then None else Some { name; version = ""; pm = "pipx" }
       | name :: ver :: _ ->
         let name = trim_cutset "," name in
         if name = ""
         then None
         else Some { name; version = trim_cutset "()," ver; pm = "pipx" })
    (lines_of output)
;;

(** [uv tool list]: [name vX.Y.Z] headers with [- exe] shim lines beneath.
    [Failed to parse entry …] warnings are skipped. *)
let parse_uv (output : string) : app list =
  List.filter_map
    (fun raw ->
       let line = String.trim raw in
       if line = "" || line.[0] = '-' || Strutil.contains_substring "Failed to parse" line
       then None
       else (
         match fields line with
         | [ name; ver ] when name <> "" ->
           let version =
             if String.length ver > 0 && ver.[0] = 'v'
             then String.sub ver 1 (String.length ver - 1)
             else ver
           in
           Some { name; version; pm = "uv" }
         | _ -> None))
    (lines_of output)
;;

(** [cargo install --list]: [name vX.Y.Z:] rows, detail lines indented. *)
let parse_cargo (output : string) : app list =
  List.filter_map
    (fun raw ->
       match fields (String.trim raw) with
       | name :: ver :: _ when name <> "" ->
         let ver =
           ver
           |> (fun s ->
           if String.length s > 0 && s.[String.length s - 1] = ':'
           then String.sub s 0 (String.length s - 1)
           else s)
           |> trim_cutset "v"
         in
         Some { name; version = ver; pm = "cargo" }
       | _ -> None)
    (lines_of output)
;;

(** winget rows via {!Winget_parse}; the Go scanner keys these by id and so
    do we ([Name] is the id column). Sorted by id for deterministic output.
    Go preserved winget's row order, but every consumer only needs lookup. *)
let parse_winget (output : string) : app list =
  let m = Winget_parse.parse_list_table output in
  Winget_parse.IdMap.fold
    (fun _ (inf : Winget_parse.info) acc ->
       { name = inf.id; version = inf.version; pm = "winget" } :: acc)
    m
    []
  |> List.sort (fun a b -> String.compare a.name b.name)
;;

(** The five built-ins as plugin entries, so custom tools and built-ins
    share one type: scan programs, row parsers and install/upgrade
    templates in one place. Scan order is the list order. winget keeps
    its memoized-fetch path in {!scan_all} (a raw [run] would spawn a
    window per call); its entry documents the equivalent commands.

    Templates float to latest (no [{version}] yet; install takes no
    version). cargo has no upgrade command ([cargo-update] is a
    third-party plugin), hence [upgrade = None]. *)
let npm_tool : Plugin.tool =
  { name = "npm"
  ; prog = "npm"
  ; list_args = [ "list"; "-g"; "--depth=0" ]
  ; install = Some [ "npm"; "install"; "-g"; "{id}" ]
  ; upgrade = Some [ "npm"; "update"; "-g"; "{id}" ]
  ; parse = parse_npm
  }
;;

let pipx_tool : Plugin.tool =
  { name = "pipx"
  ; prog = "pipx"
  ; list_args = [ "list"; "--short" ]
  ; install = Some [ "pipx"; "install"; "{id}" ]
  ; upgrade = Some [ "pipx"; "upgrade"; "{id}" ]
  ; parse = parse_pipx
  }
;;

let uv_tool : Plugin.tool =
  { name = "uv"
  ; prog = "uv"
  ; list_args = [ "tool"; "list" ]
  ; install = Some [ "uv"; "tool"; "install"; "{id}" ]
  ; upgrade = Some [ "uv"; "tool"; "upgrade"; "{id}" ]
  ; parse = parse_uv
  }
;;

let cargo_tool : Plugin.tool =
  { name = "cargo"
  ; prog = "cargo"
  ; list_args = [ "install"; "--list" ]
  ; install = Some [ "cargo"; "install"; "{id}" ]
  ; upgrade = None
  ; parse = parse_cargo
  }
;;

let winget_tool : Plugin.tool =
  { name = "winget"
  ; prog = "winget"
  ; list_args = [ "list"; "--accept-source-agreements"; "--verbose" ]
  ; install =
      Some
        [ "winget"
        ; "install"
        ; "--id"
        ; "{id}"
        ; "-e"
        ; "--accept-package-agreements"
        ; "--accept-source-agreements"
        ; "--silent"
        ]
  ; upgrade =
      Some
        [ "winget"
        ; "upgrade"
        ; "--id"
        ; "{id}"
        ; "--accept-package-agreements"
        ; "--accept-source-agreements"
        ]
  ; parse = parse_winget
  }
;;

(** All built-ins in scan order. *)
let built_ins : Plugin.tool list =
  [ winget_tool; npm_tool; pipx_tool; uv_tool; cargo_tool ]
;;

(** Names that custom tools may not take (shadowing would silently
    change a built-in scan). *)
let reserved_names : string list = List.map (fun (t : Plugin.tool) -> t.name) built_ins

(** Run one plugin tool: missing binary (or failure) means skipped, as
    with every built-in scanner. *)
let scan_tool (run : Proc.runner) (t : Plugin.tool) : app list =
  match run t.prog t.list_args with
  | None -> []
  | Some out -> t.parse out
;;

(** Full scan order: winget, npm, pipx, uv, cargo, then [extra] custom
    tools in file order. The winget fetch runs on its own domain while
    the rest scan in parallel, so startup costs max(winget, slowest
    other) instead of the sum. [winget] is the memoized [winget list]
    fetch ([Proc.winget_list]), so a scan followed by an update check
    spawns winget once per window. *)
let scan_all
      (run : Proc.runner)
      ~(winget : unit -> string option)
      ~(extra : Plugin.tool list)
  : app list
  =
  let winget_dom =
    Domain.spawn (fun () ->
      match winget () with
      | None -> []
      | Some out -> parse_winget out)
  in
  let apps =
    List.concat
      (Proc.par_map8
         (scan_tool run)
         ([ npm_tool; pipx_tool; uv_tool; cargo_tool ] @ extra))
  in
  Domain.join winget_dom @ apps
;;
