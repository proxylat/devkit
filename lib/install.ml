(** App install/update dispatch.

    Status strings are [installed], [updated], [opened], [skip], [error].
    Flag sets: [winget install --id … -e] with both [--accept-…] plus
    [--silent], [winget upgrade --id …] with both accepts, and the [.exe]
    installer receiving all three silent switches in one invocation.

    Design notes:

    - Winget failures read [winget <args…> failed: <output>]:
      {!Bootstrap.spawn} reports only output + ok, without the OS exit
      error.
    - [open_browser] waits for the opener to exit. A browser that
      daemonizes returns at once either way; a missing opener surfaces
      here instead of silently succeeding.
    - The empty-hash hole (hash computed, never checked) is closed by
      construction: {!deps.download} is [Hash.download_and_verify] with an
      explicit [No_expected]. *)

(** Outcome of installing one app. *)
type status =
  | Installed
  | Updated
  | Opened
  | Skipped of string
  | Failed of string

let status_to_string = function
  | Installed -> "installed"
  | Updated -> "updated"
  | Opened -> "opened"
  | Skipped _ -> "skip"
  | Failed _ -> "error"
;;

type outcome =
  { value : string
  ; status : status
  }

let succeed value status = { value; status }
let fail value msg = { value; status = Failed msg }

(** Injected surface. {!real_deps} wires production backends. *)
type deps =
  { winget : unit -> string
  ; spawn : Bootstrap.spawn
  ; open_browser : string -> (unit, string) result
  ; latest_release : string -> (Gh.release, string) result
  ; download : url:string -> (string, string) result
  ; run_installer : string -> (unit, string) result
  ; tools : Plugin.tool list
  }

(** Run a downloaded installer: [.msi] via [msiexec], [.exe] with silent
    switches, anything else refused. Extension match is case-insensitive. *)
let run_installer (spawn : Bootstrap.spawn) (path : string) : (unit, string) result =
  let ext = String.lowercase_ascii (Filename.extension path) in
  match ext with
  | ".msi" ->
    (match spawn "msiexec" [ "/i"; path; "/quiet"; "/norestart" ] with
     | _, true -> Ok ()
     | out, false -> Error ("msiexec failed: " ^ out))
  | ".exe" ->
    (match spawn path [ "/S"; "/silent"; "/verysilent" ] with
     | _, true -> Ok ()
     | out, false -> Error ("installer failed: " ^ out))
  | _ -> Error ("unsupported installer type: " ^ ext)
;;

(** Open [url] in the default browser: [cmd /c start] on Windows, [open]
    on macOS, [xdg-open] elsewhere. macOS is detected with [uname -s]
    through [spawn], keeping the function testable. *)
let real_open_browser (spawn : Bootstrap.spawn) (url : string) : (unit, string) result =
  let prog, args =
    if Sys.win32
    then "cmd", [ "/c"; "start"; ""; url ]
    else (
      let uname, ok = spawn "uname" [ "-s" ] in
      if ok && String.trim uname = "Darwin" then "open", [ url ] else "xdg-open", [ url ])
  in
  match spawn prog args with
  | _, true -> Ok ()
  | out, false -> Error ("open browser failed: " ^ out)
;;

(** Non-zero winget exits that still mean success (already-installed
    readings, matched case-sensitively). *)
let already_markers = [ "already installed"; "No newer package versions are available" ]

let install_winget (d : deps) (id : string) (update : bool) : outcome =
  let w = d.winget () in
  if w = ""
  then fail id "winget unavailable"
  else (
    let args =
      if update
      then
        [ "upgrade"
        ; "--id"
        ; id
        ; "--accept-package-agreements"
        ; "--accept-source-agreements"
        ]
      else
        [ "install"
        ; "--id"
        ; id
        ; "-e"
        ; "--accept-package-agreements"
        ; "--accept-source-agreements"
        ; "--silent"
        ]
    in
    let out, ok = d.spawn w args in
    let done_status = if update then Updated else Installed in
    if ok
    then succeed id done_status
    else if List.exists (fun m -> Strutil.contains_substring m out) already_markers
    then succeed id done_status
    else fail id ("winget " ^ String.concat " " args ^ " failed: " ^ String.trim out))
;;

let install_github (d : deps) (repo : string) : outcome =
  match d.latest_release repo with
  | Error e -> fail repo e
  | Ok rel ->
    (match Gh.match_by_arch rel.Gh.assets with
     | None ->
       (* No Windows installer asset: open the release page instead. *)
       let page = "https://github.com/" ^ repo ^ "/releases/latest" in
       (match d.open_browser page with
        | Ok () -> succeed repo Opened
        | Error e -> fail repo e)
     | Some asset ->
       (match d.download ~url:asset.Gh.browser_download_url with
        | Error e -> fail repo e
        | Ok path ->
          (match d.run_installer path with
           | Ok () -> succeed repo Installed
           | Error e -> fail repo e)))
;;

(** Install/update via a plugin tool's command template. Only [{id}]
    is substituted (install takes no version). A missing template or a
    failed spawn is a [Failed] outcome rather than a skip. *)
let install_plugin (d : deps) (t : Plugin.tool) (id : string) (update : bool) : outcome =
  let kind = if update then "upgrade" else "install" in
  match if update then t.upgrade else t.install with
  | None -> fail id (t.name ^ " has no " ^ kind ^ " template")
  | Some argv ->
    (match Plugin.expand [ "id", id ] argv with
     | Error e -> fail id e
     | Ok [] -> fail id (t.name ^ ": empty " ^ kind ^ " template")
     | Ok (prog :: args) ->
       (match d.spawn prog args with
        | _, true -> succeed id (if update then Updated else Installed)
        | out, false -> fail id (t.name ^ " " ^ kind ^ " failed: " ^ String.trim out)))
;;

(** Dispatch an install/update for one manifest entry. [kind] is one of
    ["winget"], ["github"], ["url"], a plugin tool name, or anything
    else (a skip). winget keeps its special path (already-installed
    readings); its plugin entry only documents the equivalent command. *)
let install (d : deps) (kind : string) (value : string) (update : bool) : outcome =
  match kind with
  | "winget" -> install_winget d value update
  | "github" -> install_github d value
  | "url" ->
    (match d.open_browser value with
     | Ok () -> succeed value Opened
     | Error e -> fail value e)
  | other ->
    (match Plugin.find other d.tools with
     | None -> { value; status = Skipped ("unsupported type \"" ^ other ^ "\"") }
     | Some t -> install_plugin d t value update)
;;

(** Production wiring: resolve winget via {!Bootstrap} (empty string when
    unavailable), curl fetcher, real spawns. *)
let real_deps (fetch : Fetch.fetch) ~winget_override () : deps =
  let io = Bootstrap.real_io fetch in
  { winget =
      (fun () ->
        match Bootstrap.ensure ~override_path:winget_override io with
        | Ok p -> p
        | Error _ -> "")
  ; spawn = Bootstrap.real_spawn
  ; open_browser = real_open_browser Bootstrap.real_spawn
  ; latest_release = Gh.latest_release fetch
  ; download = (fun ~url -> Hash.download_and_verify fetch Hash.No_expected url)
  ; run_installer = run_installer Bootstrap.real_spawn
  ; tools = Inventory.built_ins
  }
;;
