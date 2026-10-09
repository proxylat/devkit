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
  ; warnings : string list
  }

let succeed ?(warnings = []) value status = { value; status; warnings }
let fail ?(warnings = []) value msg = { value; status = Failed msg; warnings }

(** Injected surface. {!real_deps} wires production backends. *)
type deps =
  { winget : unit -> string
  ; spawn : Bootstrap.spawn
  ; open_browser : string -> (unit, string) result
  ; latest_release : Provider.t -> string -> (Gh.release, string) result
  ; download : url:string -> (string, string) result
  ; fetch : Fetch.fetch
  ; run_installer : string -> (unit, string) result
  ; lock_load : unit -> Lockfile.t
  ; lock_save : Lockfile.t -> unit
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
let real_open_browser ?(os = Sys.os_type) (spawn : Bootstrap.spawn) (url : string)
  : (unit, string) result
  =
  let prog, args =
    if os = "Win32"
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

let repo_page (repo : string) : string = "https://github.com/" ^ Strutil.canon_repo repo

(** Quarantine: refuse releases younger than [days]. A missing or
    unparseable date skips the check — there is nothing to judge age by. *)
let quarantine_block (days : int) (rel : Gh.release) : string option =
  if days <= 0 || rel.Gh.published_at = ""
  then None
  else (
    match Strutil.parse_iso_date rel.Gh.published_at with
    | None -> None
    | Some pub ->
      let tm = Unix.gmtime (Unix.time ()) in
      let today = tm.Unix.tm_year + 1900, tm.Unix.tm_mon + 1, tm.Unix.tm_mday in
      let age = Strutil.days_between pub today in
      if age < days
      then
        Some
          (Printf.sprintf
             "quarantined: %s released %s (%dd old, needs %dd)"
             rel.Gh.tag_name
             rel.Gh.published_at
             age
             days)
      else None)
;;

(** Post-download pipeline: tag-immutability lock, signature, checksum,
    attestation, signer continuity, then run and record. *)
let install_verified
      (d : deps)
      ~(value : string)
      ~(os : string)
      (prov : Provider.t)
      (rel : Gh.release)
      (asset : Gh.asset)
      (path : string)
      done_status
  : outcome
  =
  let lock = d.lock_load () in
  let locked = Lockfile.find lock value in
  let sha =
    match Hash.sha256_file path with
    | Ok s -> s
    | Error _ -> ""
  in
  match locked with
  | Some e
    when e.Lockfile.tag = rel.Gh.tag_name
         && e.Lockfile.sha256 <> ""
         && sha <> ""
         && e.Lockfile.sha256 <> sha ->
    (* Same tag, different bytes: the vendor re-cut the release. *)
    Verify.discard path;
    let short s = String.sub s 0 (min 12 (String.length s)) in
    fail
      value
      (Printf.sprintf
         "tag %s changed bytes since first seen (lock %s, now %s)"
         rel.Gh.tag_name
         (short e.Lockfile.sha256)
         (short sha))
  | _ ->
    let sig_v = Verify.signature ~os ~spawn:d.spawn path in
    let sum_v =
      Verify.checksum ~fetch:d.fetch rel.Gh.assets ~asset:asset.Gh.name ~path
    in
    let att_v = Verify.attestation ~os ~spawn:d.spawn ~repo:value path in
    let thumb = Verify.signer ~os ~spawn:d.spawn path in
    let signer_warn =
      match locked, thumb with
      | Some e, Some t when e.Lockfile.thumbprint <> "" && e.Lockfile.thumbprint <> t ->
        Some
          (Printf.sprintf
             "signer changed for %s: was %s, now %s"
             value
             e.Lockfile.thumbprint
             t)
      | _ -> None
    in
    let warns =
      List.filter_map
        (function
          | Verify.Warn w -> Some w
          | _ -> None)
        [ sig_v; sum_v; att_v ]
      @ (match signer_warn with Some w -> [ w ] | None -> [])
    in
    (match sig_v, sum_v, att_v with
     | Verify.Block m, _, _ | _, Verify.Block m, _ | _, _, Verify.Block m ->
       Verify.discard path;
       fail ~warnings:warns value m
     | _ ->
       (match d.run_installer path with
        | Error e -> fail ~warnings:warns value e
        | Ok () ->
          let host =
            match prov with
            | Provider.GitHub -> "github.com"
            | Provider.GitLab h | Provider.Forgejo h -> h
          in
          let thumbprint = match thumb with Some t -> t | None -> "" in
          d.lock_save
            (Lockfile.upsert
               lock
               Lockfile.
                 { id = value
                 ; tag = rel.Gh.tag_name
                 ; sha256 = sha
                 ; thumbprint
                 ; host
                 });
          succeed ~warnings:warns value done_status))
;;

let install_repo
      (d : deps)
      ~(value : string)
      ~(os : string)
      ~quarantine_days
      (prov : Provider.t)
      (repo : string)
      (update : bool)
  : outcome
  =
  let done_status = if update then Updated else Installed in
  match d.latest_release prov repo with
  | Error _ ->
    (* API unreachable: fall back to opening the repo page, mirroring
       the no-asset fallback below, so a link-like row never dead-ends. *)
    (match d.open_browser (Provider.page_url prov repo) with
     | Ok () -> succeed value Opened
     | Error e -> fail value e)
  | Ok rel ->
    (match quarantine_block quarantine_days rel with
     | Some msg -> fail value msg
     | None ->
       (match Gh.match_by_arch rel.Gh.assets with
        | None ->
          (* No Windows installer asset: open the release feed instead
             (the repo page for GitLab: no stable latest permalink). *)
          let page = Provider.page_url prov repo ^ Provider.release_suffix prov in
          (match d.open_browser page with
           | Ok () -> succeed value Opened
           | Error e -> fail value e)
        | Some asset ->
          (match d.download ~url:asset.Gh.browser_download_url with
           | Error e -> fail value e
           | Ok path -> install_verified d ~value ~os prov rel asset path done_status)))
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
    ["winget"], ["github"], ["gitlab"], ["forgejo"], ["url"], a plugin
    tool name, or anything else (a skip). winget keeps its special path
    (already-installed readings); its plugin entry only documents the
    equivalent command. A non-empty [upstream] diverts winget, github,
    gitlab, forgejo, and plugin rows to the vendor's release instead of
    their PM (the provider is parsed out of the pin); outcomes still
    carry the manifest value. Url rows always open the page. [host]
    carries the self-hosted forge host for gitlab/forgejo rows
    (defaulting to gitlab.com / codeberg.org). [quarantine_days] refuses
    releases younger than that many days (0 disables). Tag, digest, and
    signer are recorded to the lockfile on success; a re-cut tag or a
    changed signer surfaces on the next install. *)
let install
      (d : deps)
      ?(upstream : string = "")
      ?(host : string = "")
      ?(os : string = Sys.os_type)
      ?(quarantine_days : int = 0)
      (kind : string)
      (value : string)
      (update : bool)
  : outcome
  =
  match kind with
  | "winget" ->
    if upstream <> ""
    then (
      let prov, repo = Provider.of_upstream upstream in
      install_repo d ~value ~os ~quarantine_days prov repo update)
    else install_winget d value update
  | "github" ->
    let prov, repo =
      if upstream <> "" then Provider.of_upstream upstream else Provider.GitHub, value
    in
    install_repo d ~value ~os ~quarantine_days prov repo update
  | "gitlab" ->
    let host = if host = "" then "gitlab.com" else host in
    let repo = if upstream <> "" then snd (Provider.of_upstream upstream) else value in
    install_repo d ~value ~os ~quarantine_days (Provider.GitLab host) repo update
  | "forgejo" ->
    let host = if host = "" then "codeberg.org" else host in
    let repo = if upstream <> "" then snd (Provider.of_upstream upstream) else value in
    install_repo d ~value ~os ~quarantine_days (Provider.Forgejo host) repo update
  | "url" ->
    (match d.open_browser value with
     | Ok () -> succeed value Opened
     | Error e -> fail value e)
  | other ->
    if upstream <> ""
    then (
      let prov, repo = Provider.of_upstream upstream in
      install_repo d ~value ~os ~quarantine_days prov repo update)
    else (
      match Plugin.find other d.tools with
      | None ->
        { value; status = Skipped ("unsupported type \"" ^ other ^ "\""); warnings = [] }
      | Some t -> install_plugin d t value update)
;;

(** Production wiring: resolve winget via {!Bootstrap} (empty string when
    unavailable), curl fetcher, real spawns, lockfile in the working dir. *)
let real_deps
      (fetch : Fetch.fetch)
      ~winget_override
      ~(read_file : string -> (string, string) result)
      ~(write_file : string -> string -> (unit, string) result)
      ()
  : deps
  =
  let io = Bootstrap.real_io fetch in
  { winget =
      (fun () ->
        match Bootstrap.ensure ~override_path:winget_override io with
        | Ok p -> p
        | Error _ -> "")
  ; spawn = Bootstrap.real_spawn
  ; open_browser = real_open_browser Bootstrap.real_spawn
  ; latest_release = Provider.latest fetch
  ; download = (fun ~url -> Hash.download_and_verify fetch Hash.No_expected url)
  ; fetch
  ; run_installer = run_installer Bootstrap.real_spawn
  ; lock_load =
      (fun () ->
        match read_file Lockfile.filename with
        | Error _ -> []
        | Ok text -> Lockfile.parse text)
  ; lock_save =
      (fun t -> ignore (write_file Lockfile.filename (Lockfile.to_string t)))
  ; tools = Inventory.built_ins
  }
;;
