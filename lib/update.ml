(** On-demand update checks for non-winget package managers.

    Only manifest items drive checks, so cost stays bounded; registry
    lookups run in parallel on up to 8 domains. Results are [(value,
    available)] pairs the TUI marks [NeedsUpdate]. Version comparison
    is dumb string inequality, like the winget path — except npm, where
    [outdated] itself decides by semver. *)

open Manifest

(** npm: one [outdated -g --json] spawn covers every global. npm exits
    1 when updates exist (fatal to our runner, which drops non-zero
    output), so the shell forces exit 0. [os] pins the shell choice
    for tests; defaults to [Sys.os_type]. *)
let npm_updates ?(os = Sys.os_type) (run : Proc.runner) : (string * string) list =
  let prog, args =
    if os = "Win32" || os = "Cygwin"
    then "cmd", [ "/c"; "npm outdated -g --json || exit 0" ]
    else "sh", [ "-c"; "npm outdated -g --json || true" ]
  in
  let parse out =
    try
      match Yojson.Safe.from_string out with
      | `Assoc pkgs ->
        List.filter_map
          (fun (name, js) ->
             match js with
             | `Assoc fields ->
               (match List.assoc_opt "latest" fields with
                | Some (`String latest) when latest <> "" -> Some (name, latest)
                | _ -> None)
             | _ -> None)
          pkgs
      | _ -> []
    with
    | _ -> []
  in
  match run prog args with
  | None -> []
  | Some out -> parse out
;;

let parse_json (body : string) : Yojson.Safe.t option =
  try Some (Yojson.Safe.from_string body) with
  | _ -> None
;;

let pypi_latest (js : Yojson.Safe.t) : string option =
  match js with
  | `Assoc top ->
    (match List.assoc_opt "info" top with
     | Some (`Assoc info) ->
       (match List.assoc_opt "version" info with
        | Some (`String v) -> Some v
        | _ -> None)
     | _ -> None)
  | _ -> None
;;

let cargo_latest (js : Yojson.Safe.t) : string option =
  match js with
  | `Assoc top ->
    (match List.assoc_opt "crate" top with
     | Some (`Assoc crate) ->
       (match List.assoc_opt "max_version" crate with
        | Some (`String v) -> Some v
        | _ -> None)
     | _ -> None)
  | _ -> None
;;

(** One registry URL per package, checked in parallel; a stale
    installed version (or any fetch/parse failure) simply yields no
    update for that package. *)
let registry_updates
      (fetch : Fetch.fetch)
      (headers : (string * string) list)
      (url_of : string -> string)
      (latest_of : Yojson.Safe.t -> string option)
      (versions : (string * string) list)
  : (string * string) list
  =
  let one (name, installed) =
    if name = "" || installed = ""
    then None
    else (
      (* 15s cap like other API calls: without it a dead network hangs
         the TUI until curl gives up on its own. *)
      match fetch ~timeout_s:15 (url_of name) headers with
      | Error _ -> None
      | Ok body ->
        (match parse_json body with
         | None -> None
         | Some js ->
           (match latest_of js with
            | Some v when v <> "" && v <> installed -> Some (name, v)
            | _ -> None)))
  in
  List.filter_map Fun.id (Proc.par_map8 one versions)
;;

let pypi_updates (fetch : Fetch.fetch) (versions : (string * string) list)
  : (string * string) list
  =
  registry_updates
    fetch
    []
    (fun n -> Printf.sprintf "https://pypi.org/pypi/%s/json" n)
    pypi_latest
    versions
;;

let cargo_updates (fetch : Fetch.fetch) (versions : (string * string) list)
  : (string * string) list
  =
  registry_updates
    fetch
    [ "User-Agent", "devkit (https://github.com/proxylat/devkit)" ]
    (fun n -> Printf.sprintf "https://crates.io/api/v1/crates/%s" n)
    cargo_latest
    versions
;;

(** Group Installed Pm items by manager: npm via [outdated], pipx and
    uv via PyPI, cargo via crates.io. Anything else (winget, links,
    unknown Pms, non-Installed rows) is ignored. *)
let check_all ~(run : Proc.runner) ~(fetch : Fetch.fetch) (items : item list)
  : (string * string) list
  =
  let installed =
    List.filter_map
      (fun it ->
         match it.typ with
         | Pm pm when it.status = Installed -> Some (pm, it.value, it.installed_version)
         | _ -> None)
      items
  in
  let has pm = List.exists (fun (p, _, _) -> p = pm) installed in
  let of_pm pm =
    List.filter_map (fun (p, v, i) -> if p = pm then Some (v, i) else None) installed
  in
  let npm = if has "npm" then npm_updates run else [] in
  let pypi = pypi_updates fetch (of_pm "pipx" @ of_pm "uv") in
  let cargo = cargo_updates fetch (of_pm "cargo") in
  List.sort_uniq compare (npm @ pypi @ cargo)
;;
