(** Manifest (devkit.toml) parsing and rendering.

    The manifest is TOML: an optional top-level [winget_path] plus a
    [[section]] array, each section holding a [name] and a [package]
    array of [{ id }] tables with optional version fields. The id carries
    its own source, first match wins: a github.com URL canonicalizes to
    its [owner/repo]; a gitlab.com or codeberg.org URL infers its forge
    the same way; any other URL is a plain link; [prefix:name] is
    explicit ([winget:]/[github:]/[gitlab:]/[codeberg:]/[forgejo:]/[url:]
    or a package-manager name); [@scope/pkg] is npm; [owner/repo] is
    GitHub; dotted [Publisher.App] is winget; a bare word is ambiguous
    and rejected. Self-hosted forges need the prefix with the full URL
    ([gitlab:https://host/group/project]) since no static rule can tell
    them apart. A leftover [pm] key is also rejected. A package may also carry [upstream]: version
    truth and downloads then come from the vendor's releases instead of
    the community manifest. A top-level [sources] array selects the scan
    sources in order (["registry" first wins ties); missing means
    {!default_sources}. A top-level [quarantine_days] holds back fresh
    releases globally; a per-package [quarantine_days] overrides it per
    row. Parsing is total: malformed TOML yields an
    empty result, rejected rows are skipped with a [warnings] entry
    naming the fix. Statuses are never persisted; they are assigned at
    runtime by the merge step. *)

let filename = "devkit.toml"

type item_type =
  | Winget
  | GitHub
  | GitLab of string (** forge host, always explicit, e.g. ["gitlab.com"] *)
  | Forgejo of string (** forge host, always explicit, e.g. ["codeberg.org"] *)
  | Url
  | Pm of string
  | Registry (** Windows uninstall entry: discovery-only, never persisted *)

type status =
  | Installed
  | NeedsUpdate
  | NotFound
  | New
  | Manual

type item =
  { typ : item_type
  ; value : string
  ; installed_version : string
  ; available_version : string
  ; status : status
  ; upstream : string
  ; quarantine_days : int
  }

type section =
  { name : string
  ; items : item list
  }

type parse_result =
  { sections : section list
  ; winget_path : string
  ; warnings : string list (** skipped rows, in document order, naming the fix *)
  ; sources : string list (** scan sources in order; {!default_sources} when unset *)
  ; quarantine : int
  }

(** Scan sources when the manifest sets none: classic order, registry last. *)
let default_sources = [ "winget"; "npm"; "pipx"; "uv"; "cargo"; "registry" ]

let make_item typ value =
  { typ
  ; value
  ; installed_version = ""
  ; available_version = ""
  ; upstream = ""
  ; quarantine_days = 0
  ; (* Fresh items carry [NotFound]; statuses are assigned later by the
       merge step. *)
    status = NotFound
  }
;;

let type_string = function
  | Winget -> "winget"
  | GitHub -> "github"
  | GitLab _ -> "gitlab"
  | Forgejo _ -> "forgejo"
  | Url -> "url"
  | Pm s -> s
  | Registry -> "registry"
;;

let item_type_of_string s =
  match String.lowercase_ascii s with
  | "winget" -> Winget
  | "github" -> GitHub
  | "gitlab" -> GitLab "gitlab.com"
  | "forgejo" | "gitea" -> Forgejo "codeberg.org"
  | "codeberg" -> Forgejo "codeberg.org"
  | "url" -> Url
  | "registry" -> Registry
  | _ -> Pm s
;;

open Toml.Lenses

let get_str tbl k =
  match get tbl (key k |-- string) with
  | Some s -> s
  | None -> ""
;;

(** Clamped N-day value: negatives are 0, non-ints are [None] (ignored,
    like other malformed keys). *)
let get_days tbl k =
  match get tbl (key k |-- int) with
  | Some n -> Some (max n 0)
  | None -> None
;;

let has_blank s =
  String.contains s ' ' || String.contains s '\t' || String.contains s '\n'
;;

(** [owner/repo] shape: two non-empty segments, nothing exotic. *)
let is_repo_shape s =
  (not (has_blank s))
  && (not (String.contains s ':'))
  && (not (String.contains s '\\'))
  && (not (Strutil.contains_substring "://" s))
  &&
  match List.filter (fun x -> x <> "") (String.split_on_char '/' s) with
  | [ _; _ ] -> true
  | _ -> false
;;

let check_plain s (ok : unit -> (item_type * string, string) result) =
  if has_blank s || String.contains s '\\'
  then Error "no spaces or backslashes allowed"
  else ok ()
;;

let check_dotted id =
  let segs = String.split_on_char '.' id in
  if has_blank id || String.contains id '\\' || List.exists (fun x -> x = "") segs
  then Error "want Publisher.App"
  else Ok (Winget, id)
;;

(** An explicit [kind:...] prefix must reach the prefix rung, not the
    URL rung. Unknown prefixes with a scheme stay plain links. *)
let known_prefix s =
  match String.index_opt s ':' with
  | None -> false
  | Some i ->
    (match String.sub s 0 i |> String.trim |> String.lowercase_ascii with
     | "winget" | "github" | "gitlab" | "codeberg" | "gitea" | "forgejo" | "url" -> true
     | _ -> false)
;;

(** [classify id] derives the package source from the id string (see the
    header grammar). Explicit [prefix:name] always wins; anything the
    grammar cannot place is an [Error] naming the fix. *)
let classify (id : string) : (item_type * string, string) result =
  let id = String.trim id in
  if id = ""
  then Error "empty id"
  else (
    let canon = Strutil.canon_repo id in
    if canon <> id && is_repo_shape canon
    then Ok (GitHub, canon)
    else (
      match Provider.infer_url id with
      | Some (Provider.GitLab h, path) -> Ok (GitLab h, path)
      | Some (Provider.Forgejo h, path) -> Ok (Forgejo h, path)
      | Some (Provider.GitHub, path) -> Ok (GitHub, path)
      | None ->
        if Strutil.contains_substring "://" id && not (known_prefix id)
        then Ok (Url, id)
        else (
          match String.index_opt id ':' with
          | Some i ->
            let pre = String.sub id 0 i |> String.trim |> String.lowercase_ascii in
            let rest = String.trim (String.sub id (i + 1) (String.length id - i - 1)) in
            (match pre, rest with
             | "", _ -> Error "empty prefix before ':'"
             | _, "" -> Error ("nothing after '" ^ pre ^ ":'")
             | "winget", r -> check_plain r (fun () -> Ok (Winget, r))
             | "github", r ->
               let c = Strutil.canon_repo r in
               if is_repo_shape c then Ok (GitHub, c) else Error "want owner/repo"
             | ("gitlab" | "codeberg" | "gitea" | "forgejo"), r ->
               (match Provider.of_prefix pre r with
                | Ok (Provider.GitLab h, path) -> Ok (GitLab h, path)
                | Ok (Provider.Forgejo h, path) -> Ok (Forgejo h, path)
                | Ok (Provider.GitHub, path) -> Ok (GitHub, path)
                | Error reason -> Error reason)
             | "url", r -> Ok (Url, r)
             | p, r -> check_plain r (fun () -> Ok (Pm p, r)))
          | None ->
            if id.[0] = '@' && String.contains id '/'
            then check_plain id (fun () -> Ok (Pm "npm", id))
            else if String.contains id '/'
            then if is_repo_shape id then Ok (GitHub, id) else Error "want owner/repo"
            else if String.contains id '.'
            then check_dotted id
            else Error ("ambiguous, try npm:" ^ id))))
;;

(** [parse text] parses a devkit.toml manifest. *)
let parse (text : string) : parse_result =
  match Toml.Parser.from_string text with
  | `Error _ ->
    { sections = []
    ; winget_path = ""
    ; warnings = []
    ; sources = default_sources
    ; quarantine = 0
    }
  | `Ok tbl ->
    let winget_path = get_str tbl "winget_path" in
    let quarantine = get_days tbl "quarantine_days" |> Option.value ~default:0 in
    let sources =
      match get tbl (key "sources" |-- array |-- strings) with
      | Some (_ :: _ as ss) -> ss
      | _ -> default_sources
    in
    let warnings = ref [] in
    let skipped id reason =
      warnings := Printf.sprintf "package %S skipped: %s" id reason :: !warnings
    in
    let sections =
      match get tbl (key "section" |-- array |-- tables) with
      | None -> []
      | Some secs ->
        List.filter_map
          (fun sec ->
             let name = get_str sec "name" in
             if name = ""
             then None
             else (
               let items =
                 match get sec (key "package" |-- array |-- tables) with
                 | None -> []
                 | Some pkgs ->
                   List.filter_map
                     (fun pkg ->
                        match
                          get pkg (key "pm" |-- string), get pkg (key "id" |-- string)
                        with
                        | Some pm, Some id when String.trim id <> "" ->
                          let id = String.trim id
                          and pm = String.trim pm in
                          skipped id ("pm is gone, use id = \"" ^ pm ^ ":" ^ id ^ "\"");
                          None
                        | Some pm, _ ->
                          skipped "?" ("pm \"" ^ pm ^ "\" is gone and no id given");
                          None
                        | None, Some id ->
                          (match classify id with
                           | Ok (typ, value) ->
                             let quarantine_days =
                               get_days pkg "quarantine_days"
                               |> Option.value ~default:quarantine
                             in
                             Some
                               { (make_item typ value) with
                                 installed_version = get_str pkg "installed_version"
                               ; available_version = get_str pkg "available_version"
                               ; upstream = get_str pkg "upstream"
                               ; quarantine_days
                               }
                           | Error reason ->
                             skipped (String.trim id) reason;
                             None)
                        | None, None -> None)
                     pkgs
               in
               Some { name; items }))
          secs
    in
    { sections; winget_path; warnings = List.rev !warnings; sources; quarantine }
;;

let str k v = Toml.Min.key k, Toml.Types.TString v
let str_list k vs = Toml.Min.key k, Toml.Types.TArray (Toml.Types.NodeString vs)
let num k v = Toml.Min.key k, Toml.Types.TInt v

(** [to_string r] renders a manifest back to TOML. *)
let to_string (r : parse_result) : string =
  let id_of_item it =
    match it.typ with
    | Winget | GitHub | Url -> it.value
    | GitLab h ->
      if h = "gitlab.com"
      then "gitlab:" ^ it.value
      else "gitlab:https://" ^ h ^ "/" ^ it.value
    | Forgejo h ->
      if h = "codeberg.org"
      then "codeberg:" ^ it.value
      else "forgejo:https://" ^ h ^ "/" ^ it.value
    | Pm p -> p ^ ":" ^ it.value
    | Registry -> "registry:" ^ it.value
  in
  let pkg it =
    Toml.Min.of_key_values
      ([ str "id" (id_of_item it) ]
       @ (if it.upstream <> "" then [ str "upstream" it.upstream ] else [])
       @ (if it.quarantine_days <> 0
          then [ num "quarantine_days" it.quarantine_days ]
          else [])
       @ (if it.installed_version <> ""
          then [ str "installed_version" it.installed_version ]
          else [])
       @
       if it.available_version <> ""
       then [ str "available_version" it.available_version ]
       else [])
  in
  let sec (s : section) =
    Toml.Min.of_key_values
      [ str "name" s.name
      ; ( Toml.Min.key "package"
        , Toml.Types.TArray (Toml.Types.NodeTable (List.map pkg s.items)) )
      ]
  in
  let top =
    (if r.winget_path <> "" then [ str "winget_path" r.winget_path ] else [])
    (* Fragments (append) carry [sources = []] and must not emit the key:
       a duplicate top-level key would invalidate the manifest. Defaults
       are omitted too: emitting them would pin today's order against
       future additions. *)
    @ (if r.sources <> [] && r.sources <> default_sources
       then [ str_list "sources" r.sources ]
       else [])
    @ (if r.quarantine <> 0 then [ num "quarantine_days" r.quarantine ] else [])
    @ [ ( Toml.Min.key "section"
        , Toml.Types.TArray (Toml.Types.NodeTable (List.map sec r.sections)) )
      ]
  in
  Toml.Printer.string_of_table (Toml.Min.of_key_values top)
;;
