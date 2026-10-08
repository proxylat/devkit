(** Manifest (devkit.toml) parsing and rendering.

    The manifest is TOML: an optional top-level [winget_path] plus a
    [[section]] array, each section holding a [name] and a [package]
    array of [{ id }] tables with optional version fields. The id carries
    its own source, first match wins: a github.com URL canonicalizes to
    its [owner/repo]; any other URL is a plain link; [prefix:name] is
    explicit ([winget:]/[github:]/[url:] or a package-manager name);
    [@scope/pkg] is npm; [owner/repo] is GitHub; dotted [Publisher.App]
    is winget; a bare word is ambiguous and rejected. A leftover [pm]
    key is also rejected. A package may also carry [upstream]: version
    truth and downloads then come from the vendor's releases instead of
    the community manifest. Parsing is total: malformed TOML yields an
    empty result, rejected rows are skipped with a [warnings] entry
    naming the fix. Statuses are never persisted; they are assigned at
    runtime by the merge step. *)

let filename = "devkit.toml"

type item_type =
  | Winget
  | GitHub
  | Url
  | Pm of string

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
  }

type section =
  { name : string
  ; items : item list
  }

type parse_result =
  { sections : section list
  ; winget_path : string
  ; warnings : string list (** skipped rows, in document order, naming the fix *)
  }

let make_item typ value =
  { typ
  ; value
  ; installed_version = ""
  ; available_version = ""
  ; upstream = ""
  ; (* Fresh items carry [NotFound]; statuses are assigned later by the
       merge step. *)
    status = NotFound
  }
;;

let type_string = function
  | Winget -> "winget"
  | GitHub -> "github"
  | Url -> "url"
  | Pm s -> s
;;

let item_type_of_string s =
  match String.lowercase_ascii s with
  | "winget" -> Winget
  | "github" -> GitHub
  | "url" -> Url
  | _ -> Pm s
;;

open Toml.Lenses

let get_str tbl k =
  match get tbl (key k |-- string) with
  | Some s -> s
  | None -> ""
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

(** Explicit [url:...] must reach the prefix rung, not the URL rung. *)
let url_prefixed s =
  match String.index_opt s ':' with
  | None -> false
  | Some i -> String.sub s 0 i |> String.trim |> String.lowercase_ascii = "url"
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
    else if Strutil.contains_substring "://" id && not (url_prefixed id)
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
         | "url", r -> Ok (Url, r)
         | p, r -> check_plain r (fun () -> Ok (Pm p, r)))
      | None ->
        if id.[0] = '@' && String.contains id '/'
        then check_plain id (fun () -> Ok (Pm "npm", id))
        else if String.contains id '/'
        then if is_repo_shape id then Ok (GitHub, id) else Error "want owner/repo"
        else if String.contains id '.'
        then check_dotted id
        else Error ("ambiguous, try npm:" ^ id)))
;;

(** [parse text] parses a devkit.toml manifest. *)
let parse (text : string) : parse_result =
  match Toml.Parser.from_string text with
  | `Error _ -> { sections = []; winget_path = ""; warnings = [] }
  | `Ok tbl ->
    let winget_path = get_str tbl "winget_path" in
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
                             Some
                               { (make_item typ value) with
                                 installed_version = get_str pkg "installed_version"
                               ; available_version = get_str pkg "available_version"
                               ; upstream = get_str pkg "upstream"
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
    { sections; winget_path; warnings = List.rev !warnings }
;;

let str k v = Toml.Min.key k, Toml.Types.TString v

(** [to_string r] renders a manifest back to TOML. *)
let to_string (r : parse_result) : string =
  let id_of_item it =
    match it.typ with
    | Winget | GitHub | Url -> it.value
    | Pm p -> p ^ ":" ^ it.value
  in
  let pkg it =
    Toml.Min.of_key_values
      ([ str "id" (id_of_item it) ]
       @ (if it.upstream <> "" then [ str "upstream" it.upstream ] else [])
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
    @ [ ( Toml.Min.key "section"
        , Toml.Types.TArray (Toml.Types.NodeTable (List.map sec r.sections)) )
      ]
  in
  Toml.Printer.string_of_table (Toml.Min.of_key_values top)
;;
