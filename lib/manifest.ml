(** Manifest (devkit.toml) parsing and rendering.

    The manifest is TOML: an optional top-level [winget_path] plus a
    [[section]] array, each section holding a [name] and a [package]
    array of [{ pm, id }] tables with optional version fields. Parsing
    is total: malformed TOML yields an empty result, unknown package
    managers and items without an id are skipped. Statuses are never
    persisted; they are assigned at runtime by the merge step. *)

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
  }

type section =
  { name : string
  ; items : item list
  }

type parse_result =
  { sections : section list
  ; winget_path : string
  }

let make_item typ value =
  { typ
  ; value
  ; installed_version = ""
  ; available_version = ""
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

(** [parse text] parses a devkit.toml manifest. *)
let parse (text : string) : parse_result =
  match Toml.Parser.from_string text with
  | `Error _ -> { sections = []; winget_path = "" }
  | `Ok tbl ->
    let winget_path = get_str tbl "winget_path" in
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
                        | Some pm, Some id when id <> "" ->
                          let typ = item_type_of_string pm in
                          Some
                            { (make_item typ id) with
                              installed_version = get_str pkg "installed_version"
                            ; available_version = get_str pkg "available_version"
                            }
                        | _ -> None)
                     pkgs
               in
               Some { name; items }))
          secs
    in
    { sections; winget_path }
;;

let str k v = Toml.Min.key k, Toml.Types.TString v

(** [to_string r] renders a manifest back to TOML. *)
let to_string (r : parse_result) : string =
  let pkg it =
    Toml.Min.of_key_values
      ([ str "pm" (type_string it.typ); str "id" it.value ]
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
