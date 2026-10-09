(** Tag-immutability lockfile (devkit.lock) parsing and rendering.

    After every vendor-direct install devkit records [(id, tag, sha256)],
    plus an optional code-signing certificate thumbprint and the manifest
    host for continuity checks. The lockfile is TOML: an [[entry]] array
    of [{ id; tag; sha256 }] tables with optional [thumbprint]/[host]
    keys. Parsing is total: malformed TOML yields an empty list and
    missing keys default to [""]. *)

type entry =
  { id : string
  ; tag : string
  ; sha256 : string
  ; thumbprint : string (** "" means unset *)
  ; host : string (** "" means unset *)
  }

type t = entry list

let filename = "devkit.lock"

open Toml.Lenses

let get_str tbl k =
  match get tbl (key k |-- string) with
  | Some s -> s
  | None -> ""
;;

(** [parse text] parses a lockfile. *)
let parse (text : string) : entry list =
  try
    match Toml.Parser.from_string text with
    | `Error _ -> []
    | `Ok tbl ->
      (match get tbl (key "entry" |-- array |-- tables) with
       | None -> []
       | Some tables ->
         List.map
           (fun e ->
              { id = get_str e "id"
              ; tag = get_str e "tag"
              ; sha256 = get_str e "sha256"
              ; thumbprint = get_str e "thumbprint"
              ; host = get_str e "host"
              })
           tables)
  with
  | _ -> []
;;

let str k v = Toml.Min.key k, Toml.Types.TString v

(** [to_string entries] renders a lockfile back to TOML. *)
let to_string (entries : entry list) : string =
  let one e =
    Toml.Min.of_key_values
      ([ str "id" e.id; str "tag" e.tag; str "sha256" e.sha256 ]
       @ (if e.thumbprint <> "" then [ str "thumbprint" e.thumbprint ] else [])
       @ if e.host <> "" then [ str "host" e.host ] else [])
  in
  let top =
    Toml.Min.of_key_values
      [ ( Toml.Min.key "entry"
        , Toml.Types.TArray (Toml.Types.NodeTable (List.map one entries)) )
      ]
  in
  Toml.Printer.string_of_table top
;;

(** [find entries id] looks up the entry with the given id. *)
let find (entries : entry list) (id : string) : entry option =
  List.find_opt (fun e -> e.id = id) entries
;;

(** [upsert entries e] replaces the entry with the same id, else appends. *)
let upsert (entries : entry list) (e : entry) : entry list =
  if List.exists (fun x -> x.id = e.id) entries
  then List.map (fun x -> if x.id = e.id then e else x) entries
  else entries @ [ e ]
;;
