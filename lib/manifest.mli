(** Manifest (devkit.toml) parsing and rendering. *)

(** The manifest file name, resolved from the working directory. *)
val filename : string

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

val make_item : item_type -> string -> item
val type_string : item_type -> string
val item_type_of_string : string -> item_type
val parse : string -> parse_result
val to_string : parse_result -> string
