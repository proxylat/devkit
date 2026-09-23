(** Manifest (pkgs.txt) parsing. *)

type item_type =
  | Winget
  | GitHub
  | Url

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
val parse : string -> parse_result
