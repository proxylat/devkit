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
  ; upstream : string (** [owner/repo] or a full GitHub URL; "" disables *)
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

val make_item : item_type -> string -> item
val type_string : item_type -> string
val item_type_of_string : string -> item_type

(** Parse a manifest. The id carries its own source: github.com URLs
    canonicalize to [owner/repo], other URLs are plain links,
    [prefix:name] is explicit, [@scope/pkg] is npm, [owner/repo] is
    GitHub, dotted ids are winget, bare words are rejected — as is any
    leftover [pm] key. Rejected rows are skipped with a [warnings]
    entry; malformed TOML yields an empty result. *)
val parse : string -> parse_result

(** Render back to TOML: one [id] line per package (manager kinds keep
    their [prefix:], self-identifying shapes stay bare). *)
val to_string : parse_result -> string
