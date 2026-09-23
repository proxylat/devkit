(** User-defined package-manager tools ([tools.sexp]). *)

(** How to read [name]/[version] rows out of a tool's list output. *)
type row_spec =
  { skip_prefixes : string list
  ; skip_res : Re.re list
  ; row_re : Re.re
  ; strip_version_prefix : string
  }

(** A package-manager tool: scan program plus optional install/upgrade
    templates. [parse] turns list output into apps tagged with [name]. *)
type tool =
  { name : string
  ; prog : string
  ; list_args : string list
  ; install : string list option
  ; upgrade : string list option
  ; parse : string -> Dashboard.app list
  }

val parse_rows : row_spec -> string -> string -> Dashboard.app list

(** Expand [{key}] placeholders from [vars]; unknown or unclosed
    placeholders are errors. *)
val expand : (string * string) list -> string list -> (string list, string) result

val tool_of_sexp : source:string -> index:int -> Sexplib.Sexp.t -> (tool, string) result

(** Parse one whole [(tools ...)] file. *)
val load_string : source:string -> string -> (tool list, string) result

(** A missing file means no custom tools; a broken one is an error.
    [read] is the file reader (injected for tests). *)
val load_file : read:(string -> string option) -> string -> (tool list, string) result

(** Load several files in order; first file wins per tool name. Tools
    named in [reserved] (built-ins) are rejected. *)
val load_many
  :  read:(string -> string option)
  -> ?reserved:string list
  -> string list
  -> (tool list, string) result

(** Case-insensitive lookup. *)
val find : string -> tool list -> tool option

val config_file : unit -> string option
val default_paths : unit -> string list
