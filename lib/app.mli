(** CLI orchestration: the [run*] commands. *)

val pm_order : string list

type fs =
  { read_file : string -> string option
  ; append_file : string -> string -> (unit, string) result
  ; write_file : string -> string -> (unit, string) result
  }

type env =
  { run : Proc.runner
  ; fs : fs
  ; bio : Bootstrap.io
  ; fetch : Fetch.fetch
  }

type scan =
  { apps : Dashboard.app list
  ; info : Winget_parse.info Winget_parse.IdMap.t option
  ; winget : string
  ; winget_error : string
  ; sources_error : string
  }

(** Missing/unparseable file yields empty sections and default sources. *)
val load_manifest : fs -> string -> Manifest.section list * string * string list

val winget_show : Bootstrap.io -> string -> string -> string

(** Vendor truth for [upstream] rows: [(provider, repo)] → latest
    release tag ([None] on any failure). *)
val upstream_ver_of_fetch : Fetch.fetch -> Provider.t -> string -> string option

val scan
  :  env
  -> ?os:string
  -> override_path:string
  -> extra:Plugin.tool list
  -> sources:string list
  -> unit
  -> scan

val default_view : env -> tools:Plugin.tool list -> string
val import_view : env -> ?tools:Plugin.tool list -> string -> (string, string) result
val append_selected : fs -> string -> Manifest.item list -> (unit, string) result
val new_items : ?extra:bool -> Manifest.section list -> Manifest.item list
val run_add : env -> ?tools:Plugin.tool list -> string list -> string list

val run_append
  :  env
  -> ?tools:Plugin.tool list
  -> ?os:string
  -> string list
  -> string list

val run_append_new : env -> tools:Plugin.tool list -> ?os:string -> unit -> string list
val json_sibling : string -> string
val order_for : Plugin.tool list -> string list

(** Case-insensitive validation of raw [sources] names against
    [order_for tools]; [] means absent (the defaults). *)
val resolve_sources
  :  string list
  -> tools:Plugin.tool list
  -> (string list, string) result

val run_export
  :  env
  -> ?tools:Plugin.tool list
  -> ?os:string
  -> ?only:string list
  -> ?except:string list
  -> string
  -> string list
