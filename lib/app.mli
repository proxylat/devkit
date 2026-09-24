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
  }

type scan =
  { apps : Dashboard.app list
  ; info : Winget_parse.info Winget_parse.IdMap.t option
  ; winget : string
  }

(** Missing/unparseable file yields empty sections. *)
val load_manifest : fs -> string -> Manifest.section list * string

val winget_show : Bootstrap.io -> string -> string -> string
val scan : env -> override_path:string -> extra:Plugin.tool list -> scan
val default_view : env -> tools:Plugin.tool list -> string
val import_view : env -> ?tools:Plugin.tool list -> string -> (string, string) result
val append_selected : fs -> string -> Manifest.item list -> (unit, string) result
val new_items : ?extra:bool -> Manifest.section list -> Manifest.item list
val run_add : env -> ?tools:Plugin.tool list -> string list -> string list
val run_append : env -> ?tools:Plugin.tool list -> string list -> string list
val run_append_new : env -> tools:Plugin.tool list -> string list
val json_sibling : string -> string
val order_for : Plugin.tool list -> string list
val run_export : env -> ?tools:Plugin.tool list -> string -> string list
