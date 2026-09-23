(** winget import-compatible JSON export. *)

val to_json : ?now:float -> Dashboard.app list -> Yojson.Basic.t
val to_string : ?now:float -> Dashboard.app list -> string
