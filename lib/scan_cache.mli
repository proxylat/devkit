(** Scan-result disk cache: the TUI paints instantly from the previous
    run's inventory while a fresh scan runs in the background. *)

type snapshot =
  { apps : Dashboard.app list
  ; info : (string * string * string * string) list
    (** [(id, name, version, available)] tuples. *)
  ; saved_at : float
  }

(** Sibling [scan-cache.json] next to [Plugin.config_file ()]; [None] when
    no config dir resolves. *)
val cache_path : unit -> string option

(** Write a JSON snapshot to [path]. Best-effort: never raises. [None]
    info serializes as [[]]; the caller treats [[]] as [None] on the way
    back (see {!load}). *)
val save
  :  string
  -> Dashboard.app list
  -> Winget_parse.info Winget_parse.IdMap.t option
  -> unit

(** Read a snapshot back. [None] on missing file, corrupt JSON, schema
    mismatch (including a missing or non-1 ["version"] field), or
    anything else. An empty [info] list means the snapshot was saved
    with [None]. *)
val load : string -> snapshot option

(** Rebuild the winget info map keyed by lowercase id (the same keying
    as [Winget_parse.parse_list_table]). *)
val to_info_map
  :  (string * string * string * string) list
  -> Winget_parse.info Winget_parse.IdMap.t
