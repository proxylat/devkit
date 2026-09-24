(** Package installation across winget, GitHub releases and plain URLs. *)

type status =
  | Installed
  | Updated
  | Opened
  | Skipped of string
  | Failed of string

val status_to_string : status -> string

type outcome =
  { value : string
  ; status : status
  }

(** Injected surface. {!real_deps} wires production backends. *)
type deps =
  { winget : unit -> string
  ; spawn : Bootstrap.spawn
  ; open_browser : string -> (unit, string) result
  ; latest_release : string -> (Gh.release, string) result
  ; download : url:string -> (string, string) result
  ; run_installer : string -> (unit, string) result
  ; tools : Plugin.tool list
  }

(** Run a downloaded installer: [.msi] via [msiexec], [.exe] with silent
    switches, anything else refused. *)
val run_installer : Bootstrap.spawn -> string -> (unit, string) result

val real_open_browser : ?os:string -> Bootstrap.spawn -> string -> (unit, string) result
val install : deps -> string -> string -> bool -> outcome
val real_deps : Fetch.fetch -> winget_override:string -> unit -> deps
