(** winget location, portable bootstrap and download. *)

type spawn = string -> string list -> string * bool

(** Real backend on top of [Unix.open_process_args_in]. *)
val real_spawn : spawn

(** Injected OS surface. Tests fake the filesystem/spawns; production uses
    {!real_io}. *)
type io =
  { getenv : string -> string option
  ; is_file : string -> bool
  ; spawn : spawn
  ; cwd : unit -> (string, string) result
  ; exe_dir : unit -> (string, string) result
  ; mkdir_p : string -> (unit, string) result
  ; cache_dir : unit -> string
  ; latest_winget_cli : unit -> (Gh.release, string) result
  ; download : url:string -> (string, string) result
  ; unzip : zip:string -> dir:string -> (unit, string) result
  }

(** Production wiring: curl fetcher, digestif verifier, central-dir zip reader. *)
val real_io : Fetch.fetch -> io

val find_portable : is_file:(string -> bool) -> string -> string

val find_on_path
  :  ?os:string
  -> getenv:(string -> string option)
  -> is_file:(string -> bool)
  -> string
  -> string

val winget_runs : spawn:spawn -> string -> bool
val resolve_via_cmd : spawn:spawn -> string
val resolve : io -> string
val is_cert_error : string -> bool
val cert_advice : string
val find_msixbundle : Gh.release -> Gh.asset option
val download_winget : io -> (string, string) result
val format_size : int64 -> string
val make_path_cache : unit -> (io -> string) * (unit -> unit)

(** Locate winget, downloading it when nothing resolves. A non-empty
    [~override_path] is returned blindly. The download only runs when
    [~os] is ["Win32"] (default: [Sys.os_type]). *)
val ensure_full
  :  override_path:string
  -> resolve_path:(io -> string)
  -> ?os:string
  -> io
  -> (string, string) result

val ensure : override_path:string -> io -> (string, string) result
