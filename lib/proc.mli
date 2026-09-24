(** Process execution with an injectable runner. *)

(** [runner prog args] runs [prog] with [args], returning stdout trimmed on
    success, [None] when the program is missing or exits non-zero. *)
type runner = string -> string list -> string option

(** Real runner on top of [Unix.open_process_args_in]. *)
val default_runner : runner

(** Memoized [winget list --verbose] fetcher with resetter: one winget
    invocation per 8s window per resolved path. *)
val winget_list : runner -> (string -> string option) * (unit -> unit)

(** Map [f] over [xs] on up to 8 domains, results in input order.
    Domain-safe as long as [f] touches no shared mutable state. *)
val par_map8 : ('a -> 'b) -> 'a list -> 'b list

(** PATH probe over candidate command names in a single spawn: a
    [command -v] loop on Unix, one [where] call on Win32. Returns the
    subset of [names] found on PATH. [os] defaults to [Sys.os_type] so
    tests can exercise the Windows branch. *)
val which : ?os:string -> runner -> string list -> string list
