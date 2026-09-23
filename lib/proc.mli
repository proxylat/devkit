(** Process execution with an injectable runner. *)

(** [runner prog args] runs [prog] with [args], returning stdout trimmed on
    success, [None] when the program is missing or exits non-zero. *)
type runner = string -> string list -> string option

(** Real runner on top of [Unix.open_process_args_in]. *)
val default_runner : runner

(** Memoized [winget list --verbose] fetcher with resetter: one winget
    invocation per 8s window per resolved path. *)
val winget_list : runner -> (string -> string option) * (unit -> unit)
