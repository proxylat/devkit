(** Small shared string predicates. *)

(** True when [sub] occurs in [s]. Empty [sub] matches. *)
val contains_substring : string -> string -> bool

(** True when [s] contains two consecutive spaces. *)
val contains_double_space : string -> bool

(** True when [latest] is a strictly newer version than [installed]:
    a leading [v] is stripped, numeric segments compare as integers
    (missing segments count as 0), a release outranks a prerelease. *)
val is_newer_version : string -> string -> bool
