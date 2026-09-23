(** Small shared string predicates. *)

(** True when [sub] occurs in [s]. Empty [sub] matches. *)
val contains_substring : string -> string -> bool

(** True when [s] contains two consecutive spaces. *)
val contains_double_space : string -> bool
