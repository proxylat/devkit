(** Parsing of [winget list --verbose] table output. *)

type info =
  { id : string (** Original-case id; case-sensitive for [winget install --id]. *)
  ; name : string
  ; version : string
  ; available : string
  }

module IdMap : Map.S with type key = string

(** Split a row on runs of 2+ blanks. Single blanks inside names survive. *)
val split_cols : string -> string list

(** Separator lines: dashes, blanks or box-drawing characters. *)
val is_winget_separator : string -> bool

(** Version-ish token: contains an ASCII digit. *)
val looks_like_version : string -> bool

(** Parse [winget list --verbose] output into a map keyed by lowercase id. *)
val parse_list_table : string -> info IdMap.t
