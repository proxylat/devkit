(** Small shared string predicates. *)

(** True when [sub] occurs in [s]. Empty [sub] matches. *)
val contains_substring : string -> string -> bool

(** True when [s] contains two consecutive spaces. *)
val contains_double_space : string -> bool

(** True when [latest] is a strictly newer version than [installed]:
    a leading [v] is stripped, numeric segments compare as integers
    (missing segments count as 0), a release outranks a prerelease. *)
val is_newer_version : string -> string -> bool

(** [canon_repo] accepts a bare [owner/repo] or a full GitHub URL — repo
    page, deep link, or clone URL — and returns the canonical [owner/repo].
    Anything unrecognized is returned unchanged. *)
val canon_repo : string -> string

(** [parse_iso_date "2026-10-05T12:34:56Z"] = [Some (2026,10,5)]. Reads
    the YYYY-MM-DD prefix; [None] unless month 1-12 and day 1-31. *)
val parse_iso_date : string -> (int * int * int) option

(** [days_between earlier later] = later minus earlier in days (Howard
    Hinnant days_from_civil). May be negative. *)
val days_between : int * int * int -> int * int * int -> int
