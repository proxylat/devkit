(** On-demand update checks for non-winget package managers. *)

(** npm: one [outdated -g --json] spawn; entries are already outdated
    by npm's semver compare, so every [latest] is an update. *)
val npm_updates : ?os:string -> Proc.runner -> (string * string) list

(** PyPI latest per package (pipx and uv tools are PyPI names);
    [None]/unparseable/current yields no update for that package. *)
val pypi_updates : Fetch.fetch -> (string * string) list -> (string * string) list

(** crates.io max_version per package, same skip rules as PyPI. *)
val cargo_updates : Fetch.fetch -> (string * string) list -> (string * string) list

(** Pinned rows (any kind), plus self-pinned forge rows, against their
    vendor release tag; returns [(value, tag)] for rows the tag leaves
    behind. *)
val upstream_updates : Fetch.fetch -> Manifest.item list -> (string * string) list

(** Group Installed Pm items by manager and check each group; pinned
    and self-pinned rows check the vendor tag instead. Returns
    [(value, available)] sorted and deduplicated, plus yank warnings
    for locked tags that no longer exist upstream. *)
val check_all
  :  run:Proc.runner
  -> fetch:Fetch.fetch
  -> ?lock:Lockfile.t
  -> Manifest.item list
  -> (string * string) list * string list
