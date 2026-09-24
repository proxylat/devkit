(** On-demand update checks for non-winget package managers. *)

(** npm: one [outdated -g --json] spawn; entries are already outdated
    by npm's semver compare, so every [latest] is an update. *)
val npm_updates : ?os:string -> Proc.runner -> (string * string) list

(** PyPI latest per package (pipx and uv tools are PyPI names);
    [None]/unparseable/current yields no update for that package. *)
val pypi_updates : Fetch.fetch -> (string * string) list -> (string * string) list

(** crates.io max_version per package, same skip rules as PyPI. *)
val cargo_updates : Fetch.fetch -> (string * string) list -> (string * string) list

(** Group Installed Pm items by manager and check each group; returns
    [(value, available)] sorted and deduplicated. *)
val check_all
  :  run:Proc.runner
  -> fetch:Fetch.fetch
  -> Manifest.item list
  -> (string * string) list
