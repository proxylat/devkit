(** Manifest (devkit.toml) parsing and rendering. *)

(** The manifest file name, resolved from the working directory. *)
val filename : string

type item_type =
  | Winget
  | GitHub
  | GitLab of string (** forge host, always explicit, e.g. ["gitlab.com"] *)
  | Forgejo of string (** forge host, always explicit, e.g. ["codeberg.org"] *)
  | Url
  | Pm of string
  | Registry (** Windows uninstall entry: discovery-only, never persisted *)

type status =
  | Installed
  | NeedsUpdate
  | NotFound
  | New
  | Manual

type item =
  { typ : item_type
  ; value : string
  ; installed_version : string
  ; available_version : string
  ; status : status
  ; upstream : string
    (** vendor pin: [owner/repo], a forge URL, or a [gitlab:]/[forgejo:]-prefixed id; empty disables *)
  ; quarantine_days : int (** resolved N-day new-release hold; 0 = off *)
  }

type section =
  { name : string
  ; items : item list
  }

type parse_result =
  { sections : section list
  ; winget_path : string
  ; warnings : string list (** skipped rows, in document order, naming the fix *)
  ; sources : string list (** scan sources in order; {!default_sources} when unset *)
  ; quarantine : int (** global N-day quarantine; 0 = off *)
  }

(** Scan sources when the manifest sets none: classic order, registry last. *)
val default_sources : string list

val make_item : item_type -> string -> item
val type_string : item_type -> string
val item_type_of_string : string -> item_type

(** The vendor repo a row answers to: the explicit [upstream] pin when
    set, else the row's own id for forge kinds (GitHub, GitLab,
    Forgejo) — repo rows are self-pinned. [None] for winget, link,
    manager, and registry rows without a pin. *)
val effective_upstream : item -> (Provider.t * string) option

(** Parse a manifest. The id carries its own source: github.com URLs
    canonicalize to [owner/repo], gitlab.com and codeberg.org URLs
    infer their forge, other URLs are plain links, [prefix:name] is
    explicit, [@scope/pkg] is npm, [owner/repo] is GitHub, dotted ids
    are winget, bare words are rejected — as is any leftover [pm] key.
    Self-hosted forges need [gitlab:]/[forgejo:] with the full URL.
    A top-level [quarantine_days] sets the global N-day hold (negatives
    clamp to 0, non-ints are ignored); each package resolves to its own
    [quarantine_days] override when present, else the global.
    Rejected rows are skipped with a [warnings] entry; malformed TOML
    yields an empty result. *)
val parse : string -> parse_result

(** Render back to TOML: one [id] line per package (manager kinds keep
    their [prefix:], self-identifying shapes stay bare). Nonzero global
    and per-row [quarantine_days] are emitted. *)
val to_string : parse_result -> string
