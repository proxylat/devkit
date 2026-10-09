(** Package installation across winget, forge releases (GitHub, GitLab,
    Forgejo-family) and plain URLs. *)

type status =
  | Installed
  | Updated
  | Opened
  | Skipped of string
  | Failed of string

val status_to_string : status -> string

type outcome =
  { value : string
  ; status : status
  ; warnings : string list
  }

(** Injected surface. {!real_deps} wires production backends. *)
type deps =
  { winget : unit -> string
  ; spawn : Bootstrap.spawn
  ; open_browser : string -> (unit, string) result
  ; latest_release : Provider.t -> string -> (Gh.release, string) result
  ; download : url:string -> (string, string) result
  ; fetch : Fetch.fetch
  ; run_installer : string -> (unit, string) result
  ; lock_load : unit -> Lockfile.t
  ; lock_save : Lockfile.t -> unit
  ; tools : Plugin.tool list
  }

(** The GitHub repo page for [repo] ([owner/name]). Used both as the
    Enter target for installed link-like GitHub rows and as the fallback
    when the release API is unreachable. *)
val repo_page : string -> string

(** Run a downloaded installer: [.msi] via [msiexec], [.exe] with silent
    switches, anything else refused. *)
val run_installer : Bootstrap.spawn -> string -> (unit, string) result

val real_open_browser : ?os:string -> Bootstrap.spawn -> string -> (unit, string) result

(** Dispatch an install/update for one manifest entry. A non-empty
    [upstream] diverts winget, github, gitlab, forgejo, and plugin rows
    to the vendor's release (the provider is parsed out of the pin).
    [host] carries the self-hosted forge host for gitlab/forgejo rows.
    [os] selects the platform for the Authenticode check (Win32 only).
    Vendor-direct downloads are signature- and checksum-verified per
    SecDoc; soft findings ride [warnings], blocks are [Failed].
    [quarantine_days] refuses releases younger than that many days.
    Successful vendor installs record tag, digest, and signer via
    [lock_save]; a re-cut tag fails the next install. *)
val install
  :  deps
  -> ?upstream:string
  -> ?host:string
  -> ?os:string
  -> ?quarantine_days:int
  -> string
  -> string
  -> bool
  -> outcome

val real_deps
  :  Fetch.fetch
  -> winget_override:string
  -> read_file:(string -> (string, string) result)
  -> write_file:(string -> string -> (unit, string) result)
  -> unit
  -> deps
