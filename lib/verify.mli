(** Post-download verification for vendor-direct release assets.
    Tamper evidence blocks, missing evidence warns (see SecDoc.md). *)

type verdict =
  | Pass
  | Warn of string
  | Block of string

(** Authenticode status of [path] via PowerShell. Win32 only
    ([?os] defaults to [Sys.os_type]); other OSes pass silently.
    [Valid] passes; [HashMismatch]/[NotTrusted] block; anything else
    warns with the raw text. *)
val signature : ?os:string -> spawn:Bootstrap.spawn -> string -> verdict

(** Authenticode signer cert thumbprint of [path] via PowerShell.
    Win32 only ([?os] defaults to [Sys.os_type]); other OSes yield
    [None]. Blank output (unsigned) and spawn failures yield [None].
    Pure observation for continuity checks: never raises, never
    blocks. *)
val signer : ?os:string -> spawn:Bootstrap.spawn -> string -> string option

(** Sigstore attestation of [path] via [gh attestation verify --repo
    repo] (GitHub repos only; the caller filters providers).
    [?os] (default [Sys.os_type]) picks the gh probe: [where gh] on
    Win32, [command -v gh] elsewhere. A missing gh passes silently
    (opt-in by installation); a failed verify warns — naming [repo]
    when no attestation was published, quoting the raw output
    otherwise. NEVER blocks: offline/network failures must not brick
    installs. *)
val attestation : ?os:string -> spawn:Bootstrap.spawn -> repo:string -> string -> verdict

(** Release checksum-file names: [SHA256SUMS*], [*checksums*.txt],
    [*.sha256], case-insensitive. *)
val is_checksum_file : string -> bool

(** Parse [hex  filename] / [hex *filename] lines, skipping malformed
    ones. *)
val parse_sums : string -> (string * string) list

(** Compare [path] (downloaded as [asset]) against the release's
    checksum file, fetched via [fetch]. Only a hash mismatch blocks;
    a missing file, a fetch failure, or no line for our asset warns. *)
val checksum
  :  fetch:Fetch.fetch
  -> Gh.asset list
  -> asset:string
  -> path:string
  -> verdict

(** Delete a blocked download, ignoring errors. Removes the file only. *)
val discard : string -> unit
