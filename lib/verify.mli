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

(** Delete a blocked download and its temp dir, ignoring errors. *)
val discard : string -> unit
