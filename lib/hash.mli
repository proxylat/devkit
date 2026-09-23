(** SHA-256 file hashing and verification. *)

type expectation =
  | No_expected
  | Hex of string

(** SHA-256 hex digest of a file's contents. *)
val sha256_file : string -> (string, string) result

(** Compare [expected] against [path]'s digest. [No_expected] passes;
    [Hex ""] is rejected. *)
val verify_file : expectation -> string -> (unit, string) result

(** Download [url] into a fresh temp dir, verify, return the file path. *)
val download_and_verify : Fetch.fetch -> expectation -> string -> (string, string) result
