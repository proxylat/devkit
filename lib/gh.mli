(** GitHub releases API. *)

type asset =
  { name : string
  ; browser_download_url : string
  ; size : int64
  }

type release =
  { tag_name : string
  ; assets : asset list
  }

(** Latest release for [owner/repo]. *)
val parse_release : string -> (release, string) result

(** Canonicalize a repo reference: bare [owner/repo] or a full GitHub URL
    (page, deep link, clone URL) becomes [owner/repo]; anything else is
    returned unchanged. *)
val canon_repo : string -> string

val latest_release : Fetch.fetch -> string -> (release, string) result

(** Pick the best Windows installer asset: prefer arch-tagged Windows
    builds, fall back to any .exe/.msi. *)
val match_by_arch : asset list -> asset option
