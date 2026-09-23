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

val latest_release : Fetch.fetch -> string -> (release, string) result

(** Pick the best Windows installer asset: prefer arch-tagged Windows
    builds, fall back to any .exe/.msi. *)
val match_by_arch : asset list -> asset option
