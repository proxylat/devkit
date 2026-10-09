(** Dashboard: merging scan results with the manifest, plus plain-text render. *)

type app =
  { name : string
  ; version : string
  ; pm : string
  }

val status_symbol : Manifest.status -> string
val format_ver : Manifest.item -> string

(** Merge the machine scan with manifest sections into dashboard sections:
    "Pending updates", "Newly detected", then the manifest sections minus
    their updated and installed items, then a trailing "Installed" section
    so the table ends on green. [show] enriches NotFound winget items via
    [winget show]. [run] enables a single-spawn PATH probe: manifest
    items whose candidate command is on PATH (but missed by every PM
    scan) count as installed. [os] selects the probe shell (default:
    [Sys.os_type]) so tests stay platform-independent. [upstream_ver]
    maps an [(provider, upstream repo)] pair to its latest release tag:
    pinned rows, plus forge rows self-pinned to their own id, compare
    against the vendor tag instead of their PM's reading, and a failed
    lookup keeps the PM reading. Matching is candidate-based (full value,
    basename, URL stem), not exact-id. *)
val build_sections
  :  ?show:(string -> string option)
  -> ?upstream_ver:(Provider.t -> string -> string option)
  -> ?run:Proc.runner option
  -> ?os:string
  -> app list
  -> Manifest.section list
  -> Winget_parse.info Winget_parse.IdMap.t option
  -> Manifest.section list

(** Plain-text dashboard. Empty sections and "Newly detected" are skipped. *)
val render : Manifest.section list -> string
