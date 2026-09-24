(** Dashboard: merging scan results with the manifest, plus plain-text render. *)

type app =
  { name : string
  ; version : string
  ; pm : string
  }

val format_ver : Manifest.item -> string

(** Merge the machine scan with manifest sections into dashboard sections:
    "Pending updates", "Newly detected", then the manifest sections minus
    their updated items. [show] enriches NotFound winget items via
    [winget show]. *)
val build_sections
  :  ?show:(string -> string option)
  -> app list
  -> Manifest.section list
  -> Winget_parse.info Winget_parse.IdMap.t option
  -> Manifest.section list

(** Plain-text dashboard. Empty sections and "Newly detected" are skipped. *)
val render : Manifest.section list -> string
