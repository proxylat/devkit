(** Community winget-pkgs manifest lookup via the GitHub contents API. *)

(** [dir_url id] is the contents-API URL of the manifest dir for [id]. *)
val dir_url : string -> string

(** Version-dir names under [dir_url id]. *)
val list_versions : Fetch.fetch -> string -> (string list, string) result

(** Highest version in the list ([None] on [[]]). *)
val pick_max : string list -> string option

(** Sorted-uniq lowercase installer hosts scanned out of installer yaml. *)
val installer_hosts : string -> string list

(** Installer hosts for [id] at [version] (first sorted .installer.yaml). *)
val hosts_of : Fetch.fetch -> string -> string -> (string list, string) result
