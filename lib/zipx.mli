(** Minimal ZIP reader: stored + deflated entries, central-directory driven. *)

type entry =
  { name : string
  ; meth : int
  ; comp_size : int
  ; uncomp_size : int
  ; local_off : int
  }

(** Parse the central directory into entries. *)
val list_entries : bytes -> (entry list, string) result

(** Entry payload from its local header. *)
val extract_entry : bytes -> entry -> (bytes, string) result

(** [safe_path ~dest name] resolves [name] inside [dest], returning [None]
    for anything that escapes. Trailing separators mark directories. *)
val safe_path : dest:string -> string -> (string * bool) option

val mkdir_p : string -> unit

(** Extract [zip_path] into [dest_dir]. Fails fast on the first unsafe entry. *)
val extract : zip_path:string -> dest_dir:string -> (unit, string) result
