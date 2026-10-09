(** Tag-immutability lockfile (devkit.lock) parsing and rendering. *)

(** The lockfile name, resolved from the working directory. *)
val filename : string

type entry =
  { id : string
  ; tag : string
  ; sha256 : string
  ; thumbprint : string (** "" means unset *)
  ; host : string (** "" means unset *)
  }

(** A lockfile: one entry per vendor-direct install. *)
type t = entry list

(** Parse a lockfile; malformed TOML yields [], missing keys default to "". *)
val parse : string -> entry list

(** Render back to TOML: one [[entry]] block per entry, [thumbprint]/[host] omitted when "". *)
val to_string : entry list -> string

(** Find an entry by id. *)
val find : entry list -> string -> entry option

(** Replace the entry with the same id, else append; order-stable. *)
val upsert : entry list -> entry -> entry list
