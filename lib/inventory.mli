(** Inventory scanners: installed-software discovery per package manager. *)

val parse_npm : string -> Dashboard.app list
val parse_pipx : string -> Dashboard.app list
val parse_uv : string -> Dashboard.app list
val parse_cargo : string -> Dashboard.app list
val parse_winget : string -> Dashboard.app list
val parse_reg : string -> Dashboard.app list

(** Windows uninstall sweep: machine + 32-bit view + current user. *)
val scan_reg : Proc.runner -> Dashboard.app list

(** Full scan order follows [sources] (default: {!Manifest.default_sources});
    [registry] only spawns on Windows. [winget] is the memoized
    [winget list] fetch. *)
val scan_all
  :  Proc.runner
  -> winget:(unit -> string option)
  -> extra:Plugin.tool list
  -> ?os:string
  -> ?sources:string list
  -> unit
  -> Dashboard.app list

(** The five built-ins as plugin entries (scan order). *)
val built_ins : Plugin.tool list

(** Names custom tools may not take. *)
val reserved_names : string list

(** Run one plugin tool; missing binary means skipped. *)
val scan_tool : Proc.runner -> Plugin.tool -> Dashboard.app list

(** [DEVKIT_TIMING=1] phase timer shared by the scan and the TUI's
    progressive loop: one [[timing] <label>: Nms] line on stderr. *)
val time_src : string -> (unit -> 'a) -> 'a
