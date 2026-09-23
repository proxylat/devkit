(** Inventory scanners: installed-software discovery per package manager. *)

val parse_npm : string -> Dashboard.app list
val parse_pipx : string -> Dashboard.app list
val parse_uv : string -> Dashboard.app list
val parse_cargo : string -> Dashboard.app list
val parse_winget : string -> Dashboard.app list

(** Full scan order: winget, npm, pipx, uv, cargo, then [extra] custom
    tools in file order. [winget] is the memoized [winget list] fetch. *)
val scan_all
  :  Proc.runner
  -> winget:(unit -> string option)
  -> extra:Plugin.tool list
  -> Dashboard.app list

(** The five built-ins as plugin entries (scan order). *)
val built_ins : Plugin.tool list

(** Names custom tools may not take. *)
val reserved_names : string list

(** Run one plugin tool; missing binary means skipped. *)
val scan_tool : Proc.runner -> Plugin.tool -> Dashboard.app list
