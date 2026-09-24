(** Interactive dashboard state machine (pure navigation + frame). *)

open Manifest

type entry =
  { section : string
  ; item : item
  }

val build_entries : section list -> entry list

type state =
  { entries : entry list
  ; cursor : int
  ; offset : int
  ; height : int
  ; width : int
  ; message : string option
  ; log : string list
  }

val make : section list -> height:int -> width:int -> state
val clamp : state -> state
val resize : state -> height:int -> width:int -> state

type action =
  | Up
  | Down
  | Page_up
  | Page_down
  | Home
  | End
  | Quit

val step : state -> action -> state
val selected : state -> entry option

type enter =
  | Do_install of item
  | Do_open of string
  | Do_nothing

val enter_action : state -> enter
val row_text : entry -> string
val visible : state -> entry list
val frame : state -> string list
val apply_outcome : state -> string -> Install.outcome -> state
