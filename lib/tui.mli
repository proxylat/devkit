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
  | Scroll_up
  | Scroll_down
  | Home
  | End
  | Quit

val step : state -> action -> state
val selected : state -> entry option

(** Row color by status, for the frontend to map to terminal colors:
    green = installed, yellow = needs update, red = missing,
    cyan = newly detected, plain = manual links. *)
type color =
  | Plain
  | Green
  | Yellow
  | Red
  | Cyan

val color_of : status -> color

type enter =
  | Do_install of item
  | Do_open of string
  | Do_nothing

val enter_action : state -> enter
val row_text : entry -> string
val visible : state -> entry list

type line =
  | Head of string
  | Divider of string
  | Row of entry * bool

val lines : state -> line list
val render_line : line -> string
val frame : state -> string list
val apply_outcome : state -> string -> Install.outcome -> state
val apply_updates : state -> (string * string) list -> state
