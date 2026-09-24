(** Interactive dashboard state machine.

    Pure navigation, selection and frame rendering over dashboard sections.
    The notty frontend in [bin/] draws the frame and feeds key actions in;
    tests drive [step] and [frame] without a terminal. Row selection
    mirrors [Dashboard.render]: empty sections and "Newly detected" are
    skipped. *)

open Manifest

type entry =
  { section : string
  ; item : item
  }

(** Flatten sections to selectable rows, skipping empties and Newly
    detected (same rule as the plain renderer). *)
let build_entries (sections : section list) : entry list =
  List.concat_map
    (fun (sec : section) ->
       if sec.items = [] || String.lowercase_ascii sec.name = "newly detected"
       then []
       else List.map (fun item -> { section = sec.name; item }) sec.items)
    sections
;;

type state =
  { entries : entry list
  ; cursor : int
  ; offset : int
  ; height : int
  ; width : int
  ; message : string option
  ; log : string list
  }

let make (sections : section list) ~(height : int) ~(width : int) : state =
  { entries = build_entries sections
  ; cursor = 0
  ; offset = 0
  ; height = max 1 height
  ; width = max 1 width
  ; message = None
  ; log = []
  }
;;

let max_cursor (s : state) : int = max 0 (List.length s.entries - 1)

(** Keep cursor in range and the viewport pinned to it. *)
let clamp (s : state) : state =
  let cursor = min (max 0 s.cursor) (max_cursor s) in
  let offset =
    if cursor < s.offset
    then cursor
    else if cursor >= s.offset + s.height
    then cursor - s.height + 1
    else s.offset
  in
  { s with cursor; offset }
;;

let resize (s : state) ~(height : int) ~(width : int) : state =
  clamp { s with height = max 1 height; width = max 1 width }
;;

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

let step (s : state) (a : action) : state =
  let cursor =
    match a with
    | Up -> s.cursor - 1
    | Down -> s.cursor + 1
    | Page_up -> s.cursor - s.height
    | Page_down -> s.cursor + s.height
    | Scroll_up -> s.cursor - 3
    | Scroll_down -> s.cursor + 3
    | Home -> 0
    | End -> max_cursor s
    | Quit -> s.cursor
  in
  clamp { s with cursor; message = None }
;;

let selected (s : state) : entry option = List.nth_opt s.entries s.cursor

(** What Enter does on the cursor row: installable statuses run the
    installer, Manual rows open the URL, anything else is a no-op. *)
type enter =
  | Do_install of item
  | Do_open of string
  | Do_nothing

let enter_action (s : state) : enter =
  match selected s with
  | None -> Do_nothing
  | Some e ->
    (match e.item.status with
     | NeedsUpdate | NotFound -> Do_install e.item
     | Manual -> Do_open e.item.value
     | Installed | New -> Do_nothing)
;;

(** Row color by install status; the frontend maps this to terminal colors. *)
type color =
  | Plain
  | Green
  | Yellow
  | Red
  | Cyan

let color_of : status -> color = function
  | Installed -> Green
  | NeedsUpdate -> Yellow
  | NotFound -> Red
  | New -> Cyan
  | Manual -> Plain
;;

let symbol : status -> string = function
  | Installed -> "ok"
  | NeedsUpdate -> "update"
  | NotFound -> "new"
  | New -> "?"
  | Manual -> "manual"
;;

let row_text (e : entry) : string =
  Printf.sprintf
    "%s  %s  %s"
    (symbol e.item.status)
    e.item.value
    (Dashboard.format_ver e.item)
;;

let visible (s : state) : entry list =
  let rec drop n l =
    if n <= 0
    then l
    else (
      match l with
      | [] -> []
      | _ :: t -> drop (n - 1) t)
  in
  let rec take n l =
    if n <= 0
    then []
    else (
      match l with
      | [] -> []
      | h :: t -> h :: take (n - 1) t)
  in
  take s.height (drop s.offset s.entries)
;;

(** Text frame: header, legend, visible rows (cursor marked), message,
    footer. The notty frontend draws one line per string. *)
let frame (s : state) : string list =
  let header = "devkit — enter installs/updates, q quits" in
  let legend = "ok installed · update pending · new missing · manual link" in
  let rows =
    List.mapi
      (fun i e ->
         let mark = if s.offset + i = s.cursor then "> " else "  " in
         mark ^ row_text e)
      (visible s)
  in
  let footer =
    match s.message with
    | Some m -> m
    | None ->
      Printf.sprintf
        "%d/%d"
        (min (s.cursor + 1) (List.length s.entries))
        (List.length s.entries)
  in
  (header :: legend :: rows) @ [ footer ]
;;

(** Record an install outcome: refresh the row status, append the log. *)
let apply_outcome (s : state) (value : string) (o : Install.outcome) : state =
  let status =
    match o.Install.status with
    | Install.Installed | Install.Updated -> Installed
    | Install.Opened -> Manual
    | Install.Skipped _ | Install.Failed _ ->
      (match selected s with
       | Some e when e.item.value = value -> e.item.status
       | _ -> NotFound)
  in
  let entries =
    List.map
      (fun e ->
         if e.item.value = value then { e with item = { e.item with status } } else e)
      s.entries
  in
  let line = Printf.sprintf "%s: %s" value (Install.status_to_string o.Install.status) in
  clamp { s with entries; message = Some line; log = s.log @ [ line ] }
;;
