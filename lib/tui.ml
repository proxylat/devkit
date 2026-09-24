(** Interactive dashboard state machine.

    Pure navigation, selection and frame rendering over dashboard sections.
    The lambda-term frontend in [bin/] draws the frame and feeds key actions in;
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
    | Scroll_up -> s.cursor - 1
    | Scroll_down -> s.cursor + 1
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

(** Same bracket symbol as the plain renderer, so the TUI and the
    exit printout show identical rows. *)
let row_text (e : entry) : string =
  Printf.sprintf
    "[%s] %s%s"
    (Dashboard.status_symbol e.item.status)
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

(** Structured frame lines: headers and dividers are plain text,
    rows carry their entry plus a cursor flag for the frontend. *)
type line =
  | Head of string
  | Divider of string
  | Row of entry * bool

let lines (s : state) : line list =
  let at i = if i < 0 then None else List.nth_opt s.entries i in
  let prev0 =
    match at (s.offset - 1) with
    | None -> None
    | Some e -> Some e.section
  in
  (* Entry units from the offset, each tagged with its index. *)
  let rec units i prev =
    match at i with
    | None -> []
    | Some e ->
      let div = if Some e.section <> prev then Some (Divider e.section) else None in
      (i, div, e) :: units (i + 1) (Some e.section)
  in
  (* Flatten to tagged lines: divider (if any) then the row. *)
  let all =
    List.concat_map
      (fun (i, div, e) ->
         (match div with
          | Some d -> [ i, d ]
          | None -> [])
         @ [ i, Row (e, i = s.cursor) ])
      (units s.offset prev0)
  in
  let rec take k = function
    | [] -> []
    | (i, l) :: t -> if k <= 0 then [] else (i, l) :: take (k - 1) t
  in
  let win = take s.height all in
  let win =
    if List.exists (fun (i, _) -> i = s.cursor) win
    then win
    else (
      (* Dividers pushed the cursor row past the budget: end the window
         at the cursor and fill upward so the screen stays full. *)
      let rec upto acc = function
        | [] -> List.rev acc
        | (i, l) :: t -> if i > s.cursor then List.rev acc else upto ((i, l) :: acc) t
      in
      let before = upto [] all in
      List.rev (take s.height (List.rev before)))
  in
  (* Restore the section context when the drop cut the divider. *)
  let win =
    match win with
    | (i, Row (e, _)) :: _ when i > 0 ->
      let prev_sec =
        match at (i - 1) with
        | None -> None
        | Some p -> Some p.section
      in
      if prev_sec <> Some e.section && List.length win < s.height
      then (i, Divider e.section) :: win
      else win
    | _ -> win
  in
  let body = List.map snd win in
  let footer =
    match s.message with
    | Some m -> m
    | None ->
      Printf.sprintf
        "%d/%d"
        (min (s.cursor + 1) (List.length s.entries))
        (List.length s.entries)
  in
  [ Head "devkit — enter installs/updates, u checks updates, q quits"
  ; Head "[✓] installed · [!] update · [+] missing · [~] manual"
  ]
  @ body
  @ [ Head footer ]
;;

let render_line : line -> string = function
  | Head s -> s
  | Divider name -> String.uppercase_ascii name
  | Row (e, cursor) -> (if cursor then "> " else "  ") ^ row_text e
;;

(** Text frame: one string per line. The lambda-term frontend draws these
    with per-row colors from {!lines}. *)
let frame (s : state) : string list = List.map render_line (lines s)

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

(** Mark rows with available updates: set available_version +
    [NeedsUpdate] by value, with a summary message + log. An empty
    result keeps every row and reports everything current. *)
let apply_updates (s : state) (updates : (string * string) list) : state =
  let entries =
    List.map
      (fun e ->
         match List.assoc_opt e.item.value updates with
         | Some avail ->
           { e with
             item = { e.item with status = NeedsUpdate; available_version = avail }
           }
         | None -> e)
      s.entries
  in
  let message =
    match updates with
    | [] -> "everything up to date"
    | [ (v, _) ] -> Printf.sprintf "1 update available: %s" v
    | _ -> Printf.sprintf "%d updates available" (List.length updates)
  in
  clamp { s with entries; message = Some message; log = s.log @ [ message ] }
;;
