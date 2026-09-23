(** Parsing of [winget list --verbose] table output.

    Rows split on runs of 2+ spaces, which handles both the aligned
    terminal output and the compact piped output devkit actually
    captures. *)

type info =
  { id : string (** Original-case id; case-sensitive for [winget install --id]. *)
  ; name : string
  ; version : string
  ; available : string
  }

module IdMap = Map.Make (String)

let is_blank = function
  | ' ' | '\t' | '\r' -> true
  | _ -> false
;;

(** Split a row on runs of 2+ blanks. Single blanks inside names
    (e.g. ["7-Zip 26.00 (x64)"]) survive. *)
let split_cols (line : string) : string list =
  let n = String.length line in
  let cols = ref [] in
  let cur = Buffer.create 32 in
  let flush () =
    let s = String.trim (Buffer.contents cur) in
    Buffer.clear cur;
    if s <> "" then cols := s :: !cols
  in
  let i = ref 0 in
  while !i < n do
    if is_blank line.[!i]
    then (
      let j = ref !i in
      while !j < n && is_blank line.[!j] do
        incr j
      done;
      if !j - !i >= 2 then flush () else Buffer.add_char cur ' ';
      i := !j)
    else (
      Buffer.add_char cur line.[!i];
      incr i)
  done;
  flush ();
  List.rev !cols
;;

(** Third bytes (after E2 94) of the accepted box-drawing code points:
    ─ │ ┬ ┴ ┼ ├ ┤ ┌ ┐ └ ┘. *)
let box_thirds = [ 0x80; 0x82; 0xAC; 0xB4; 0xBC; 0x9C; 0xA4; 0x8C; 0x90; 0x94; 0x98 ]

(** Separator lines: dashes, blanks or box-drawing characters. *)
let is_winget_separator (line : string) : bool =
  let n = String.length line in
  if n = 0
  then false
  else (
    let i = ref 0 in
    let ok = ref true in
    while !ok && !i < n do
      let c = line.[!i] in
      if c = '-' || is_blank c
      then incr i
      else if
        Char.code c = 0xE2
        && !i + 2 <= n - 1
        && Char.code line.[!i + 1] = 0x94
        && List.mem (Char.code line.[!i + 2]) box_thirds
      then i := !i + 3
      else ok := false
    done;
    !ok)
;;

(** Version-ish token: contains an ASCII digit. "Unknown" and source names
    ("winget"/"msstore") have none and are excluded. Go uses Unicode digits;
    winget emits ASCII versions. *)
let looks_like_version (s : string) : bool =
  let found = ref false in
  String.iter (fun c -> if c >= '0' && c <= '9' then found := true) s;
  !found
;;

(** Id column by pattern: dotted, has a letter, no spaces/backslash/braces.
    Go additionally requires no tabs; tabs cannot survive {!split_cols}. *)
let is_id_col (col : string) : bool =
  String.contains col '.'
  && (not (String.contains col '\\'))
  && (not (String.contains col '{'))
  && (not (String.contains col ' '))
  &&
  let has_letter = ref false in
  String.iter
    (fun c -> if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') then has_letter := true)
    col;
  !has_letter
;;

let is_header cols = List.exists (fun c -> String.lowercase_ascii c = "id") cols

(** Parse [winget list --verbose] output into a map keyed by lowercase id. *)
let parse_list_table (output : string) : info IdMap.t =
  let rows = String.split_on_char '\n' output in
  List.fold_left
    (fun acc raw ->
       let line = String.trim raw in
       if line = "" || is_winget_separator line
       then acc
       else (
         let cols = split_cols line in
         if List.length cols < 2 || is_header cols
         then acc
         else (
           let id_idx = List.find_index (fun c -> is_id_col c) cols in
           match id_idx with
           | None -> acc
           | Some 0 -> acc (* need at least a Name before the Id *)
           | Some k ->
             let id = List.nth cols k in
             let name = List.filteri (fun i _ -> i < k) cols |> String.concat " " in
             let rest = List.filteri (fun i _ -> i > k) cols in
             let version =
               match rest with
               | v :: _ -> v
               | [] -> ""
             in
             let available =
               match rest with
               | _ :: a :: _ when looks_like_version a && a <> version -> a
               | _ -> ""
             in
             IdMap.add (String.lowercase_ascii id) { id; name; version; available } acc)))
    IdMap.empty
    rows
;;
