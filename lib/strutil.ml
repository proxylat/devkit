(** Small shared string predicates. *)

(** True when [sub] occurs in [s]. Empty [sub] matches. *)
let contains_substring (sub : string) (s : string) : bool =
  let m = String.length sub
  and n = String.length s in
  if m = 0
  then true
  else if m > n
  then false
  else (
    let found = ref false in
    for i = 0 to n - m do
      if String.sub s i m = sub then found := true
    done;
    !found)
;;

(** True when [s] contains two consecutive spaces. *)
let contains_double_space (s : string) : bool =
  let n = String.length s in
  let found = ref false in
  for i = 0 to n - 2 do
    if s.[i] = ' ' && s.[i + 1] = ' ' then found := true
  done;
  !found
;;

(** Segment-wise version order: a leading [v] is stripped, dot-separated
    segments compare numerically when both sides parse as integers
    (missing segments count as 0, so "2.48" equals "2.48.0"), and a
    numeric segment outranks a non-numeric one ("1.0" beats "1.0-beta").
    Anything else falls back to string order. *)
let is_newer_version (installed : string) (latest : string) : bool =
  let norm s =
    let s = String.trim s in
    let s =
      if String.length s > 0 && (s.[0] = 'v' || s.[0] = 'V')
      then String.sub s 1 (String.length s - 1)
      else s
    in
    String.split_on_char '.' s
  in
  let value seg =
    match int_of_string_opt (String.trim seg) with
    | Some n -> `Num n
    | None -> `Str seg
  in
  let rec go a b =
    match a, b with
    | [], [] -> false
    | [], _ :: _ -> List.exists (fun s -> value s <> `Num 0) b
    | _ :: _, [] ->
      (* Installed runs longer: newer only if installed's tail is all
         zeros, which the segment loop below already ruled out — so no. *)
      false
    | x :: xs, y :: ys ->
      (match value x, value y with
       | `Num m, `Num n -> if m = n then go xs ys else n > m
       | `Num _, `Str _ -> false
       | `Str _, `Num _ -> true
       | `Str m, `Str n -> if m = n then go xs ys else String.compare y x > 0)
  in
  go (norm installed) (norm latest)
;;
