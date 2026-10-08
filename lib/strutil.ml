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

(** [canon_repo] accepts a bare [owner/repo] or a full GitHub URL — repo
    page, deep link, or clone URL — and returns the canonical [owner/repo].
    Anything unrecognized is returned unchanged. *)
let canon_repo (s : string) : string =
  let s = String.trim s in
  let drop n str = String.sub str n (String.length str - n) in
  let chop p str =
    let low = String.lowercase_ascii str in
    let n = String.length p in
    if String.length str >= n && String.sub low 0 n = p then Some (drop n str) else None
  in
  let strip_git r =
    let low = String.lowercase_ascii r in
    let n = String.length r in
    if n > 4 && String.sub low (n - 4) 4 = ".git" then String.sub r 0 (n - 4) else r
  in
  let segs p = List.filter (fun x -> x <> "") (String.split_on_char '/' p) in
  let path =
    match chop "git@github.com:" s with
    | Some rest -> Some (`Url rest)
    | None ->
      let noscheme =
        match chop "https://" s with
        | Some _ as hit -> hit
        | None -> chop "http://" s
      in
      (match noscheme with
       | None -> Some (`Bare s)
       | Some rest ->
         (match chop "github.com/" rest with
          | Some _ as hit -> hit
          | None -> chop "www.github.com/" rest)
         |> Option.map (fun p -> `Url p))
  in
  match path with
  | None -> s
  | Some (`Bare b) ->
    (match segs b with
     | [ owner; repo ] -> owner ^ "/" ^ repo
     | _ -> s)
  | Some (`Url u) ->
    (match segs u with
     | owner :: repo :: _ -> owner ^ "/" ^ strip_git repo
     | _ -> s)
;;
