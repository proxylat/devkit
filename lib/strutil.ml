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
