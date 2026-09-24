(** Process execution with an injectable runner.

    All inventory scanners take a [runner] so tests feed canned output
    without spawning processes. There is no timeout on spawned commands. *)

(** [runner prog args] runs [prog] with [args], returning stdout trimmed on
    success, [None] when the program is missing or exits non-zero. *)
type runner = string -> string list -> string option

(** Real runner on top of [Unix.open_process_args_in]. *)
let default_runner : runner =
  fun prog args ->
  try
    let cmd = Array.of_list (prog :: args) in
    let ic = Unix.open_process_args_in prog cmd in
    let buf = Buffer.create 256 in
    (try
       while true do
         Buffer.add_channel buf ic 4096
       done
     with
     | End_of_file -> ());
    let out = Buffer.contents buf in
    match Unix.close_process_in ic with
    | Unix.WEXITED 0 -> Some (String.trim out)
    | _ -> None
  with
  | _ -> None
;;

(** Map [f] over [xs] on up to 8 domains, results in input order. One
    domain per item would drown short lists in thread overhead, so items
    are chunked; empty chunks are skipped. Domain-safe as long as [f]
    touches no shared mutable state (here: one process spawn per call). *)
let par_map8 (f : 'a -> 'b) (xs : 'a list) : 'b list =
  let arr = Array.of_list xs in
  let n = Array.length arr in
  if n = 0
  then []
  else (
    let workers = min 8 n in
    let chunk = (n + workers - 1) / workers in
    let jobs =
      List.filter_map
        (fun w ->
           let lo = w * chunk in
           if lo >= n
           then None
           else Some (Array.sub arr lo (min chunk (n - lo)) |> Array.to_list))
        (List.init workers Fun.id)
    in
    let doms = List.map (fun job -> Domain.spawn (fun () -> List.map f job)) jobs in
    List.concat_map Domain.join doms)
;;

(** PATH probe over candidate command names in a single spawn. Unix
    runs one [sh] loop over [command -v] (a shell builtin, so this is
    one fork); Win32 runs one [where] call, forcing exit 0 because
    [where] fails when any name is missing. [where] prints paths, so
    hits map back by lowercased basename without extension. *)
let quote_sh s = "'" ^ String.concat "'\\''" (String.split_on_char '\'' s) ^ "'"

let win_basename line =
  let base =
    match String.rindex_opt line '\\' with
    | Some i -> String.sub line (i + 1) (String.length line - i - 1)
    | None ->
      (match String.rindex_opt line '/' with
       | Some i -> String.sub line (i + 1) (String.length line - i - 1)
       | None -> line)
  in
  let stem =
    match String.rindex_opt base '.' with
    | Some i -> String.sub base 0 i
    | None -> base
  in
  String.lowercase_ascii (String.trim stem)
;;

let which ?(os = Sys.os_type) (run : runner) (names : string list) : string list =
  let names = List.sort_uniq String.compare (List.filter (fun s -> s <> "") names) in
  if names = []
  then []
  else if os = "Win32" || os = "Cygwin"
  then (
    let args = List.map (fun n -> "\"" ^ n ^ "\"") names in
    match run "cmd" [ "/c"; "where " ^ String.concat " " args ^ " 2>nul || exit 0" ] with
    | None -> []
    | Some out ->
      let hits =
        List.filter_map
          (fun line ->
             let stem = win_basename (String.trim line) in
             if List.mem stem names then Some stem else None)
          (String.split_on_char '\n' out)
      in
      List.sort_uniq String.compare hits)
  else (
    (* The loop exits nonzero when the last candidate misses; force
       success because hits arrive on stdout (default_runner drops
       non-zero output entirely). *)
    let script =
      "for c in "
      ^ String.concat " " (List.map quote_sh names)
      ^ "; do command -v \"$c\" >/dev/null 2>&1 && printf '%s\\n' \"$c\"; done; exit 0"
    in
    match run "sh" [ "-c"; script ] with
    | None -> []
    | Some out ->
      List.filter (fun line -> List.mem line names) (String.split_on_char '\n' out))
;;

(** Memoized [winget list --verbose] fetcher with resetter. The
    inventory scan and the update check share one [winget list]
    invocation per 8s window, so refresh ticks never pop extra winget
    windows. [resolve] is the winget executable located by the caller;
    empty falls back to PATH lookup. *)
let winget_list (run : runner) : (string -> string option) * (unit -> unit) =
  let cache : (string, string * float) Hashtbl.t = Hashtbl.create 4 in
  let ttl = 8.0 in
  let fetch resolved =
    let now = Unix.gettimeofday () in
    match Hashtbl.find_opt cache resolved with
    | Some (out, at) when now -. at < ttl -> Some out
    | _ ->
      let prog = if resolved = "" then "winget" else resolved in
      (match run prog [ "list"; "--accept-source-agreements"; "--verbose" ] with
       | None -> None
       | Some out ->
         Hashtbl.replace cache resolved (out, now);
         Some out)
  in
  fetch, fun () -> Hashtbl.clear cache
;;
