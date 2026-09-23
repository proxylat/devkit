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
