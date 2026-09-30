(** Tests for {!Proc.which}: single-spawn PATH probe. *)

open Devkit

let unix_subset () =
  let calls = ref 0 in
  let script = ref "" in
  let run prog args =
    incr calls;
    match prog, args with
    | "sh", [ "-c"; s ] ->
      script := s;
      Some "git\nnode\n"
    | _ -> None
  in
  let hits = Proc.which ~os:"Unix" run [ "git"; "node"; "missing"; ""; "git" ] in
  Alcotest.(check (list string)) "echo order" [ "git"; "node" ] hits;
  Alcotest.(check int) "single spawn" 1 !calls;
  (* default_runner drops non-zero output, so the loop must force exit 0. *)
  Alcotest.(check bool)
    "exit forced zero"
    true
    (let s = !script in
     String.length s >= 6 && String.sub s (String.length s - 6) 6 = "exit 0")
;;

let unix_none () =
  let run _ _ = None in
  Alcotest.(check (list string)) "runner miss" [] (Proc.which ~os:"Unix" run [ "git" ])
;;

let unix_empty () =
  let run _ _ = Alcotest.fail "must not spawn" in
  Alcotest.(check (list string)) "no names" [] (Proc.which ~os:"Unix" run [])
;;

let win_basenames () =
  let calls = ref 0 in
  let script = ref "" in
  let run prog args =
    incr calls;
    match prog, args with
    | "cmd", [ "/c"; s ] ->
      script := s;
      Some "C:\\Program Files\\Git\\bin\\git.exe\r\nC:\\Tools\\ripgrep\\rg.exe\r\n"
    | _ -> None
  in
  let hits = Proc.which ~os:"Win32" run [ "git"; "rg"; "missing" ] in
  Alcotest.(check (list string)) "stem match" [ "git"; "rg" ] hits;
  Alcotest.(check int) "single spawn" 1 !calls;
  Alcotest.(check bool)
    "exit forced zero"
    true
    (let s = !script in
     String.length s >= 9 && String.sub s (String.length s - 9) 9 = "|| exit 0")
;;

(** Child stderr never reaches our terminal: it is drained and dropped
    while stdout is returned. The test's own stderr is parked in a temp
    file around the spawn to prove nothing leaks. *)
let stderr_dropped () =
  let prog, args =
    if Sys.os_type = "Win32" || Sys.os_type = "Cygwin"
    then "cmd", [ "/c"; "echo BOOM 1>&2 & echo hi" ]
    else "sh", [ "-c"; "echo BOOM >&2; echo hi" ]
  in
  let tmp = Filename.temp_file "devkit-err-" ".log" in
  let fd = Unix.openfile tmp [ Unix.O_WRONLY ] 0 in
  let saved = Unix.dup Unix.stderr in
  Unix.dup2 fd Unix.stderr;
  let r =
    match Proc.default_runner prog args with
    | v ->
      Unix.dup2 saved Unix.stderr;
      v
    | exception e ->
      Unix.dup2 saved Unix.stderr;
      raise e
  in
  Unix.close saved;
  Unix.close fd;
  Alcotest.(check (option string)) "stdout kept" (Some "hi") r;
  let ic = open_in_bin tmp in
  let n = in_channel_length ic in
  let leaked = really_input_string ic n in
  close_in ic;
  Sys.remove tmp;
  Alcotest.(check string) "stderr clean" "" leaked
;;

let () =
  Alcotest.run
    "proc"
    [ ( "which"
      , [ Alcotest.test_case "unix subset" `Quick unix_subset
        ; Alcotest.test_case "unix miss" `Quick unix_none
        ; Alcotest.test_case "unix empty" `Quick unix_empty
        ; Alcotest.test_case "win basenames" `Quick win_basenames
        ] )
    ; "runner", [ Alcotest.test_case "stderr dropped" `Quick stderr_dropped ]
    ]
;;
