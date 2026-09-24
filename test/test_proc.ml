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

let () =
  Alcotest.run
    "proc"
    [ ( "which"
      , [ Alcotest.test_case "unix subset" `Quick unix_subset
        ; Alcotest.test_case "unix miss" `Quick unix_none
        ; Alcotest.test_case "unix empty" `Quick unix_empty
        ; Alcotest.test_case "win basenames" `Quick win_basenames
        ] )
    ]
;;
