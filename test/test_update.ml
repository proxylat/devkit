(** Tests for {!Update}: backend parsing/routing, plus {!Tui.apply_updates}. *)

open Devkit
open Manifest

let npm_json =
  {|{"typescript":{"current":"5.3.3","wanted":"5.3.3","latest":"5.9.2","dependent":"global","location":"global"}}|}
;;

let npm_parses_outdated () =
  let run _ _ = Some npm_json in
  Alcotest.(check (list (pair string string)))
    "one update"
    [ "typescript", "5.9.2" ]
    (Update.npm_updates run)
;;

let npm_shell_by_os () =
  let calls = ref [] in
  let run prog args =
    calls := (prog, args) :: !calls;
    Some "{}"
  in
  ignore (Update.npm_updates ~os:"Win32" run);
  Alcotest.(check (pair string (list string)))
    "cmd with exit guard"
    ("cmd", [ "/c"; "npm outdated -g --json || exit 0" ])
    (List.hd !calls);
  calls := [];
  ignore (Update.npm_updates ~os:"Linux" run);
  Alcotest.(check (pair string (list string)))
    "sh with true guard"
    ("sh", [ "-c"; "npm outdated -g --json || true" ])
    (List.hd !calls)
;;

let npm_failures_empty () =
  Alcotest.(check (list (pair string string)))
    "missing npm"
    []
    (Update.npm_updates (fun _ _ -> None));
  Alcotest.(check (list (pair string string)))
    "bad json"
    []
    (Update.npm_updates (fun _ _ -> Some "not json"))
;;

let pypi_json v = Printf.sprintf {|{"info":{"version":%S}}|} v

let pypi_newer () =
  let fetch ?timeout_s url _ =
    Alcotest.(check (option int)) "15s timeout" (Some 15) timeout_s;
    Ok (pypi_json "24.0")
  in
  Alcotest.(check (list (pair string string)))
    "update"
    [ "black", "24.0" ]
    (Update.pypi_updates fetch [ "black", "23.1.0" ])
;;

let pypi_current_and_missing () =
  let fetch ?timeout_s:_ url _ =
    if url = "https://pypi.org/pypi/black/json"
    then Ok (pypi_json "23.1.0")
    else Error "404"
  in
  Alcotest.(check (list (pair string string)))
    "none"
    []
    (Update.pypi_updates fetch [ "black", "23.1.0"; "ghost", "1.0"; "noversion", "" ])
;;

let cargo_newer_and_ua () =
  let calls = ref [] in
  let fetch ?timeout_s:_ url headers =
    calls := (url, headers) :: !calls;
    Ok {|{"crate":{"max_version":"1.75.0"}}|}
  in
  Alcotest.(check (list (pair string string)))
    "update"
    [ "ripgrep", "1.75.0" ]
    (Update.cargo_updates fetch [ "ripgrep", "1.70.0" ]);
  Alcotest.(check bool)
    "user-agent sent"
    true
    (List.exists (fun (h, _) -> h = "User-Agent") (snd (List.hd !calls)))
;;

let inst pm value version =
  { (make_item (Pm pm) value) with installed_version = version; status = Installed }
;;

let check_all_routes () =
  let run_calls = ref 0 in
  let run _ _ =
    incr run_calls;
    Some "{}"
  in
  let fetch_calls = ref [] in
  let fetch ?timeout_s:_ url headers =
    fetch_calls := (url, headers) :: !fetch_calls;
    if String.length url >= 19 && String.sub url 8 11 = "crates.io/a"
    then Ok {|{"crate":{"max_version":"9.9"}}|}
    else Ok (pypi_json "9.9")
  in
  let items =
    [ inst "npm" "typescript" "5.3.3"
    ; inst "pipx" "black" "1.0"
    ; inst "uv" "ruff" "1.0"
    ; inst "cargo" "ripgrep" "1.0"
    ; { (make_item Winget "Git.Git") with status = Installed }
    ; { (make_item (Pm "npm") "missing") with status = NotFound }
    ; { (make_item GitHub "o/t") with status = Manual }
    ]
  in
  let got = Update.check_all ~run ~fetch items in
  Alcotest.(check int) "npm once" 1 !run_calls;
  Alcotest.(check int) "three registry hits" 3 (List.length !fetch_calls);
  Alcotest.(check (list (pair string string)))
    "updates"
    [ "black", "9.9"; "ripgrep", "9.9"; "ruff", "9.9" ]
    got
;;

let apply_updates_marks () =
  let st =
    Tui.make
      [ { name = "Setup"; items = [ inst "npm" "typescript" "5.3.3" ] } ]
      ~height:10
      ~width:80
  in
  let st = Tui.apply_updates st [ "typescript", "5.9.2" ] in
  let e = List.hd st.Tui.entries in
  (match e.Tui.item.status with
   | NeedsUpdate -> ()
   | _ -> Alcotest.fail "should need update");
  Alcotest.(check string) "available set" "5.9.2" e.Tui.item.available_version;
  Alcotest.(check (option string))
    "message"
    (Some "1 update available: typescript")
    st.Tui.message
;;

let apply_updates_empty () =
  let st =
    Tui.make
      [ { name = "Setup"; items = [ inst "npm" "typescript" "5.3.3" ] } ]
      ~height:10
      ~width:80
  in
  let st = Tui.apply_updates st [] in
  (match (List.hd st.Tui.entries).Tui.item.status with
   | Installed -> ()
   | _ -> Alcotest.fail "should stay installed");
  Alcotest.(check (option string)) "message" (Some "everything up to date") st.Tui.message
;;

let () =
  Alcotest.run
    "update"
    [ ( "npm"
      , [ Alcotest.test_case "parses outdated" `Quick npm_parses_outdated
        ; Alcotest.test_case "shell by os" `Quick npm_shell_by_os
        ; Alcotest.test_case "failures empty" `Quick npm_failures_empty
        ] )
    ; ( "registry"
      , [ Alcotest.test_case "pypi newer" `Quick pypi_newer
        ; Alcotest.test_case "pypi current/missing" `Quick pypi_current_and_missing
        ; Alcotest.test_case "cargo newer + UA" `Quick cargo_newer_and_ua
        ; Alcotest.test_case "check_all routes" `Quick check_all_routes
        ] )
    ; ( "tui"
      , [ Alcotest.test_case "apply marks" `Quick apply_updates_marks
        ; Alcotest.test_case "apply empty" `Quick apply_updates_empty
        ] )
    ]
;;
