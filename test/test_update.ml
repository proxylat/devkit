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
  let got, warns = Update.check_all ~run ~fetch items in
  Alcotest.(check int) "npm once" 1 !run_calls;
  Alcotest.(check int) "three registry hits" 3 (List.length !fetch_calls);
  Alcotest.(check (list string)) "no yanks" [] warns;
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

let pin value upstream version status =
  { (make_item (Pm "npm") value) with upstream; installed_version = version; status }
;;

let upstream_newer_current_fail () =
  let fetch ?timeout_s:_ url _ =
    if Strutil.contains_substring "repos/new/tool" url
    then Ok {|{"tag_name": "v2.0", "assets": []}|}
    else if Strutil.contains_substring "repos/same/tool" url
    then Ok {|{"tag_name": "v1.0", "assets": []}|}
    else Error "404"
  in
  let items =
    [ pin "a" "new/tool" "1.0" Installed
    ; pin "b" "same/tool" "1.0" Installed
    ; pin "c" "gone/tool" "1.0" Installed
    ; pin "d" "new/tool" "" Installed
    ; pin "e" "new/tool" "1.0" Manual
    ; pin "f" "" "1.0" Installed
    ]
  in
  Alcotest.(check (list (pair string string)))
    "only the behind row"
    [ "a", "v2.0" ]
    (Update.upstream_updates fetch items)
;;

let upstream_forge_urls () =
  (* Forge pins route to their own APIs: the GitLab list endpoint with
     a %2F-encoded path, the Forgejo latest endpoint. *)
  let urls = ref [] in
  let fetch ?timeout_s:_ url _ =
    urls := url :: !urls;
    if Strutil.contains_substring "gitlab.com" url
    then Ok {|[{"tag_name": "v2.0", "assets": {"links": []}}]|}
    else Ok {|{"tag_name": "v3.0", "assets": []}|}
  in
  let items =
    [ pin "g" "https://gitlab.com/group/proj" "1.0" Installed
    ; pin "f" "forgejo:https://git.example.com/owner/repo" "1.0" Installed
    ]
  in
  Alcotest.(check (list (pair string string)))
    "both behind"
    [ "g", "v2.0"; "f", "v3.0" ]
    (Update.upstream_updates fetch items);
  Alcotest.(check bool)
    "gitlab api hit"
    true
    (List.exists
       (fun u ->
          Strutil.contains_substring "gitlab.com/api/v4/projects/group%2Fproj/releases" u)
       !urls);
  Alcotest.(check bool)
    "forgejo api hit"
    true
    (List.exists
       (fun u ->
          Strutil.contains_substring
            "git.example.com/api/v1/repos/owner/repo/releases/latest"
            u)
       !urls)
;;

let check_all_pins_skip_registry () =
  (* A pinned npm row hits the GitHub API, never npm outdated. *)
  let run _ _ = Alcotest.fail "registry must not run for pinned rows" in
  let urls = ref [] in
  let fetch ?timeout_s:_ url _ =
    urls := url :: !urls;
    Ok {|{"tag_name": "v9.9", "assets": []}|}
  in
  let items =
    [ { (inst "npm" "typescript" "5.3.3") with upstream = "microsoft/TypeScript" } ]
  in
  let got, warns = Update.check_all ~run ~fetch items in
  Alcotest.(check (list string)) "no yanks" [] warns;
  Alcotest.(check (list (pair string string))) "tag update" [ "typescript", "v9.9" ] got;
  Alcotest.(check bool)
    "github api hit"
    true
    (List.exists
       (fun u -> Strutil.contains_substring "api.github.com/repos/microsoft/TypeScript" u)
       !urls)
;;

let yank_warns_on_retagged () =
  (* Locked tag v1.0, upstream serves v2.0: the row updates AND warns. *)
  let run _ _ = Alcotest.fail "registry must not run for pinned rows" in
  let fetch ?timeout_s:_ _ _ = Ok {|{"tag_name": "v2.0", "assets": []}|} in
  let lock =
    Lockfile.[ { id = "a"; tag = "v1.0"; sha256 = ""; thumbprint = ""; host = "" } ]
  in
  let got, warns =
    Update.check_all ~run ~fetch ~lock [ pin "a" "owner/tool" "1.0" Installed ]
  in
  Alcotest.(check (list (pair string string))) "update listed" [ "a", "v2.0" ] got;
  Alcotest.(check (list string))
    "yank warned"
    [ "release yanked: a tag v1.0 no longer on github.com" ]
    warns
;;

let yank_fetch_error_silent () =
  (* Unreachable is not yanked: no update, no warn. *)
  let run _ _ = Alcotest.fail "registry must not run for pinned rows" in
  let fetch ?timeout_s:_ _ _ = Error "boom" in
  let lock =
    Lockfile.[ { id = "a"; tag = "v1.0"; sha256 = ""; thumbprint = ""; host = "" } ]
  in
  let got, warns =
    Update.check_all ~run ~fetch ~lock [ pin "a" "owner/tool" "1.0" Installed ]
  in
  Alcotest.(check (list (pair string string))) "no update" [] got;
  Alcotest.(check (list string)) "no warn" [] warns
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
    ; ( "upstream"
      , [ Alcotest.test_case "newer/current/fail" `Quick upstream_newer_current_fail
        ; Alcotest.test_case "forge urls" `Quick upstream_forge_urls
        ; Alcotest.test_case
            "check_all skips registry"
            `Quick
            check_all_pins_skip_registry
        ; Alcotest.test_case "yank warns" `Quick yank_warns_on_retagged
        ; Alcotest.test_case "yank error silent" `Quick yank_fetch_error_silent
        ] )
    ; ( "tui"
      , [ Alcotest.test_case "apply marks" `Quick apply_updates_marks
        ; Alcotest.test_case "apply empty" `Quick apply_updates_empty
        ] )
    ]
;;
