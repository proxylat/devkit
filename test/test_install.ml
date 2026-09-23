(** Tests for {!Devkit.Install}: arg capture, already-installed readings,
    github dispatch, installer ext routing, browser selection. *)
open Devkit

let base_deps
      ?(winget = fun () -> "C:\\w\\winget.exe")
      ?(spawn = fun _ _ -> "", true)
      ?(browser = fun _ -> Ok ())
      ?(latest = fun _ -> Error "no net")
      ?(download = fun ~url:_ -> Error "no net")
      ?(installer = fun _ -> Ok ())
      ?(tools = [])
      ()
  : Install.deps
  =
  { Install.winget
  ; spawn
  ; open_browser = browser
  ; latest_release = latest
  ; download
  ; run_installer = installer
  ; tools
  }
;;

let test_status_strings () =
  let cases =
    [ Install.Installed, "installed"
    ; Install.Updated, "updated"
    ; Install.Opened, "opened"
    ; Install.Skipped "x", "skip"
    ; Install.Failed "x", "error"
    ]
  in
  List.iter
    (fun (s, want) -> Alcotest.(check string) want want (Install.status_to_string s))
    cases
;;

let test_winget_install_args () =
  let seen = ref ("", []) in
  let d =
    base_deps
      ~spawn:(fun prog args ->
        seen := prog, args;
        "", true)
      ()
  in
  let r = Install.install d "winget" "Foo.Bar" false in
  Alcotest.(check string) "prog" "C:\\w\\winget.exe" (fst !seen);
  Alcotest.(check (list string))
    "args"
    [ "install"
    ; "--id"
    ; "Foo.Bar"
    ; "-e"
    ; "--accept-package-agreements"
    ; "--accept-source-agreements"
    ; "--silent"
    ]
    (snd !seen);
  Alcotest.(check string) "value" "Foo.Bar" r.Install.value;
  Alcotest.(check bool) "installed" true (r.Install.status = Install.Installed)
;;

let test_winget_upgrade_args () =
  let seen = ref [] in
  let d =
    base_deps
      ~spawn:(fun _ args ->
        seen := args;
        "", true)
      ()
  in
  let r = Install.install d "winget" "Foo.Bar" true in
  Alcotest.(check (list string))
    "upgrade args"
    [ "upgrade"
    ; "--id"
    ; "Foo.Bar"
    ; "--accept-package-agreements"
    ; "--accept-source-agreements"
    ]
    !seen;
  Alcotest.(check bool) "updated" true (r.Install.status = Install.Updated)
;;

let test_winget_unavailable () =
  let d = base_deps ~winget:(fun () -> "") () in
  let r = Install.install d "winget" "Foo.Bar" false in
  Alcotest.(check bool)
    "unavailable"
    true
    (r.Install.status = Install.Failed "winget unavailable")
;;

let test_winget_already_installed () =
  let d =
    base_deps ~spawn:(fun _ _ -> "Found an existing package already installed", false) ()
  in
  let r = Install.install d "winget" "Foo.Bar" false in
  Alcotest.(check bool) "installed" true (r.Install.status = Install.Installed)
;;

let test_winget_no_newer_update () =
  let d =
    base_deps ~spawn:(fun _ _ -> "No newer package versions are available", false) ()
  in
  let r = Install.install d "winget" "Foo.Bar" true in
  Alcotest.(check bool) "updated" true (r.Install.status = Install.Updated)
;;

let test_winget_real_error () =
  let d = base_deps ~spawn:(fun _ _ -> "Access denied", false) () in
  let r = Install.install d "winget" "Foo.Bar" false in
  match r.Install.status with
  | Install.Failed msg ->
    Alcotest.(check bool)
      "mentions output"
      true
      (Strutil.contains_substring "Access denied" msg)
  | _ -> Alcotest.fail "expected failure"
;;

let win_asset =
  { Gh.name = "Tool-win-x64.exe"; browser_download_url = "https://x/tool.exe"; size = 1L }
;;

let win_release = { Gh.tag_name = "v1"; assets = win_asset :: [] }

let test_github_happy () =
  let got_url = ref "" in
  let got_path = ref "" in
  let d =
    base_deps
      ~latest:(fun repo ->
        Alcotest.(check string) "repo" "owner/tool" repo;
        Ok win_release)
      ~download:(fun ~url ->
        got_url := url;
        Ok "/tmp/tool.exe")
      ~installer:(fun path ->
        got_path := path;
        Ok ())
      ()
  in
  let r = Install.install d "github" "owner/tool" false in
  Alcotest.(check string) "asset url" "https://x/tool.exe" !got_url;
  Alcotest.(check string) "installer path" "/tmp/tool.exe" !got_path;
  Alcotest.(check bool) "installed" true (r.Install.status = Install.Installed)
;;

let test_github_no_asset_opens_page () =
  let opened = ref "" in
  let d =
    base_deps
      ~latest:(fun _ -> Ok { Gh.tag_name = "v1"; assets = [] })
      ~browser:(fun url ->
        opened := url;
        Ok ())
      ()
  in
  let r = Install.install d "github" "owner/tool" false in
  Alcotest.(check string)
    "releases page"
    "https://github.com/owner/tool/releases/latest"
    !opened;
  Alcotest.(check bool) "opened" true (r.Install.status = Install.Opened)
;;

let test_github_api_error () =
  let d = base_deps ~latest:(fun _ -> Error "HTTP 404") () in
  let r = Install.install d "github" "owner/tool" false in
  Alcotest.(check bool) "failed" true (r.Install.status = Install.Failed "HTTP 404")
;;

let test_url_opens () =
  let d = base_deps ~browser:(fun _ -> Ok ()) () in
  let r = Install.install d "url" "https://example.com" false in
  Alcotest.(check bool) "opened" true (r.Install.status = Install.Opened)
;;

let test_unknown_kind_skips () =
  let d = base_deps () in
  let r = Install.install d "apt" "foo" false in
  match r.Install.status with
  | Install.Skipped msg ->
    Alcotest.(check bool) "names kind" true (Strutil.contains_substring "apt" msg)
  | _ -> Alcotest.fail "expected skip"
;;

let mytool =
  { Plugin.name = "mytool"
  ; prog = "mytool"
  ; list_args = [ "list" ]
  ; install = Some [ "mytool"; "add"; "{id}" ]
  ; upgrade = Some [ "mytool"; "bump"; "{id}" ]
  ; parse = (fun _ -> [])
  }
;;

let test_plugin_install_happy () =
  let seen = ref ("", []) in
  let d =
    base_deps
      ~tools:[ mytool ]
      ~spawn:(fun prog args ->
        seen := prog, args;
        "", true)
      ()
  in
  let r = Install.install d "mytool" "some-pkg" false in
  Alcotest.(check string) "prog" "mytool" (fst !seen);
  Alcotest.(check (list string)) "args" [ "add"; "some-pkg" ] (snd !seen);
  Alcotest.(check bool) "installed" true (r.Install.status = Install.Installed)
;;

let test_plugin_upgrade_happy () =
  let seen = ref ("", []) in
  let d =
    base_deps
      ~tools:[ mytool ]
      ~spawn:(fun prog args ->
        seen := prog, args;
        "", true)
      ()
  in
  let r = Install.install d "MYTOOL" "some-pkg" true in
  Alcotest.(check string) "prog" "mytool" (fst !seen);
  Alcotest.(check (list string)) "args" [ "bump"; "some-pkg" ] (snd !seen);
  Alcotest.(check bool) "updated" true (r.Install.status = Install.Updated)
;;

let test_plugin_missing_template () =
  let bare = { mytool with install = None; upgrade = None } in
  let d = base_deps ~tools:[ bare ] () in
  let r = Install.install d "mytool" "some-pkg" false in
  match r.Install.status with
  | Install.Failed msg ->
    Alcotest.(check bool)
      "names template"
      true
      (Strutil.contains_substring "no install template" msg)
  | _ -> Alcotest.fail "expected failure"
;;

let test_plugin_spawn_fail () =
  let d = base_deps ~tools:[ mytool ] ~spawn:(fun _ _ -> "denied", false) () in
  let r = Install.install d "mytool" "some-pkg" false in
  match r.Install.status with
  | Install.Failed msg ->
    Alcotest.(check bool) "keeps output" true (Strutil.contains_substring "denied" msg)
  | _ -> Alcotest.fail "expected failure"
;;

let test_run_installer_msi () =
  let seen = ref ("", []) in
  let spawn prog args =
    seen := prog, args;
    "", true
  in
  Alcotest.(check bool) "msi ok" true (Install.run_installer spawn "C:\\s.MSI" = Ok ());
  Alcotest.(check string) "msiexec" "msiexec" (fst !seen);
  Alcotest.(check (list string))
    "msi args"
    [ "/i"; "C:\\s.MSI"; "/quiet"; "/norestart" ]
    (snd !seen)
;;

let test_run_installer_exe () =
  let seen = ref ("", []) in
  let spawn prog args =
    seen := prog, args;
    "", true
  in
  Alcotest.(check bool)
    "exe ok"
    true
    (Install.run_installer spawn "/tmp/setup.exe" = Ok ());
  Alcotest.(check string) "self" "/tmp/setup.exe" (fst !seen);
  Alcotest.(check (list string)) "switches" [ "/S"; "/silent"; "/verysilent" ] (snd !seen)
;;

let test_run_installer_unsupported () =
  match Install.run_installer (fun _ _ -> "", true) "/tmp/app.zip" with
  | Ok () -> Alcotest.fail "expected refusal"
  | Error e ->
    Alcotest.(check bool) "names ext" true (Strutil.contains_substring ".zip" e)
;;

let test_run_installer_failure () =
  match Install.run_installer (fun _ _ -> "denied", false) "/tmp/s.exe" with
  | Ok () -> Alcotest.fail "expected failure"
  | Error e ->
    Alcotest.(check bool) "output kept" true (Strutil.contains_substring "denied" e)
;;

let test_browser_linux () =
  let seen = ref ("", []) in
  let spawn prog args =
    if prog = "uname"
    then "Linux", true
    else (
      seen := prog, args;
      "", true)
  in
  Alcotest.(check bool) "ok" true (Install.real_open_browser spawn "https://x" = Ok ());
  Alcotest.(check string) "xdg-open" "xdg-open" (fst !seen)
;;

let test_browser_darwin () =
  let seen = ref "" in
  let spawn prog args =
    if prog = "uname"
    then "Darwin", true
    else (
      seen := prog;
      ignore args;
      "", true)
  in
  ignore (Install.real_open_browser spawn "https://x");
  Alcotest.(check string) "open" "open" !seen
;;

let () =
  Alcotest.run
    "install"
    [ "status", [ Alcotest.test_case "strings" `Quick test_status_strings ]
    ; ( "winget"
      , [ Alcotest.test_case "install args" `Quick test_winget_install_args
        ; Alcotest.test_case "upgrade args" `Quick test_winget_upgrade_args
        ; Alcotest.test_case "unavailable" `Quick test_winget_unavailable
        ; Alcotest.test_case "already installed" `Quick test_winget_already_installed
        ; Alcotest.test_case "no newer on update" `Quick test_winget_no_newer_update
        ; Alcotest.test_case "real error" `Quick test_winget_real_error
        ] )
    ; ( "github"
      , [ Alcotest.test_case "happy path" `Quick test_github_happy
        ; Alcotest.test_case "no asset opens page" `Quick test_github_no_asset_opens_page
        ; Alcotest.test_case "api error" `Quick test_github_api_error
        ] )
    ; ( "dispatch"
      , [ Alcotest.test_case "url opens" `Quick test_url_opens
        ; Alcotest.test_case "unknown kind skips" `Quick test_unknown_kind_skips
        ] )
    ; ( "plugin"
      , [ Alcotest.test_case "install happy" `Quick test_plugin_install_happy
        ; Alcotest.test_case "upgrade happy" `Quick test_plugin_upgrade_happy
        ; Alcotest.test_case "missing template" `Quick test_plugin_missing_template
        ; Alcotest.test_case "spawn fail" `Quick test_plugin_spawn_fail
        ] )
    ; ( "run_installer"
      , [ Alcotest.test_case "msi" `Quick test_run_installer_msi
        ; Alcotest.test_case "exe" `Quick test_run_installer_exe
        ; Alcotest.test_case "unsupported" `Quick test_run_installer_unsupported
        ; Alcotest.test_case "failure" `Quick test_run_installer_failure
        ] )
    ; ( "browser"
      , [ Alcotest.test_case "linux" `Quick test_browser_linux
        ; Alcotest.test_case "darwin" `Quick test_browser_darwin
        ] )
    ]
;;
