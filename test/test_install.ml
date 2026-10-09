(** Tests for {!Devkit.Install}: arg capture, already-installed readings,
    github dispatch, installer ext routing, browser selection. *)
open Devkit

let base_deps
      ?(winget = fun () -> "C:\\w\\winget.exe")
      ?(spawn = fun _ _ -> "", true)
      ?(browser = fun _ -> Ok ())
      ?(latest = fun _ _ -> Error "no net")
      ?(download = fun ~url:_ -> Error "no net")
      ?(fetch = fun ?timeout_s:_ _ _ -> Error "no net")
      ?(installer = fun _ -> Ok ())
      ?(lock = [])
      ?(lock_saved = ref [])
      ?(tools = [])
      ()
  : Install.deps
  =
  { Install.winget
  ; spawn
  ; open_browser = browser
  ; latest_release = latest
  ; download
  ; fetch
  ; run_installer = installer
  ; lock_load = (fun () -> lock)
  ; lock_save = (fun t -> lock_saved := t)
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

let win_release = { Gh.tag_name = "v1"; published_at = ""; assets = win_asset :: [] }

let test_winget_upstream_install () =
  (* Upstream rows download the vendor asset; winget never spawns, and
     the outcome still carries the winget id for row matching. Other
     spawns (attestation probe) are fine. *)
  let progs = ref [] in
  let got_url = ref "" in
  let d =
    base_deps
      ~spawn:(fun prog _ ->
        progs := prog :: !progs;
        "", true)
      ~latest:(fun _ repo ->
        Alcotest.(check string) "repo" "owner/tool" repo;
        Ok win_release)
      ~download:(fun ~url ->
        got_url := url;
        Ok "/tmp/tool.exe")
      ()
  in
  let r = Install.install ~upstream:"owner/tool" d "winget" "Foo.Bar" false in
  Alcotest.(check bool) "winget never spawned" false (List.mem "C:\\w\\winget.exe" !progs);
  Alcotest.(check string) "asset url" "https://x/tool.exe" !got_url;
  Alcotest.(check string) "winget id kept" "Foo.Bar" r.Install.value;
  Alcotest.(check bool) "installed" true (r.Install.status = Install.Installed)
;;

let test_winget_upstream_update () =
  let d =
    base_deps
      ~latest:(fun _ _ -> Ok win_release)
      ~download:(fun ~url:_ -> Ok "/tmp/tool.exe")
      ()
  in
  let r = Install.install ~upstream:"owner/tool" d "winget" "Foo.Bar" true in
  Alcotest.(check string) "winget id kept" "Foo.Bar" r.Install.value;
  Alcotest.(check bool) "updated" true (r.Install.status = Install.Updated)
;;

let test_pm_upstream_update () =
  (* Pinned plugin rows take the vendor asset path; no PM command
     spawns, and the outcome keeps the manifest value for matching.
     Other spawns (attestation probe) are fine. *)
  let progs = ref [] in
  let d =
    base_deps
      ~spawn:(fun prog _ ->
        progs := prog :: !progs;
        "", true)
      ~latest:(fun _ repo ->
        Alcotest.(check string) "repo" "owner/tool" repo;
        Ok win_release)
      ~download:(fun ~url:_ -> Ok "/tmp/tool.exe")
      ()
  in
  let r = Install.install ~upstream:"owner/tool" d "npm" "some-tool" true in
  Alcotest.(check bool) "pm never spawned" false (List.mem "npm" !progs);
  Alcotest.(check string) "value kept" "some-tool" r.Install.value;
  Alcotest.(check bool) "updated" true (r.Install.status = Install.Updated)
;;

let test_github_upstream_override () =
  let d =
    base_deps
      ~latest:(fun _ repo ->
        Alcotest.(check string) "repo" "other/repo" repo;
        Ok win_release)
      ~download:(fun ~url:_ -> Ok "/tmp/tool.exe")
      ()
  in
  let r = Install.install ~upstream:"other/repo" d "github" "owner/tool" true in
  Alcotest.(check string) "value kept" "owner/tool" r.Install.value;
  Alcotest.(check bool) "updated" true (r.Install.status = Install.Updated)
;;

let test_github_happy () =
  let got_url = ref "" in
  let got_path = ref "" in
  let d =
    base_deps
      ~latest:(fun _ repo ->
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
      ~latest:(fun _ _ -> Ok { Gh.tag_name = "v1"; published_at = ""; assets = [] })
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

let test_github_api_error_opens_page () =
  let opened = ref "" in
  let d =
    base_deps
      ~latest:(fun _ _ -> Error "HTTP 404")
      ~browser:(fun url ->
        opened := url;
        Ok ())
      ()
  in
  let r = Install.install d "github" "owner/tool" false in
  Alcotest.(check string) "repo page" "https://github.com/owner/tool" !opened;
  Alcotest.(check bool) "opened" true (r.Install.status = Install.Opened)
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
  Alcotest.(check bool)
    "ok"
    true
    (Install.real_open_browser ~os:"Unix" spawn "https://x" = Ok ());
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
  ignore (Install.real_open_browser ~os:"Unix" spawn "https://x");
  Alcotest.(check string) "open" "open" !seen
;;

let test_gitlab_install () =
  (* gitlab rows query the GitLab provider on the default host; the
     outcome keeps the manifest value. *)
  let seen = ref (Provider.GitHub, "") in
  let d =
    base_deps
      ~latest:(fun prov repo ->
        seen := prov, repo;
        Ok win_release)
      ~download:(fun ~url:_ -> Ok "/tmp/tool.exe")
      ()
  in
  let r = Install.install d "gitlab" "group/proj" false in
  Alcotest.(check string) "provider" "gitlab" (Provider.to_string (fst !seen));
  Alcotest.(check string) "host" "gitlab.com" (Provider.host_of (fst !seen));
  Alcotest.(check string) "repo" "group/proj" (snd !seen);
  Alcotest.(check string) "value kept" "group/proj" r.Install.value;
  Alcotest.(check bool) "installed" true (r.Install.status = Install.Installed)
;;

let test_gitlab_no_asset_opens_page () =
  (* GitLab has no latest-release permalink: the fallback opens the
     repo page itself. *)
  let opened = ref "" in
  let d =
    base_deps
      ~latest:(fun _ _ -> Ok { Gh.tag_name = "v1"; published_at = ""; assets = [] })
      ~browser:(fun url ->
        opened := url;
        Ok ())
      ()
  in
  let r = Install.install d "gitlab" "group/proj" false in
  Alcotest.(check string) "repo page" "https://gitlab.com/group/proj" !opened;
  Alcotest.(check bool) "opened" true (r.Install.status = Install.Opened)
;;

let test_forgejo_selfhosted () =
  (* forgejo rows carry their self-hosted host; the manifest value is
     the bare path, the host rides alongside. *)
  let seen = ref (Provider.GitHub, "") in
  let d =
    base_deps
      ~latest:(fun prov repo ->
        seen := prov, repo;
        Ok win_release)
      ~download:(fun ~url:_ -> Ok "/tmp/tool.exe")
      ()
  in
  let r = Install.install ~host:"git.example.com" d "forgejo" "owner/repo" false in
  Alcotest.(check string) "provider" "forgejo" (Provider.to_string (fst !seen));
  Alcotest.(check string) "host" "git.example.com" (Provider.host_of (fst !seen));
  Alcotest.(check string) "repo" "owner/repo" (snd !seen);
  Alcotest.(check string) "value kept" "owner/repo" r.Install.value;
  Alcotest.(check bool) "installed" true (r.Install.status = Install.Installed)
;;

let real_download () =
  (* Fake download that writes a real temp file inside a per-test subdir
     (like {!Hash.download_and_verify}), so block-path deletion is
     observable without touching shared dirs. *)
  let path = ref "" in
  let download ~url:_ =
    let dir = Filename.temp_dir "devkit-inst-" "" in
    let p = Filename.concat dir "tool.exe" in
    let oc = open_out_bin p in
    output_string oc "payload";
    close_out oc;
    path := p;
    Ok p
  in
  download, path
;;

let ps_spawn (status : string) : Bootstrap.spawn =
  fun prog _ -> if prog = "powershell" then status, true else "", true
;;

let test_verify_sig_block_deletes () =
  let download, path = real_download () in
  let ran = ref false in
  let d =
    base_deps
      ~spawn:(ps_spawn "HashMismatch")
      ~latest:(fun _ _ -> Ok win_release)
      ~download
      ~installer:(fun _ ->
        ran := true;
        Ok ())
      ()
  in
  let r = Install.install ~os:"Win32" d "github" "owner/tool" false in
  (match r.Install.status with
   | Install.Failed m ->
     Alcotest.(check bool)
       "names status"
       true
       (Strutil.contains_substring "HashMismatch" m)
   | _ -> Alcotest.fail "expected Failed");
  Alcotest.(check bool) "download deleted" false (Sys.file_exists !path);
  Alcotest.(check bool) "installer never ran" false !ran
;;

let test_verify_sig_warn_proceeds () =
  let download, path = real_download () in
  let ran = ref false in
  let d =
    base_deps
      ~spawn:(ps_spawn "NotSigned")
      ~latest:(fun _ _ -> Ok win_release)
      ~download
      ~installer:(fun _ ->
        ran := true;
        Ok ())
      ()
  in
  let r = Install.install ~os:"Win32" d "github" "owner/tool" false in
  Alcotest.(check bool) "installed" true (r.Install.status = Install.Installed);
  Alcotest.(check bool) "installer ran" true !ran;
  (* signature warn + no-checksum-file warn, in pipeline order. *)
  Alcotest.(check int) "two warnings" 2 (List.length r.Install.warnings);
  Alcotest.(check bool)
    "sig first"
    true
    (Strutil.contains_substring "unsigned" (List.nth r.Install.warnings 0));
  try Sys.remove !path with
  | _ -> ()
;;

let test_verify_sum_block_deletes () =
  let sums =
    { Gh.name = "SHA256SUMS"; browser_download_url = "https://x/s"; size = 1L }
  in
  let rel = { win_release with Gh.assets = [ win_asset; sums ] } in
  let download, path = real_download () in
  let d =
    base_deps
      ~spawn:(ps_spawn "Valid")
      ~latest:(fun _ _ -> Ok rel)
      ~fetch:(fun ?timeout_s:_ _ _ -> Ok (String.make 64 '0' ^ "  Tool-win-x64.exe\n"))
      ~download
      ()
  in
  let r = Install.install ~os:"Win32" d "github" "owner/tool" false in
  (match r.Install.status with
   | Install.Failed m ->
     Alcotest.(check bool)
       "mismatch"
       true
       (Strutil.contains_substring "mismatch" (String.lowercase_ascii m))
   | _ -> Alcotest.fail "expected Failed");
  Alcotest.(check bool) "download deleted" false (Sys.file_exists !path)
;;

let test_lock_records_entry () =
  let download, path = real_download () in
  let saved = ref [] in
  let d = base_deps ~latest:(fun _ _ -> Ok win_release) ~download ~lock_saved:saved () in
  let r = Install.install d "github" "owner/tool" false in
  Alcotest.(check bool) "installed" true (r.Install.status = Install.Installed);
  (match Lockfile.find !saved "owner/tool" with
   | None -> Alcotest.fail "expected lock entry"
   | Some e ->
     Alcotest.(check string) "tag" "v1" e.Lockfile.tag;
     let sha =
       match Hash.sha256_file !path with
       | Ok s -> s
       | Error _ -> Alcotest.fail "sha of download"
     in
     Alcotest.(check string) "sha" sha e.Lockfile.sha256;
     Alcotest.(check string) "host" "github.com" e.Lockfile.host);
  try Sys.remove !path with
  | _ -> ()
;;

let test_lock_blocks_recut_tag () =
  let download, path = real_download () in
  let lock =
    Lockfile.
      [ { id = "owner/tool"
        ; tag = "v1"
        ; sha256 = String.make 64 '0'
        ; thumbprint = ""
        ; host = ""
        }
      ]
  in
  let ran = ref false in
  let d =
    base_deps
      ~latest:(fun _ _ -> Ok win_release)
      ~download
      ~lock
      ~installer:(fun _ ->
        ran := true;
        Ok ())
      ()
  in
  let r = Install.install d "github" "owner/tool" false in
  (match r.Install.status with
   | Install.Failed m ->
     Alcotest.(check bool)
       "names re-cut"
       true
       (Strutil.contains_substring "changed bytes" m)
   | _ -> Alcotest.fail "expected Failed");
  Alcotest.(check bool) "download deleted" false (Sys.file_exists !path);
  Alcotest.(check bool) "installer never ran" false !ran
;;

let test_quarantine_blocks_young () =
  let rel = { win_release with Gh.published_at = "2999-01-01T00:00:00Z" } in
  let downloaded = ref false in
  let d =
    base_deps
      ~latest:(fun _ _ -> Ok rel)
      ~download:(fun ~url:_ ->
        downloaded := true;
        Ok "/tmp/tool.exe")
      ()
  in
  let r = Install.install ~quarantine_days:5 d "github" "owner/tool" false in
  (match r.Install.status with
   | Install.Failed m ->
     Alcotest.(check bool) "quarantined" true (Strutil.contains_substring "quarantined" m)
   | _ -> Alcotest.fail "expected Failed");
  Alcotest.(check bool) "no download" false !downloaded
;;

let test_quarantine_old_proceeds () =
  let rel = { win_release with Gh.published_at = "2020-01-01T00:00:00Z" } in
  let d =
    base_deps ~latest:(fun _ _ -> Ok rel) ~download:(fun ~url:_ -> Ok "/tmp/tool.exe") ()
  in
  let r = Install.install ~quarantine_days:5 d "github" "owner/tool" false in
  Alcotest.(check bool) "installed" true (r.Install.status = Install.Installed)
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
        ; Alcotest.test_case "upstream install" `Quick test_winget_upstream_install
        ; Alcotest.test_case "upstream update" `Quick test_winget_upstream_update
        ; Alcotest.test_case "pm upstream update" `Quick test_pm_upstream_update
        ; Alcotest.test_case
            "github upstream override"
            `Quick
            test_github_upstream_override
        ] )
    ; ( "github"
      , [ Alcotest.test_case "happy path" `Quick test_github_happy
        ; Alcotest.test_case "no asset opens page" `Quick test_github_no_asset_opens_page
        ; Alcotest.test_case
            "api error opens page"
            `Quick
            test_github_api_error_opens_page
        ] )
    ; ( "forges"
      , [ Alcotest.test_case "gitlab install" `Quick test_gitlab_install
        ; Alcotest.test_case
            "gitlab no asset opens page"
            `Quick
            test_gitlab_no_asset_opens_page
        ; Alcotest.test_case "forgejo self-hosted" `Quick test_forgejo_selfhosted
        ] )
    ; ( "dispatch"
      , [ Alcotest.test_case "url opens" `Quick test_url_opens
        ; Alcotest.test_case "unknown kind skips" `Quick test_unknown_kind_skips
        ] )
    ; ( "verify"
      , [ Alcotest.test_case "sig block deletes" `Quick test_verify_sig_block_deletes
        ; Alcotest.test_case "sig warn proceeds" `Quick test_verify_sig_warn_proceeds
        ; Alcotest.test_case "sum block deletes" `Quick test_verify_sum_block_deletes
        ] )
    ; ( "lock"
      , [ Alcotest.test_case "records entry" `Quick test_lock_records_entry
        ; Alcotest.test_case "re-cut tag blocked" `Quick test_lock_blocks_recut_tag
        ; Alcotest.test_case "young quarantined" `Quick test_quarantine_blocks_young
        ; Alcotest.test_case "old proceeds" `Quick test_quarantine_old_proceeds
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
