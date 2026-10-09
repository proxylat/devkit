open Devkit

let winget_table =
  "Name   Id   Version   Available   Source\n\
   --------------------------------\n\
   Git    Git.Git   2.47.1   2.48.0   winget\n\
   Brave  Brave.Brave  1.80.122  winget\n"
;;

let fake_bio ?(show_out = "", false) () : Bootstrap.io =
  { getenv = (fun _ -> None)
  ; is_file = (fun _ -> false)
  ; spawn = (fun _ _ -> show_out)
  ; cwd = (fun () -> Error "no cwd")
  ; exe_dir = (fun () -> Error "no exe")
  ; mkdir_p = (fun _ -> Error "ro")
  ; cache_dir = (fun () -> ".")
  ; latest_winget_cli = (fun () -> Error "offline")
  ; download = (fun ~url:_ -> Error "offline")
  ; unzip = (fun ~zip:_ ~dir:_ -> Error "ro")
  }
;;

(** In-memory filesystem. *)
let mem_fs (files : (string, string) Hashtbl.t) : App.fs =
  { read_file = Hashtbl.find_opt files
  ; append_file =
      (fun path text ->
        let cur = Option.value ~default:"" (Hashtbl.find_opt files path) in
        Hashtbl.replace files path (cur ^ text);
        Ok ())
  ; write_file =
      (fun path text ->
        Hashtbl.replace files path text;
        Ok ())
  }
;;

let scan_run prog _args =
  match prog with
  | "winget" -> Some winget_table
  | _ -> None
;;

let offline_fetch ?timeout_s:_ _ _ = Error "offline"

let env files =
  { App.run = scan_run; fs = mem_fs files; bio = fake_bio (); fetch = offline_fetch }
;;

let contains sub s =
  try
    ignore (Str.search_forward (Str.regexp_string sub) s 0);
    true
  with
  | Not_found -> false
;;

let load_missing () =
  let s, w, _ = App.load_manifest (mem_fs (Hashtbl.create 1)) Manifest.filename in
  Alcotest.(check int) "no sections" 0 (List.length s);
  Alcotest.(check string) "no winget path" "" w
;;

let resolve () =
  Alcotest.(check (result (list string) string))
    "canonical spellings"
    (Ok [ "winget"; "npm" ])
    (App.resolve_sources [ "Winget"; "NPM" ] ~tools:[]);
  (match App.resolve_sources [] ~tools:[] with
   | Ok srcs ->
     Alcotest.(check (list string)) "empty means defaults" Manifest.default_sources srcs
   | Error _ -> Alcotest.fail "empty should resolve");
  match App.resolve_sources [ "winget"; "bogus" ] ~tools:[] with
  | Ok _ -> Alcotest.fail "unknown should error"
  | Error e ->
    Alcotest.(check bool) "names source" true (contains "unknown source: bogus" e);
    Alcotest.(check bool) "lists known" true (contains "winget" e)
;;

let bad_sources () =
  (* sources=["bogus"]: manifest-driven commands fail before spawning
     anything. run_export is exempt: it scans with ~sources:[] (a full
     snapshot, filtered by --only/--except), so a bad key cannot break it. *)
  let files = Hashtbl.create 1 in
  Hashtbl.add files Manifest.filename "sources = [\"bogus\"]\n";
  let e = env files in
  Alcotest.(check bool)
    "add errors"
    true
    (match App.run_add e [ "Git.Git" ] with
     | [ m ] -> contains "unknown source: bogus" m
     | _ -> false);
  Alcotest.(check bool)
    "default shows error"
    true
    (contains "error: unknown source: bogus" (App.default_view ~tools:[] e))
;;

let win_reg_out =
  "HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\{A1}\n\
  \    DisplayName    REG_SZ    7-Zip 24.09\n\
  \    DisplayVersion    REG_SZ    24.09\n\n\
   HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\{B2}\n\
  \    DisplayName    REG_SZ    GitHub CLI\n\
  \    DisplayVersion    REG_SZ    2.62.0\n"
;;

let win_env files =
  { (env files) with
    run = (fun prog _ -> if prog = "reg" then Some win_reg_out else None)
  }
;;

let append_registry_note () =
  (* Simulated Windows scan: the only rows are registry orphans, which
     stay in the table but never persist. *)
  let files = Hashtbl.create 1 in
  Alcotest.(check (list string))
    "orphans noted, nothing appended"
    [ "  nothing new to append"; "  2 registry row(s) need manual ids" ]
    (App.run_append (win_env files) ~os:"Win32" []);
  Alcotest.(check bool) "no write" false (Hashtbl.mem files Manifest.filename)
;;

let export_registry_note () =
  let files = Hashtbl.create 1 in
  Alcotest.(check (list string))
    "registry skipped with note"
    [ "  no installed software found"; "  2 registry app(s) skipped (need manual ids)" ]
    (App.run_export (win_env files) ~os:"Win32" "out.toml");
  Alcotest.(check bool) "no toml written" false (Hashtbl.mem files "out.toml")
;;

let load_parses () =
  let files = Hashtbl.create 1 in
  Hashtbl.add
    files
    Manifest.filename
    "winget_path = \"C:\\\\w\\\\winget.exe\"\n\n\
     [[section]]\n\
     name = \"tools\"\n\n\
     [[section.package]]\n\
     id = \"Git.Git\"\n";
  let s, w, _ = App.load_manifest (mem_fs files) Manifest.filename in
  Alcotest.(check string) "winget path" "C:\\w\\winget.exe" w;
  Alcotest.(check int) "one section" 1 (List.length s)
;;

let append_groups () =
  let files = Hashtbl.create 1 in
  let items =
    [ { Manifest.typ = Manifest.Winget
      ; value = "Brave.Brave"
      ; installed_version = ""
      ; available_version = ""
      ; status = Manifest.New
      ; upstream = ""
      ; quarantine_days = 0
      }
    ; { Manifest.typ = Manifest.Winget
      ; value = "Git.Git"
      ; installed_version = ""
      ; available_version = ""
      ; status = Manifest.New
      ; upstream = ""
      ; quarantine_days = 0
      }
    ]
  in
  Alcotest.(check (result unit string))
    "ok"
    (Ok ())
    (App.append_selected (mem_fs files) Manifest.filename items);
  let written = Hashtbl.find files Manifest.filename in
  Alcotest.(check bool) "section header" true (contains "Newly detected" written);
  Alcotest.(check bool)
    "both ids"
    true
    (contains "Brave.Brave" written && contains "Git.Git" written)
;;

let append_empty () =
  let files = Hashtbl.create 1 in
  Alcotest.(check (result unit string))
    "ok"
    (Ok ())
    (App.append_selected (mem_fs files) Manifest.filename []);
  Alcotest.(check bool) "no write" false (Hashtbl.mem files Manifest.filename)
;;

let add () =
  let files = Hashtbl.create 1 in
  Hashtbl.add
    files
    Manifest.filename
    "[[section]]\nname = \"tools\"\n\n[[section.package]]\nid = \"Git.Git\"\n";
  let msgs = App.run_add (env files) [ "Git.Git"; "Nope.Nope"; "Brave.Brave" ] in
  Alcotest.(check (list string))
    "messages"
    [ "  skip (already in manifest): Git.Git"
    ; "  skip (not installed): Nope.Nope"
    ; "  appended 1 app(s) to " ^ Manifest.filename
    ]
    msgs;
  Alcotest.(check bool)
    "brave persisted"
    true
    (contains "Brave.Brave" (Hashtbl.find files Manifest.filename))
;;

let add_nothing () =
  let files = Hashtbl.create 1 in
  let msgs = App.run_add (env files) [ "Nope.Nope" ] in
  Alcotest.(check (list string))
    "nothing"
    [ "  skip (not installed): Nope.Nope"; "  nothing to append" ]
    msgs
;;

let export () =
  let files = Hashtbl.create 1 in
  let msgs = App.run_export (env files) "out.toml" in
  Alcotest.(check (list string))
    "messages"
    [ "wrote out.toml (2 apps)"
    ; "wrote out.json (2 winget apps, winget import -i ready)"
    ]
    msgs;
  let r = Devkit.Manifest.parse (Hashtbl.find files "out.toml") in
  let ids =
    List.concat_map
      (fun (sec : Devkit.Manifest.section) ->
         List.map (fun i -> i.Devkit.Manifest.value) sec.items)
      r.sections
  in
  Alcotest.(check (list string)) "exported ids" [ "Brave.Brave"; "Git.Git" ] ids;
  let j = Yojson.Basic.from_string (Hashtbl.find files "out.json") in
  let pkgs =
    match j with
    | `Assoc kvs ->
      (match List.assoc "Sources" kvs with
       | `List [ `Assoc src ] ->
         (match List.assoc "Packages" src with
          | `List l -> l
          | _ -> [])
       | _ -> [])
    | _ -> []
  in
  Alcotest.(check int) "json packages" 2 (List.length pkgs)
;;

let export_empty () =
  let files = Hashtbl.create 1 in
  let e = { (env files) with run = (fun _ _ -> None) } in
  Alcotest.(check (list string))
    "empty"
    [ "  no installed software found" ]
    (App.run_export e "out.toml")
;;

let export_no_winget () =
  let files = Hashtbl.create 1 in
  let e =
    { (env files) with
      run = (fun prog _ -> if prog = "npm" then Some "x@1.0.0\n" else None)
    }
  in
  let msgs = App.run_export e "out.toml" in
  Alcotest.(check bool)
    "skip message"
    true
    (List.mem "  no winget apps, json skipped" msgs);
  Alcotest.(check bool) "no json file" false (Hashtbl.mem files "out.json")
;;

let export_only () =
  let files = Hashtbl.create 1 in
  let e =
    { (env files) with
      run = (fun prog _ -> if prog = "npm" then Some "x@1.0.0\n" else None)
    }
  in
  let msgs = App.run_export e ~only:[ "npm" ] "out.toml" in
  Alcotest.(check (list string))
    "messages"
    [ "wrote out.toml (1 apps)"; "  no winget apps, json skipped" ]
    msgs;
  let r = Devkit.Manifest.parse (Hashtbl.find files "out.toml") in
  let secs = List.map (fun (sec : Devkit.Manifest.section) -> sec.name) r.sections in
  Alcotest.(check (list string)) "only npm section" [ "npm" ] secs;
  Alcotest.(check bool) "no json file" false (Hashtbl.mem files "out.json")
;;

let export_except () =
  let files = Hashtbl.create 1 in
  let msgs = App.run_export (env files) ~except:[ "winget" ] "out.toml" in
  Alcotest.(check (list string)) "filtered out" [ "  no installed software found" ] msgs;
  Alcotest.(check bool) "no toml file" false (Hashtbl.mem files "out.toml")
;;

let new_items () =
  let mk status =
    { Manifest.typ = Manifest.Winget
    ; value = "x"
    ; installed_version = ""
    ; available_version = ""
    ; status
    ; upstream = ""
    ; quarantine_days = 0
    }
  in
  let secs =
    [ { Manifest.name = "Newly detected"; items = [ mk Manifest.New ] }
    ; { Manifest.name = "Pending updates"; items = [ mk Manifest.New ] }
    ]
  in
  Alcotest.(check int) "newly only" 1 (List.length (App.new_items secs));
  Alcotest.(check int) "with pending" 2 (List.length (App.new_items ~extra:true secs))
;;

let show () =
  let bio = fake_bio ~show_out:("Version: 9.9\n", true) () in
  Alcotest.(check string) "version" "9.9" (App.winget_show bio "w" "Some.Id");
  let bad = fake_bio () in
  Alcotest.(check string) "failure" "" (App.winget_show bad "w" "Some.Id");
  Alcotest.(check string) "no winget" "" (App.winget_show bad "" "Some.Id")
;;

let default () =
  let files = Hashtbl.create 1 in
  Hashtbl.add
    files
    Manifest.filename
    "[[section]]\nname = \"tools\"\n\n[[section.package]]\nid = \"Git.Git\"\n";
  let s = App.default_view ~tools:[] (env files) in
  Alcotest.(check bool) "mentions app" true (contains "Git.Git" s)
;;

let default_upstream () =
  (* End to end: the vendor tag (equal to installed) overrules winget's
     Available column, so the row lands green instead of pending. The pin
     is a full GitHub URL; the API request still hits the canonical path. *)
  let files = Hashtbl.create 1 in
  Hashtbl.add
    files
    Manifest.filename
    "[[section]]\n\
     name = \"tools\"\n\n\
     [[section.package]]\n\
     id = \"Git.Git\"\n\
     upstream = \"https://github.com/git/git\"\n";
  let fetch ?timeout_s:_ url _ =
    if Strutil.contains_substring "api.github.com/repos/git/git/releases/latest" url
    then Ok {|{"tag_name": "v2.47.1", "assets": []}|}
    else Error "unexpected url"
  in
  let e = { (env files) with fetch } in
  let s = App.default_view ~tools:[] e in
  Alcotest.(check bool) "green row" true (contains "[✓] Git.Git 2.47.1" s);
  Alcotest.(check bool) "installed section" true (contains "INSTALLED" s);
  Alcotest.(check bool) "no pending" false (contains "PENDING" s)
;;

let default_gitlab () =
  (* End to end for a forge row: the npm scan names the repo short
     name, the GitLab tag (equal to installed) confirms it, and the
     row lands green. *)
  let files = Hashtbl.create 1 in
  Hashtbl.add
    files
    Manifest.filename
    "[[section]]\n\
     name = \"tools\"\n\n\
     [[section.package]]\n\
     id = \"gitlab:group/proj\"\n\
     upstream = \"gitlab:group/proj\"\n";
  let run prog _ = if prog = "npm" then Some "proj@1.0.0\n" else None in
  let fetch ?timeout_s:_ url _ =
    if Strutil.contains_substring "gitlab.com/api/v4/projects/group%2Fproj/releases" url
    then Ok {|[{"tag_name": "v1.0.0", "assets": {"links": []}}]|}
    else Error "unexpected url"
  in
  let e = { (env files) with run; fetch } in
  let s = App.default_view ~tools:[] e in
  Alcotest.(check bool) "green row" true (contains "[✓] group/proj 1.0.0" s);
  Alcotest.(check bool) "installed section" true (contains "INSTALLED" s);
  Alcotest.(check bool) "no pending" false (contains "PENDING" s)
;;

let scan_error () =
  let s = App.scan (env (Hashtbl.create 1)) ~override_path:"" ~extra:[] ~sources:[] () in
  Alcotest.(check string) "no winget" "" s.App.winget;
  Alcotest.(check bool) "error carried" true (s.App.winget_error <> "")
;;

let default_empty () =
  let e =
    { App.run = (fun _ _ -> None)
    ; fs = mem_fs (Hashtbl.create 1)
    ; bio = fake_bio ()
    ; fetch = offline_fetch
    }
  in
  let s = App.default_view ~tools:[] e in
  Alcotest.(check bool) "names winget" true (contains "winget" s)
;;

let doctor_manifest =
  "winget_path = \"C:\\\\w\\\\winget.exe\"\n\n\
   [[section]]\n\
   name = \"tools\"\n\n\
   [[section.package]]\n\
   id = \"Git.Git\"\n\
   upstream = \"git-for-windows/git\"\n\n\
   [[section.package]]\n\
   id = \"Evil.App\"\n\
   upstream = \"owner/repo\"\n\n\
   [[section.package]]\n\
   id = \"Plain.App\"\n"
;;

let doctor_fetch ?timeout_s:_ url _ =
  if contains "git.yaml" url
  then Ok "InstallerUrl: https://github.com/git-for-windows/git/releases/x.exe\n"
  else if contains "evil.yaml" url
  then Ok "InstallerUrl: https://evil.example.com/x.exe\n"
  else if contains "plain.yaml" url
  then Ok "InstallerUrl: https://plain.example.com/x.exe\n"
  else if contains "/2.0" url
  then (
    let dl =
      if contains "Git/Git" url
      then "https://x/git.yaml"
      else if contains "Evil/App" url
      then "https://x/evil.yaml"
      else "https://x/plain.yaml"
    in
    Ok
      (Printf.sprintf
         {|[{"type":"file","name":"x.installer.yaml","download_url":%S}]|}
         dl))
  else Ok {|[{"type":"dir","name":"2.0"}]|}
;;

let doctor_cross_check () =
  let files = Hashtbl.create 1 in
  Hashtbl.add files Manifest.filename doctor_manifest;
  let run prog _ =
    if prog = "C:\\w\\winget.exe"
    then Some "Name  Argument\nmsstore  https://x\n"
    else None
  in
  let e = { (env files) with run; fetch = doctor_fetch } in
  match App.doctor_view e with
  | Error _ -> Alcotest.fail "expected Ok"
  | Ok lines ->
    let body = String.concat "\n" lines in
    Alcotest.(check bool) "msstore ok" true (contains "msstore present" body);
    Alcotest.(check bool) "pinned ok" true (contains "Git.Git: ok (github.com)" body);
    Alcotest.(check bool)
      "mismatch flagged"
      true
      (contains
         "Evil.App: MISMATCH serves from evil.example.com, vendor is github.com"
         body);
    Alcotest.(check bool)
      "unpinned noted"
      true
      (contains "Plain.App: serves from plain.example.com (no upstream pin" body)
;;

let doctor_no_winget () =
  let e = env (Hashtbl.create 1) in
  match App.doctor_view e with
  | Error _ -> Alcotest.fail "expected Ok"
  | Ok lines ->
    let body = String.concat "\n" lines in
    Alcotest.(check bool)
      "hygiene skipped"
      true
      (contains "winget unavailable (skipped)" body);
    Alcotest.(check bool) "no rows" true (contains "no winget rows" body)
;;

let winget_table =
  "Brave  Brave.Brave  1.80.122  winget\n" ^ "Git  Git.Git  2.47.1  2.48.0  winget\n"
;;

let info_of_table () =
  (* Raw output parses; a failed winget falls back to scan rows; both
     empty means None. *)
  let apps =
    [ { Dashboard.name = "Brave.Brave"; version = "1.80.122"; pm = "winget" } ]
  in
  (match App.info_of (Some winget_table) [] with
   | None -> Alcotest.fail "table should parse"
   | Some m ->
     (match Winget_parse.IdMap.find_opt "git.git" m with
      | None -> Alcotest.fail "git row missing"
      | Some inf ->
        Alcotest.(check string) "available kept" "2.48.0" inf.Winget_parse.available));
  (match App.info_of None apps with
   | None -> Alcotest.fail "scan fallback should fire"
   | Some m -> Alcotest.(check int) "one fallback row" 1 (Winget_parse.IdMap.cardinal m));
  Alcotest.(check bool) "both empty" true (App.info_of None [] = None);
  Alcotest.(check bool) "garbage out" true (App.info_of (Some "garbage") [] = None)
;;

let () =
  Alcotest.run
    "app"
    [ ( "manifest"
      , [ Alcotest.test_case "missing" `Quick load_missing
        ; Alcotest.test_case "parses" `Quick load_parses
        ; Alcotest.test_case "resolve" `Quick resolve
        ] )
    ; ( "append"
      , [ Alcotest.test_case "groups" `Quick append_groups
        ; Alcotest.test_case "empty" `Quick append_empty
        ; Alcotest.test_case "new_items" `Quick new_items
        ] )
    ; ( "commands"
      , [ Alcotest.test_case "add" `Quick add
        ; Alcotest.test_case "add_nothing" `Quick add_nothing
        ; Alcotest.test_case "export" `Quick export
        ; Alcotest.test_case "export_empty" `Quick export_empty
        ; Alcotest.test_case "export_no_winget" `Quick export_no_winget
        ; Alcotest.test_case "export_only" `Quick export_only
        ; Alcotest.test_case "export_except" `Quick export_except
        ; Alcotest.test_case "show" `Quick show
        ; Alcotest.test_case "default" `Quick default
        ; Alcotest.test_case "default upstream" `Quick default_upstream
        ; Alcotest.test_case "default gitlab" `Quick default_gitlab
        ; Alcotest.test_case "bad sources" `Quick bad_sources
        ; Alcotest.test_case "append registry note" `Quick append_registry_note
        ; Alcotest.test_case "export registry note" `Quick export_registry_note
        ; Alcotest.test_case "scan error" `Quick scan_error
        ; Alcotest.test_case "default empty" `Quick default_empty
        ; Alcotest.test_case "doctor cross-check" `Quick doctor_cross_check
        ; Alcotest.test_case "doctor no winget" `Quick doctor_no_winget
        ; Alcotest.test_case "info of" `Quick info_of_table
        ] )
    ]
;;
