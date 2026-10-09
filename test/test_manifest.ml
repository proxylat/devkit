(** Tests for {!Manifest}: devkit.toml parsing and rendering. *)

open Devkit.Manifest

let contains = Devkit.Strutil.contains_substring

let basic =
  "winget_path = \"C:\\\\tools\\\\winget.exe\"\n\n\
   [[section]]\n\
   name = \"tools\"\n\n\
   [[section.package]]\n\
   id = \"Git.Git\"\n\
   upstream = \"git/git\"\n\n\
   [[section.package]]\n\
   id = \"npm:pyright\"\n\
   installed_version = \"1.2.3\"\n\n\
   [[section.package]]\n\
   id = \"owner/repo\"\n\n\
   [[section.package]]\n\
   id = \"https://example.com/dl/widget-2.0.exe\"\n\n\
   [[section.package]]\n\
   id = \"gitlab:group/proj\"\n\n\
   [[section.package]]\n\
   id = \"forgejo:https://git.example.com/o/r\"\n"
;;

let parses () =
  let r = parse basic in
  Alcotest.(check string) "winget path" "C:\\tools\\winget.exe" r.winget_path;
  Alcotest.(check int) "no warnings" 0 (List.length r.warnings);
  match r.sections with
  | [ sec ] ->
    Alcotest.(check string) "section name" "tools" sec.name;
    (match sec.items with
     | [ git; npm; gh; url; gl; fj ] ->
       Alcotest.(check string) "first id" "Git.Git" git.value;
       Alcotest.(check bool) "first typ" true (git.typ = Winget);
       Alcotest.(check string) "upstream" "git/git" git.upstream;
       Alcotest.(check string) "second id" "pyright" npm.value;
       Alcotest.(check string) "second pm" "npm" (type_string npm.typ);
       Alcotest.(check string) "second version" "1.2.3" npm.installed_version;
       Alcotest.(check bool) "third typ" true (gh.typ = GitHub);
       Alcotest.(check string) "third id" "owner/repo" gh.value;
       Alcotest.(check bool) "fourth typ" true (url.typ = Url);
       Alcotest.(check bool) "gitlab typ" true (gl.typ = GitLab "gitlab.com");
       Alcotest.(check string) "gitlab id" "group/proj" gl.value;
       Alcotest.(check bool) "forgejo typ" true (fj.typ = Forgejo "git.example.com");
       Alcotest.(check string) "forgejo id" "o/r" fj.value
     | _ -> Alcotest.fail "expected six items")
  | _ -> Alcotest.fail "expected one section"
;;

let one_pkg id =
  parse ("[[section]]\nname = \"t\"\n\n[[section.package]]\nid = \"" ^ id ^ "\"\n")
;;

let ladder () =
  let cases =
    [ "https://github.com/o/r/releases/download/v1/a.exe", GitHub, "o/r"
    ; "https://github.com/o/r", GitHub, "o/r"
    ; "https://github.com/o", Url, "https://github.com/o"
    ; "git@github.com:o/r.git", GitHub, "o/r"
    ; "winget:Git.Git", Winget, "Git.Git"
    ; "NPM:typescript", Pm "npm", "typescript"
    ; "cargo:ripgrep", Pm "cargo", "ripgrep"
    ; "github:o/r/", GitHub, "o/r"
    ; "@scope/pkg", Pm "npm", "@scope/pkg"
    ; "npm:@scope/pkg", Pm "npm", "@scope/pkg"
    ; "o/r", GitHub, "o/r"
    ; "Git.Git", Winget, "Git.Git"
    ; "url:https://x/y.exe", Url, "https://x/y.exe"
    ; "https://gitlab.com/group/sub/x", GitLab "gitlab.com", "group/sub/x"
    ; "gitlab:group/x", GitLab "gitlab.com", "group/x"
    ; "gitlab:https://git.example.com/g/x", GitLab "git.example.com", "g/x"
    ; "codeberg:o/r", Forgejo "codeberg.org", "o/r"
    ; "https://codeberg.org/o/r", Forgejo "codeberg.org", "o/r"
    ; "forgejo:https://git.example.com/o/r", Forgejo "git.example.com", "o/r"
    ; "gitea:https://git.example.com/o/r", Forgejo "git.example.com", "o/r"
    ]
  in
  List.iter
    (fun (id, typ, value) ->
       let r = one_pkg id in
       Alcotest.(check int) ("no warnings: " ^ id) 0 (List.length r.warnings);
       match r.sections with
       | [ sec ] ->
         (match sec.items with
          | [ it ] ->
            Alcotest.(check bool) ("typ: " ^ id) true (it.typ = typ);
            Alcotest.(check string) ("value: " ^ id) value it.value
          | _ -> Alcotest.fail ("expected one item: " ^ id))
       | _ -> Alcotest.fail ("expected one section: " ^ id))
    cases
;;

let rejections () =
  let cases =
    [ "typescript", "ambiguous"
    ; "typescript", "npm:typescript"
    ; "o/r/x", "owner/repo"
    ; "github:o", "owner/repo"
    ; "Foo.", "Publisher.App"
    ; "npm:", "nothing after"
    ; ":x", "empty prefix"
    ; "", "empty id"
    ; "npm:a b", "no spaces or backslashes"
    ; "forgejo:o/r", "need a host"
    ; "gitlab:onlyone", "group/project"
    ; "codeberg:a/b/c", "owner/repo"
    ]
  in
  List.iter
    (fun (id, frag) ->
       let r = one_pkg id in
       Alcotest.(check int) ("one warning: " ^ id) 1 (List.length r.warnings);
       Alcotest.(check bool)
         ("names fix: " ^ id)
         true
         (contains frag (List.nth r.warnings 0));
       match r.sections with
       | [ sec ] -> Alcotest.(check int) ("skipped: " ^ id) 0 (List.length sec.items)
       | _ -> Alcotest.fail ("expected one section: " ^ id))
    cases
;;

let pm_gone () =
  let r =
    parse
      "[[section]]\n\
       name = \"t\"\n\n\
       [[section.package]]\n\
       pm = \"npm\"\n\
       id = \"pyright\"\n\n\
       [[section.package]]\n\
       pm = \"winget\"\n"
  in
  Alcotest.(check int) "two warnings" 2 (List.length r.warnings);
  Alcotest.(check bool)
    "names fixed form"
    true
    (contains "id = \"npm:pyright\"" (List.nth r.warnings 0));
  Alcotest.(check bool) "missing id" true (contains "no id given" (List.nth r.warnings 1));
  match r.sections with
  | [ sec ] -> Alcotest.(check int) "both skipped" 0 (List.length sec.items)
  | _ -> Alcotest.fail "expected one section"
;;

let skips () =
  let r =
    parse
      "[[section]]\n\
       name = \"\"\n\n\
       [[section]]\n\
       name = \"t\"\n\n\
       [[section.package]]\n\
       id = \"\"\n\n\
       [[section.package]]\n\
       id = \"owner/repo\"\n"
  in
  Alcotest.(check int) "empty id warns" 1 (List.length r.warnings);
  match r.sections with
  | [ sec ] ->
    (match sec.items with
     | [ it ] ->
       Alcotest.(check bool) "github typ" true (it.typ = GitHub);
       Alcotest.(check string) "id" "owner/repo" it.value
     | _ -> Alcotest.fail "expected one item")
  | _ -> Alcotest.fail "expected one section"
;;

let malformed () =
  let r = parse "[[section\nname = " in
  Alcotest.(check int) "no sections" 0 (List.length r.sections);
  Alcotest.(check string) "no winget path" "" r.winget_path;
  Alcotest.(check int) "no warnings" 0 (List.length r.warnings)
;;

let sources () =
  let r =
    parse
      "sources = [\"registry\", \"npm\", \"winget\"]\n\n\
       [[section]]\n\
       name = \"t\"\n\n\
       [[section.package]]\n\
       id = \"Git.Git\"\n"
  in
  Alcotest.(check (list string)) "order kept" [ "registry"; "npm"; "winget" ] r.sources;
  let missing = parse "[[section]]\nname = \"t\"\n" in
  Alcotest.(check (list string)) "absent means defaults" default_sources missing.sources;
  let text = to_string r in
  Alcotest.(check bool) "key emitted" true (contains "sources" text);
  let r2 = parse text in
  Alcotest.(check (list string)) "round trip" r.sources r2.sources;
  let plain =
    parse "[[section]]\nname = \"t\"\n\n[[section.package]]\nid = \"Git.Git\"\n"
  in
  Alcotest.(check bool)
    "default key not emitted"
    false
    (contains "sources" (to_string plain))
;;

let round_trip () =
  let r = parse basic in
  let text = to_string r in
  Alcotest.(check bool) "no pm key" false (contains "pm =" text);
  Alcotest.(check bool) "prefixed npm" true (contains "npm:pyright" text);
  Alcotest.(check bool) "prefixed gitlab" true (contains "gitlab:group/proj" text);
  Alcotest.(check bool)
    "prefixed forgejo url"
    true
    (contains "forgejo:https://git.example.com/o/r" text);
  let r2 = parse text in
  Alcotest.(check string) "winget path" r.winget_path r2.winget_path;
  Alcotest.(check int) "sections" (List.length r.sections) (List.length r2.sections);
  let ids secs = List.concat_map (fun s -> List.map (fun i -> i.value) s.items) secs in
  Alcotest.(check (list string)) "ids" (ids r.sections) (ids r2.sections);
  let ups secs = List.concat_map (fun s -> List.map (fun i -> i.upstream) s.items) secs in
  Alcotest.(check (list string)) "upstream survives" (ups r.sections) (ups r2.sections)
;;

let quarantine () =
  let items text = (parse text).sections |> List.concat_map (fun s -> s.items) in
  (* Global resolves into every row. *)
  let r =
    parse
      "quarantine_days = 7\n\n\
       [[section]]\n\
       name = \"t\"\n\n\
       [[section.package]]\n\
       id = \"Git.Git\"\n\n\
       [[section.package]]\n\
       id = \"owner/repo\"\n"
  in
  Alcotest.(check int) "global" 7 r.quarantine;
  (match r.sections with
   | [ sec ] ->
     (match sec.items with
      | [ a; b ] ->
        Alcotest.(check int) "row inherits" 7 a.quarantine_days;
        Alcotest.(check int) "row inherits" 7 b.quarantine_days
      | _ -> Alcotest.fail "expected two items")
   | _ -> Alcotest.fail "expected one section");
  (* Per-row override wins, including an explicit 0. *)
  let got =
    items
      "quarantine_days = 7\n\n\
       [[section]]\n\
       name = \"t\"\n\n\
       [[section.package]]\n\
       id = \"Git.Git\"\n\
       quarantine_days = 3\n\n\
       [[section.package]]\n\
       id = \"owner/repo\"\n\
       quarantine_days = 0\n"
  in
  (match got with
   | [ a; b ] ->
     Alcotest.(check int) "override wins" 3 a.quarantine_days;
     Alcotest.(check int) "explicit zero wins" 0 b.quarantine_days
   | _ -> Alcotest.fail "expected two items");
  (* Override without a global. *)
  (match
     items
       "[[section]]\n\
        name = \"t\"\n\n\
        [[section.package]]\n\
        id = \"Git.Git\"\n\
        quarantine_days = 5\n"
   with
   | [ a ] -> Alcotest.(check int) "row only" 5 a.quarantine_days
   | _ -> Alcotest.fail "expected one item");
  (* Absent everywhere means 0. *)
  (match items "[[section]]\nname = \"t\"\n\n[[section.package]]\nid = \"Git.Git\"\n" with
   | [ a ] -> Alcotest.(check int) "default off" 0 a.quarantine_days
   | _ -> Alcotest.fail "expected one item");
  (* Negatives clamp to 0. *)
  let rneg =
    parse
      "quarantine_days = -2\n\n\
       [[section]]\n\
       name = \"t\"\n\n\
       [[section.package]]\n\
       id = \"Git.Git\"\n\
       quarantine_days = -9\n"
  in
  Alcotest.(check int) "global clamps" 0 rneg.quarantine;
  (match rneg.sections with
   | [ sec ] ->
     (match sec.items with
      | [ a ] -> Alcotest.(check int) "row clamps" 0 a.quarantine_days
      | _ -> Alcotest.fail "expected one item")
   | _ -> Alcotest.fail "expected one section");
  (* Non-int keys are ignored like other malformed keys. *)
  let rstr =
    parse
      "quarantine_days = \"soon\"\n\n\
       [[section]]\n\
       name = \"t\"\n\n\
       [[section.package]]\n\
       id = \"Git.Git\"\n\
       quarantine_days = \"many\"\n"
  in
  Alcotest.(check int) "global non-int ignored" 0 rstr.quarantine;
  match rstr.sections with
  | [ sec ] ->
    (match sec.items with
     | [ a ] -> Alcotest.(check int) "row non-int ignored" 0 a.quarantine_days
     | _ -> Alcotest.fail "expected one item")
  | _ -> Alcotest.fail "expected one section"
;;

let quarantine_round_trip () =
  let r =
    parse
      "quarantine_days = 7\n\n\
       [[section]]\n\
       name = \"t\"\n\n\
       [[section.package]]\n\
       id = \"Git.Git\"\n\
       quarantine_days = 3\n\n\
       [[section.package]]\n\
       id = \"owner/repo\"\n"
  in
  let text = to_string r in
  Alcotest.(check bool) "global emitted" true (contains "quarantine_days = 7" text);
  Alcotest.(check bool) "row emitted" true (contains "quarantine_days = 3" text);
  let r2 = parse text in
  Alcotest.(check int) "global survives" 7 r2.quarantine;
  (match r2.sections with
   | [ sec ] ->
     (match sec.items with
      | [ a; b ] ->
        (* The inheriting row was emitted with its resolved 7, so it
           re-parses explicit with the same value. *)
        Alcotest.(check int) "override survives" 3 a.quarantine_days;
        Alcotest.(check int) "inherited value survives" 7 b.quarantine_days
      | _ -> Alcotest.fail "expected two items")
   | _ -> Alcotest.fail "expected one section");
  (* Zero means no key at all. *)
  let plain =
    parse "[[section]]\nname = \"t\"\n\n[[section.package]]\nid = \"Git.Git\"\n"
  in
  Alcotest.(check bool) "zero not emitted" false (contains "quarantine" (to_string plain))
;;

let effective_upstream_self_pin () =
  (* Explicit pins win; forge rows without one answer to their own id;
     everything else yields no vendor repo. *)
  let module P = Devkit.Provider in
  let eff typ value upstream =
    effective_upstream { (make_item typ value) with upstream }
  in
  let cases =
    [ eff Winget "Git.Git" "git/git", Some (P.GitHub, "git/git")
    ; eff GitHub "o/r" "x/y", Some (P.GitHub, "x/y")
    ; eff GitHub "o/r" "", Some (P.GitHub, "o/r")
    ; eff (GitLab "git.example.com") "g/p" "", Some (P.GitLab "git.example.com", "g/p")
    ; eff (Forgejo "codeberg.org") "o/r" "", Some (P.Forgejo "codeberg.org", "o/r")
    ; eff Winget "Git.Git" "", None
    ; eff (Pm "npm") "typescript" "", None
    ; eff Url "https://x/y.exe" "", None
    ; eff Registry "VLC media player" "", None
    ]
  in
  List.iter (fun (got, want) -> Alcotest.(check bool) "upstream" true (got = want)) cases
;;

let () =
  Alcotest.run
    "manifest"
    [ ( "parse"
      , [ Alcotest.test_case "basic" `Quick parses
        ; Alcotest.test_case "ladder" `Quick ladder
        ; Alcotest.test_case "rejections" `Quick rejections
        ; Alcotest.test_case "pm gone" `Quick pm_gone
        ; Alcotest.test_case "skips" `Quick skips
        ; Alcotest.test_case "malformed" `Quick malformed
        ; Alcotest.test_case "sources" `Quick sources
        ; Alcotest.test_case "quarantine" `Quick quarantine
        ; Alcotest.test_case "effective upstream" `Quick effective_upstream_self_pin
        ] )
    ; ( "render"
      , [ Alcotest.test_case "round_trip" `Quick round_trip
        ; Alcotest.test_case "quarantine round_trip" `Quick quarantine_round_trip
        ] )
    ]
;;
