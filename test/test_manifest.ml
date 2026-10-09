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
        ] )
    ; "render", [ Alcotest.test_case "round_trip" `Quick round_trip ]
    ]
;;
