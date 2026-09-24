(** Tests for {!Manifest}: devkit.toml parsing and rendering. *)

open Devkit.Manifest

let basic =
  "winget_path = \"C:\\\\tools\\\\winget.exe\"\n\n\
   [[section]]\n\
   name = \"tools\"\n\n\
   [[section.package]]\n\
   pm = \"winget\"\n\
   id = \"Git.Git\"\n\n\
   [[section.package]]\n\
   pm = \"npm\"\n\
   id = \"pyright\"\n\
   installed_version = \"1.2.3\"\n"
;;

let parses () =
  let r = parse basic in
  Alcotest.(check string) "winget path" "C:\\tools\\winget.exe" r.winget_path;
  match r.sections with
  | [ sec ] ->
    Alcotest.(check string) "section name" "tools" sec.name;
    (match sec.items with
     | [ git; npm ] ->
       Alcotest.(check string) "first id" "Git.Git" git.value;
       Alcotest.(check bool) "first typ" true (git.typ = Winget);
       Alcotest.(check string) "second id" "pyright" npm.value;
       Alcotest.(check string) "second pm" "npm" (type_string npm.typ);
       Alcotest.(check string) "second version" "1.2.3" npm.installed_version
     | _ -> Alcotest.fail "expected two items")
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
       pm = \"winget\"\n\
       id = \"\"\n\n\
       [[section.package]]\n\
       pm = \"github\"\n\
       id = \"owner/repo\"\n"
  in
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
  Alcotest.(check string) "no winget path" "" r.winget_path
;;

let round_trip () =
  let r = parse basic in
  let r2 = parse (to_string r) in
  Alcotest.(check string) "winget path" r.winget_path r2.winget_path;
  Alcotest.(check int) "sections" (List.length r.sections) (List.length r2.sections);
  let ids secs = List.concat_map (fun s -> List.map (fun i -> i.value) s.items) secs in
  Alcotest.(check (list string)) "ids" (ids r.sections) (ids r2.sections)
;;

let () =
  Alcotest.run
    "manifest"
    [ ( "parse"
      , [ Alcotest.test_case "basic" `Quick parses
        ; Alcotest.test_case "skips" `Quick skips
        ; Alcotest.test_case "malformed" `Quick malformed
        ] )
    ; "render", [ Alcotest.test_case "round_trip" `Quick round_trip ]
    ]
;;
