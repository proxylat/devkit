(** Tests for {!Winget_parse}: winget list table parsing. *)

open Devkit.Winget_parse

let aligned_output =
  "Name                                        Id                                \
   Version      Available    Source\n"
  ^ "--------------------------------------------------------------------------------------------------\n"
  ^ "7-Zip 24.09 (x64)                           7zip.7zip                       \
     24.09        25.00        winget\n"
  ^ "Brave                                       Brave.Brave                     \
     1.80.122                  winget\n"
  ^ "Microsoft Visual C++ 2015-2022 Redistributable (x64) - 14.44.35211  \
     Microsoft.VCRedist.2015+.x64       14.44.35211  Unknown      winget\n"
;;

let piped_output =
  "Brave  Brave.Brave  1.80.122  winget\n" ^ "Git  Git.Git  2.47.1  2.48.0  winget\n"
;;

let lookup m id = IdMap.find_opt (String.lowercase_ascii id) m

let aligned () =
  let m = parse_list_table aligned_output in
  Alcotest.(check int) "three rows" 3 (IdMap.cardinal m);
  (match lookup m "7zip.7zip" with
   | None -> Alcotest.fail "7zip missing"
   | Some inf ->
     Alcotest.(check string) "name with spaces" "7-Zip 24.09 (x64)" inf.name;
     Alcotest.(check string) "version" "24.09" inf.version;
     Alcotest.(check string) "available" "25.00" inf.available;
     Alcotest.(check string) "id case kept" "7zip.7zip" inf.id);
  (match lookup m "Brave.Brave" with
   | None -> Alcotest.fail "brave missing"
   | Some inf ->
     Alcotest.(check string) "no available" "" inf.available;
     Alcotest.(check string) "version" "1.80.122" inf.version);
  match lookup m "Microsoft.VCRedist.2015+.x64" with
  | None -> Alcotest.fail "vcredist missing"
  | Some inf ->
    (* "Unknown" has no digit → not an available version. *)
    Alcotest.(check string) "unknown ignored" "" inf.available;
    Alcotest.(check string)
      "long name"
      "Microsoft Visual C++ 2015-2022 Redistributable (x64) - 14.44.35211"
      inf.name
;;

let piped () =
  let m = parse_list_table piped_output in
  Alcotest.(check int) "two rows" 2 (IdMap.cardinal m);
  match lookup m "git.git" with
  | None -> Alcotest.fail "git missing"
  | Some inf -> Alcotest.(check string) "available" "2.48.0" inf.available
;;

let header_and_separator_skipped () =
  let m = parse_list_table "Nombre  Id  Versión\n───┼───┼──\nFoo.Bar  1.0  winget\n" in
  (* "Foo.Bar  1.0  winget": single-space split → one column → skipped. *)
  Alcotest.(check int) "nothing parsed" 0 (IdMap.cardinal m)
;;

let same_version_no_update () =
  let m = parse_list_table "Git  Git.Git  2.48.0  2.48.0  winget\n" in
  match lookup m "git.git" with
  | None -> Alcotest.fail "git missing"
  | Some inf -> Alcotest.(check string) "equal ignored" "" inf.available
;;

let separator () =
  Alcotest.(check bool) "dashes" true (is_winget_separator "------  ----");
  Alcotest.(check bool) "box drawing" true (is_winget_separator "─────┼─────┼────");
  Alcotest.(check bool) "empty false" false (is_winget_separator "");
  Alcotest.(check bool) "name false" false (is_winget_separator "Brave  1.0");
  Alcotest.(check bool) "version row false" false (is_winget_separator "Git.Git  2.47.1")
;;

let versions () =
  Alcotest.(check bool) "digits" true (looks_like_version "2.48.0");
  Alcotest.(check bool) "unknown" false (looks_like_version "Unknown");
  Alcotest.(check bool) "source" false (looks_like_version "winget")
;;

let cols () =
  Alcotest.(check (list string))
    "single spaces survive"
    [ "7-Zip 24.09 (x64)"; "7zip.7zip"; "24.09" ]
    (split_cols "7-Zip 24.09 (x64)   7zip.7zip   24.09")
;;

let () =
  Alcotest.run
    "winget_parse"
    [ ( "list table"
      , [ Alcotest.test_case "aligned output" `Quick aligned
        ; Alcotest.test_case "piped output" `Quick piped
        ; Alcotest.test_case
            "header/separator skipped"
            `Quick
            header_and_separator_skipped
        ; Alcotest.test_case "equal version" `Quick same_version_no_update
        ] )
    ; ( "helpers"
      , [ Alcotest.test_case "separator" `Quick separator
        ; Alcotest.test_case "versions" `Quick versions
        ; Alcotest.test_case "columns" `Quick cols
        ] )
    ]
;;
