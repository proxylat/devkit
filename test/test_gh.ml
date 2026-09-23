(** Tests for {!Devkit.Gh}: JSON parsing, arch matching. *)
open Devkit

let release_json =
  {|{
    "tag_name": "v2.1.0",
    "assets": [
      {"name": "tool-linux.tar.gz", "browser_download_url": "https://x/lin", "size": 10},
      {"name": "Tool-win-x64.exe", "browser_download_url": "https://x/win", "size": 20},
      {"name": "tool.msi", "browser_download_url": "https://x/msi"}
    ]
  }|}
;;

let test_parse () =
  match Gh.parse_release release_json with
  | Error e -> Alcotest.fail ("parse: " ^ e)
  | Ok rel ->
    Alcotest.(check string) "tag" "v2.1.0" rel.Gh.tag_name;
    Alcotest.(check int) "assets" 3 (List.length rel.Gh.assets);
    let msi = List.nth rel.Gh.assets 2 in
    Alcotest.(check string) "msi url" "https://x/msi" msi.Gh.browser_download_url;
    Alcotest.(check int64) "missing size defaults 0" 0L msi.Gh.size
;;

let test_parse_bad () =
  match Gh.parse_release "{oops" with
  | Ok _ -> Alcotest.fail "expected decode error"
  | Error e ->
    Alcotest.(check bool) "decode prefix" true (Strutil.contains_substring "decode" e)
;;

let asset name = { Gh.name; browser_download_url = "https://x/" ^ name; size = 1L }

let test_prefers_arch_win () =
  let assets = [ asset "tool-linux-x64.tar.gz"; asset "Tool-win-x64.exe" ] in
  match Gh.match_by_arch assets with
  | None -> Alcotest.fail "expected a hit"
  | Some a -> Alcotest.(check string) "arch windows exe" "Tool-win-x64.exe" a.Gh.name
;;

let test_falls_back_plain_exe () =
  let assets = [ asset "tool-linux.tar.gz"; asset "setup.exe" ] in
  match Gh.match_by_arch assets with
  | None -> Alcotest.fail "expected fallback hit"
  | Some a -> Alcotest.(check string) "fallback exe" "setup.exe" a.Gh.name
;;

let test_case_insensitive () =
  match Gh.match_by_arch [ asset "APP-WIN-AMD64.MSI" ] with
  | None -> Alcotest.fail "expected hit"
  | Some a -> Alcotest.(check string) "name" "APP-WIN-AMD64.MSI" a.Gh.name
;;

let test_no_windows () =
  Alcotest.(check bool)
    "no windows asset"
    true
    (Gh.match_by_arch [ asset "tool-linux.tar.gz"; asset "tool-mac.dmg" ] = None)
;;

let () =
  Alcotest.run
    "gh"
    [ ( "parse"
      , [ Alcotest.test_case "release json" `Quick test_parse
        ; Alcotest.test_case "bad json errors" `Quick test_parse_bad
        ] )
    ; ( "match_by_arch"
      , [ Alcotest.test_case "prefers arch windows exe" `Quick test_prefers_arch_win
        ; Alcotest.test_case "falls back to plain exe" `Quick test_falls_back_plain_exe
        ; Alcotest.test_case "case insensitive" `Quick test_case_insensitive
        ; Alcotest.test_case "no windows asset" `Quick test_no_windows
        ] )
    ]
;;
