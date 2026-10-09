(** Tests for {!Devkit.Winget_manifest}: dir URLs, version listing, host scan. *)
open Devkit

let fake_fetch (table : (string * (string, string) result) list) : Fetch.fetch =
  fun ?timeout_s:_ url _headers ->
  match List.assoc_opt url table with
  | Some r -> r
  | None -> Error ("unexpected url: " ^ url)
;;

let test_dir_url () =
  Alcotest.(check string)
    "git.git"
    "https://api.github.com/repos/microsoft/winget-pkgs/contents/manifests/g/Git/Git"
    (Winget_manifest.dir_url "Git.Git");
  Alcotest.(check string)
    "multi-dot lowercases first letter"
    "https://api.github.com/repos/microsoft/winget-pkgs/contents/manifests/m/Microsoft/VisualStudio/Code"
    (Winget_manifest.dir_url "Microsoft.VisualStudio.Code")
;;

let versions_json =
  {|[
    {"name": "2.10.0", "type": "dir"},
    {"name": "Git.Git.yaml", "type": "file"},
    {"name": "", "type": "dir"},
    {"name": "2.9.0", "type": "dir"}
  ]|}
;;

let test_list_versions () =
  let url = Winget_manifest.dir_url "Git.Git" in
  let fetch = fake_fetch [ url, Ok versions_json ] in
  match Winget_manifest.list_versions fetch "Git.Git" with
  | Error e -> Alcotest.fail ("list_versions: " ^ e)
  | Ok vs -> Alcotest.(check (list string)) "dirs only, in order" [ "2.10.0"; "2.9.0" ] vs
;;

let test_list_versions_error () =
  let fetch = fake_fetch [ Winget_manifest.dir_url "Git.Git", Error "boom" ] in
  match Winget_manifest.list_versions fetch "Git.Git" with
  | Ok _ -> Alcotest.fail "expected fetch error to propagate"
  | Error e -> Alcotest.(check string) "propagates" "boom" e
;;

let test_pick_max () =
  Alcotest.(check (option string))
    "numeric segments"
    (Some "2.10.0")
    (Winget_manifest.pick_max [ "2.9.0"; "2.10.0"; "2.8.1" ])
;;

let test_pick_max_empty () =
  Alcotest.(check (option string)) "empty" None (Winget_manifest.pick_max [])
;;

let installer_yaml =
  {|PackageIdentifier: Git.Git
InstallerUrl: https://github.com/git-for-windows/git/releases/a.exe
installerurl: https://GITHUB.com/other/b.exe
InstallerUrl: https://artifacts.example.com/c.exe
NotAUrl: hello
InstallerUrl: not a url
Garbage line without colon
InstallerUrl:
|}
;;

let test_installer_hosts () =
  Alcotest.(check (list string))
    "sorted uniq lowercase"
    [ "artifacts.example.com"; "github.com" ]
    (Winget_manifest.installer_hosts installer_yaml)
;;

let test_installer_hosts_none () =
  Alcotest.(check (list string))
    "no urls"
    []
    (Winget_manifest.installer_hosts "Foo: bar\nno colon here\n")
;;

let hosts_dir_json =
  {|[
    {"name": "zzz.installer.yaml", "type": "file", "download_url": "https://raw/z.yaml"},
    {"name": "Git.Git.yaml", "type": "file", "download_url": "https://raw/other.yaml"},
    {"name": "Git.Git.installer.yaml", "type": "file", "download_url": "https://raw/g.yaml"}
  ]|}
;;

let g_yaml =
  "InstallerUrl: https://b.example/x.exe\nInstallerUrl: https://a.example/y.exe\n"
;;

let test_hosts_of () =
  let dir = Winget_manifest.dir_url "Git.Git" ^ "/2.10.0" in
  let fetch = fake_fetch [ dir, Ok hosts_dir_json; "https://raw/g.yaml", Ok g_yaml ] in
  (* "https://raw/z.yaml" is deliberately unmapped: picking the wrong yaml
     surfaces as an "unexpected url" fetch error and fails the test. *)
  match Winget_manifest.hosts_of fetch "Git.Git" "2.10.0" with
  | Error e -> Alcotest.fail ("hosts_of: " ^ e)
  | Ok hosts ->
    Alcotest.(check (list string))
      "first sorted yaml wins"
      [ "a.example"; "b.example" ]
      hosts
;;

let test_hosts_of_missing () =
  let dir = Winget_manifest.dir_url "Git.Git" ^ "/9.9.9" in
  let body =
    {|[{"name": "Git.Git.yaml", "type": "file", "download_url": "https://raw/x"}]|}
  in
  let fetch = fake_fetch [ dir, Ok body ] in
  match Winget_manifest.hosts_of fetch "Git.Git" "9.9.9" with
  | Ok _ -> Alcotest.fail "expected missing-yaml error"
  | Error e ->
    Alcotest.(check bool)
      "mentions installer.yaml"
      true
      (Strutil.contains_substring "installer.yaml" e)
;;

let () =
  Alcotest.run
    "winget_manifest"
    [ "dir_url", [ Alcotest.test_case "shape" `Quick test_dir_url ]
    ; ( "list_versions"
      , [ Alcotest.test_case "dirs only" `Quick test_list_versions
        ; Alcotest.test_case "fetch error propagates" `Quick test_list_versions_error
        ] )
    ; ( "pick_max"
      , [ Alcotest.test_case "picks highest" `Quick test_pick_max
        ; Alcotest.test_case "empty is none" `Quick test_pick_max_empty
        ] )
    ; ( "installer_hosts"
      , [ Alcotest.test_case "sorted uniq" `Quick test_installer_hosts
        ; Alcotest.test_case "no urls" `Quick test_installer_hosts_none
        ] )
    ; ( "hosts_of"
      , [ Alcotest.test_case "end to end" `Quick test_hosts_of
        ; Alcotest.test_case "missing yaml errors" `Quick test_hosts_of_missing
        ] )
    ]
;;
