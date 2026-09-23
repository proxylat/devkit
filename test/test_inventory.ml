(** Tests for {!Inventory} and {!Proc}: per-PM parsers, full scan wiring,
    winget memoization. *)

open Devkit

let apps_t =
  Alcotest.testable
    (fun ppf (a : Dashboard.app) -> Format.fprintf ppf "%s %s %s" a.pm a.name a.version)
    ( = )
;;

let check_apps = Alcotest.(check (list apps_t))

let npm () =
  check_apps
    "npm rows"
    [ Dashboard.{ name = "npm"; version = "10.9.0"; pm = "npm" }
    ; Dashboard.{ name = "typescript"; version = "5.4.5"; pm = "npm" }
    ; Dashboard.{ name = "@scope/pkg"; version = "1.2.3"; pm = "npm" }
    ]
    (Inventory.parse_npm
       "/usr/lib/node_modules\n\
        ├── npm@10.9.0\n\
        └── typescript@5.4.5\n\
        ├── @scope/pkg@1.2.3\n")
;;

let pipx () =
  check_apps
    "pipx rows"
    [ Dashboard.{ name = "attrs"; version = "25.3.0"; pm = "pipx" }
    ; Dashboard.{ name = "black"; version = "24.10.0"; pm = "pipx" }
    ]
    (Inventory.parse_pipx "attrs 25.3.0, injected: flake8\nblack 24.10.0\n")
;;

let uv () =
  check_apps
    "uv rows"
    [ Dashboard.{ name = "ruff"; version = "0.16.8"; pm = "uv" }
    ; Dashboard.{ name = "py-spy"; version = "0.4.2"; pm = "uv" }
    ]
    (Inventory.parse_uv "ruff v0.16.8\n- ruff\npy-spy v0.4.2\n- py-spy\n")
;;

let uv_skips () =
  check_apps
    "uv shims and warnings skipped"
    [ Dashboard.{ name = "ruff"; version = "0.16.8"; pm = "uv" } ]
    (Inventory.parse_uv "ruff v0.16.8\n- ruff\nFailed to parse entry for bandit\n")
;;

let uv_empty () = check_apps "uv empty" [] (Inventory.parse_uv "")

let cargo () =
  check_apps
    "cargo rows"
    [ Dashboard.{ name = "ripgrep"; version = "14.1.0"; pm = "cargo" }
    ; Dashboard.{ name = "fd-find"; version = "10.2.0"; pm = "cargo" }
    ]
    (Inventory.parse_cargo "ripgrep v14.1.0:\n    rg\nfd-find v10.2.0:\n    fd\n")
;;

let winget_table =
  "Name   Id   Version   Available   Source\n\
   --------------------------------\n\
   Git    Git.Git   2.47.1   2.48.0   winget\n\
   Brave  Brave.Brave  1.80.122  winget\n"
;;

let winget () =
  check_apps
    "winget rows sorted by id"
    [ Dashboard.{ name = "Brave.Brave"; version = "1.80.122"; pm = "winget" }
    ; Dashboard.{ name = "Git.Git"; version = "2.47.1"; pm = "winget" }
    ]
    (Inventory.parse_winget winget_table)
;;

let scan () =
  let run prog _args =
    match prog with
    | "npm" -> None (* missing binary → skipped *)
    | "pipx" -> Some "black 24.10.0\n"
    | "uv" -> Some "ruff v0.16.8\n- ruff\n"
    | "cargo" -> Some "rg v14.1.0:\n"
    | _ -> None
  in
  let apps = Inventory.scan_all run ~winget:(fun () -> Some winget_table) ~extra:[] in
  check_apps
    "scan order and tags"
    [ Dashboard.{ name = "Brave.Brave"; version = "1.80.122"; pm = "winget" }
    ; Dashboard.{ name = "Git.Git"; version = "2.47.1"; pm = "winget" }
    ; Dashboard.{ name = "black"; version = "24.10.0"; pm = "pipx" }
    ; Dashboard.{ name = "ruff"; version = "0.16.8"; pm = "uv" }
    ; Dashboard.{ name = "rg"; version = "14.1.0"; pm = "cargo" }
    ]
    apps
;;

let memo () =
  let calls = ref 0 in
  let run _prog _args =
    incr calls;
    Some "out"
  in
  let fetch, reset = Proc.winget_list run in
  Alcotest.(check (option string)) "first" (Some "out") (fetch "");
  Alcotest.(check (option string)) "memoized" (Some "out") (fetch "");
  Alcotest.(check int) "one spawn" 1 !calls;
  reset ();
  Alcotest.(check (option string)) "after reset" (Some "out") (fetch "");
  Alcotest.(check int) "two spawns" 2 !calls
;;

let memo_failure () =
  let fetch, _ = Proc.winget_list (fun _ _ -> None) in
  Alcotest.(check (option string)) "missing winget" None (fetch "")
;;

let () =
  Alcotest.run
    "inventory"
    [ ( "parsers"
      , [ Alcotest.test_case "npm" `Quick npm
        ; Alcotest.test_case "pipx" `Quick pipx
        ; Alcotest.test_case "uv" `Quick uv
        ; Alcotest.test_case "uv skips" `Quick uv_skips
        ; Alcotest.test_case "uv empty" `Quick uv_empty
        ; Alcotest.test_case "cargo" `Quick cargo
        ; Alcotest.test_case "winget" `Quick winget
        ] )
    ; ( "scan"
      , [ Alcotest.test_case "scan_all" `Quick scan
        ; Alcotest.test_case "winget memo" `Quick memo
        ; Alcotest.test_case "winget missing" `Quick memo_failure
        ] )
    ]
;;
