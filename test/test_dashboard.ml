(** Tests for {!Dashboard}: section merge and plain-text render. *)

open Devkit
open Pkgfile

let item typ value = make_item typ value

let winget_info rows =
  List.fold_left
    (fun acc (id, name, version, available) ->
       Winget_parse.IdMap.add
         (String.lowercase_ascii id)
         Winget_parse.{ id; name; version; available }
         acc)
    Winget_parse.IdMap.empty
    rows
;;

let merge_basic () =
  let manifest =
    [ { name = "Runtimes"
      ; items =
          [ item Winget "Git.Git"; item Winget "Missing.App"; item GitHub "owner/tool" ]
      }
    ]
  in
  let apps = [ Dashboard.{ name = "Git.Git"; version = "2.47.1"; pm = "winget" } ] in
  let info =
    winget_info [ "Git.Git", "Git", "2.47.1", "2.48.0"; "Other.App", "Other", "1.0", "" ]
  in
  let sections = Dashboard.build_sections apps manifest (Some info) in
  Alcotest.(check int) "three sections" 3 (List.length sections);
  let pending = List.nth sections 0 in
  Alcotest.(check string) "pending first" "Pending updates" pending.name;
  Alcotest.(check int) "one update" 1 (List.length pending.items);
  let git = List.nth pending.items 0 in
  Alcotest.(check string) "git value" "Git.Git" git.value;
  Alcotest.(check string) "installed" "2.47.1" git.installed_version;
  Alcotest.(check string) "available" "2.48.0" git.available_version;
  (match git.status with
   | NeedsUpdate -> ()
   | _ -> Alcotest.fail "git should need update");
  let detected = List.nth sections 1 in
  Alcotest.(check string) "newly second" "Newly detected" detected.name;
  Alcotest.(check int) "one new" 1 (List.length detected.items);
  (match (List.nth detected.items 0).status with
   | New -> ()
   | _ -> Alcotest.fail "other should be New");
  let rest = List.nth sections 2 in
  Alcotest.(check string) "manifest last" "Runtimes" rest.name;
  Alcotest.(check int) "two left" 2 (List.length rest.items);
  (match (List.nth rest.items 0).status with
   | NotFound -> ()
   | _ -> Alcotest.fail "missing should be NotFound");
  match (List.nth rest.items 1).status with
  | Manual -> ()
  | _ -> Alcotest.fail "github should be Manual"
;;

let winget_artifact_dropped () =
  let manifest = [ { name = "winget"; items = [ item Winget "Foo.Bar" ] } ] in
  let sections = Dashboard.build_sections [] manifest None in
  Alcotest.(check int) "dropped" 0 (List.length sections)
;;

let show_fallback () =
  (* show only fires for NotFound items (scan miss), so it enriches the
     displayed available version without promoting to Pending updates,
     exactly like the Go show-results loop. *)
  let manifest = [ { name = "Runtimes"; items = [ item Winget "Git.Git" ] } ] in
  let show = function
    | "Git.Git" -> Some "2.48.0"
    | _ -> None
  in
  let sections = Dashboard.build_sections ~show [] manifest None in
  Alcotest.(check int) "no pending" 1 (List.length sections);
  let sec = List.nth sections 0 in
  Alcotest.(check string) "manifest kept" "Runtimes" sec.name;
  let it = List.nth sec.items 0 in
  Alcotest.(check string) "available enriched" "2.48.0" it.available_version;
  match it.status with
  | NotFound -> ()
  | _ -> Alcotest.fail "stays NotFound"
;;

let render_fixture () =
  let sections =
    [ { name = "Pending updates"
      ; items =
          [ { (item Winget "Git.Git") with
              installed_version = "2.47.1"
            ; available_version = "2.48.0"
            ; status = NeedsUpdate
            }
          ]
      }
    ; { name = "Newly detected"
      ; items = [ { (item Winget "Other.App") with status = New } ]
      }
    ; { name = "Runtimes"
      ; items =
          [ { (item Winget "Missing.App") with status = NotFound }
          ; { (item GitHub "owner/tool") with status = Manual }
          ]
      }
    ; { name = "Empty"; items = [] }
    ]
  in
  let expected =
    "DEVKIT\n"
    ^ "! update   + not installed   ~ manual\n"
    ^ "\n"
    ^ "PENDING UPDATES\n"
    ^ "  [!] Git.Git 2.47.1 -> 2.48.0\n"
    ^ "\n"
    ^ "RUNTIMES\n"
    ^ "  [+] Missing.App\n"
    ^ "  [~] owner/tool\n"
    ^ "\n"
  in
  Alcotest.(check string) "exact render" expected (Dashboard.render sections)
;;

let render_versions () =
  Alcotest.(check string)
    "installed only"
    " 1.2.3"
    (Dashboard.format_ver { (item Winget "x") with installed_version = "1.2.3" });
  Alcotest.(check string)
    "available only"
    " 2.0"
    (Dashboard.format_ver { (item Winget "x") with available_version = "2.0" });
  Alcotest.(check string) "neither" "" (Dashboard.format_ver (item Winget "x"))
;;

let () =
  Alcotest.run
    "dashboard"
    [ ( "merge"
      , [ Alcotest.test_case "basic" `Quick merge_basic
        ; Alcotest.test_case "winget artifact dropped" `Quick winget_artifact_dropped
        ; Alcotest.test_case "show fallback" `Quick show_fallback
        ] )
    ; ( "render"
      , [ Alcotest.test_case "exact fixture" `Quick render_fixture
        ; Alcotest.test_case "version suffixes" `Quick render_versions
        ] )
    ]
;;
