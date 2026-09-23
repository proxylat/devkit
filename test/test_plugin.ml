(** Tests for {!Devkit.Plugin}: row parsing, template expansion, s-expr
    loading, multi-file merge, lookup, and {!Devkit.Inventory.scan_tool}. *)
open Devkit

let app_triple = Alcotest.triple Alcotest.string Alcotest.string Alcotest.string

let proj (apps : Dashboard.app list) : (string * string * string) list =
  List.map
    (fun (a : Dashboard.app) -> a.Dashboard.name, a.Dashboard.version, a.Dashboard.pm)
    apps
;;

let spec =
  { Plugin.skip_prefixes = [ "- " ]
  ; skip_res = [ Re.compile (Re.Pcre.re "^WARN") ]
  ; row_re = Re.compile (Re.Pcre.re "^(\\S+)\\s+v?([0-9][^ ]*)")
  ; strip_version_prefix = "v"
  }
;;

let rows () =
  let apps =
    Plugin.parse_rows
      spec
      "my"
      "ruff v0.16.8\n- ruff\n\nWARN noisy\nblack 24.10.0\ngarbage line\n"
  in
  Alcotest.(check (list app_triple))
    "skips + strips"
    [ "ruff", "0.16.8", "my"; "black", "24.10.0", "my" ]
    (proj apps)
;;

let unversioned () =
  let opt =
    { spec with Plugin.row_re = Re.compile (Re.Pcre.re "^(\\S+)(?:\\s+([0-9][^ ]*))?") }
  in
  Alcotest.(check (list app_triple))
    "missing group 2"
    [ "noversion", "", "my" ]
    (proj (Plugin.parse_rows opt "my" "noversion\n"))
;;

let expand_ok () =
  Alcotest.(check (result (list string) string))
    "subst"
    (Ok [ "mytool"; "install"; "Foo.Bar" ])
    (Plugin.expand [ "id", "Foo.Bar" ] [ "mytool"; "install"; "{id}" ])
;;

let expand_plain () =
  Alcotest.(check (result (list string) string))
    "no braces"
    (Ok [ "a"; "b" ])
    (Plugin.expand [] [ "a"; "b" ])
;;

let expand_unknown () =
  match Plugin.expand [ "id", "x" ] [ "{ver}" ] with
  | Ok _ -> Alcotest.fail "expected unknown-placeholder error"
  | Error _ -> ()
;;

let expand_unclosed () =
  match Plugin.expand [] [ "foo{id" ] with
  | Ok _ -> Alcotest.fail "expected unclosed-brace error"
  | Error _ -> ()
;;

let basic_file =
  "(tools (tool (name mytool) (prog mytool) (list --list --short) (skip_prefixes (\"- \
   \")) (skip_res (\"^WARN\")) (row_re \"^(\\\\S+)\\\\s+v?([0-9][^ ]*)\") \
   (strip_version_prefix v) (install (mytool install {id})) (upgrade (mytool upgrade \
   {id}))))"
;;

let load_basic () =
  match Plugin.load_string ~source:"t" basic_file with
  | Error e -> Alcotest.fail e
  | Ok [ t ] ->
    Alcotest.(check string) "name" "mytool" t.Plugin.name;
    Alcotest.(check string) "prog" "mytool" t.Plugin.prog;
    Alcotest.(check (list string)) "list" [ "--list"; "--short" ] t.Plugin.list_args;
    Alcotest.(check (option (list string)))
      "install"
      (Some [ "mytool"; "install"; "{id}" ])
      t.Plugin.install;
    Alcotest.(check (option (list string)))
      "upgrade"
      (Some [ "mytool"; "upgrade"; "{id}" ])
      t.Plugin.upgrade;
    Alcotest.(check (list app_triple))
      "parse"
      [ "mytool", "1.2", "mytool" ]
      (proj (t.Plugin.parse "mytool v1.2\n"))
  | Ok _ -> Alcotest.fail "expected exactly one tool"
;;

let load_minimal () =
  match
    Plugin.load_string ~source:"t" "(tools (tool (name n) (prog p) (row_re \"x\")))"
  with
  | Error e -> Alcotest.fail e
  | Ok [ t ] ->
    Alcotest.(check (list string)) "no list" [] t.Plugin.list_args;
    Alcotest.(check (option (list string))) "no install" None t.Plugin.install;
    Alcotest.(check (option (list string))) "no upgrade" None t.Plugin.upgrade
  | Ok _ -> Alcotest.fail "expected exactly one tool"
;;

let load_errors () =
  let bad =
    [ "(tool (name n))", "wrapper"
    ; "(tools (tool (prog p) (row_re \"x\")))", "name"
    ; "(tools (tool (name n) (prog p)))", "row_re"
    ; "(tools (tool (name n) (prog p) (row_re \"x\") (bogus 1)))", "unknown field"
    ; "(tools (tool (name n) (prog p) (row_re \"(\")))", "bad regex"
    ; "(tools (tool (name n) (prog p) (row_re \"x\" \"y\")))", "single value"
    ; "(oops", "parse error"
    ]
  in
  List.iter
    (fun (text, what) ->
       match Plugin.load_string ~source:"t" text with
       | Ok _ -> Alcotest.fail ("expected error: " ^ what)
       | Error _ -> ())
    bad
;;

let load_two () =
  match
    Plugin.load_string
      ~source:"t"
      "(tools (tool (name a) (prog a) (row_re \"x\")) (tool (name b) (prog b) (row_re \
       \"y\")))"
  with
  | Error e -> Alcotest.fail e
  | Ok ts ->
    Alcotest.(check (list string))
      "order"
      [ "a"; "b" ]
      (List.map (fun t -> t.Plugin.name) ts)
;;

let file_absent () =
  let names r = List.map (fun t -> t.Plugin.name) r in
  Alcotest.(check (result (list string) string))
    "absent"
    (Ok [])
    (Result.map names (Plugin.load_file ~read:(fun _ -> None) "nope"))
;;

let file_broken () =
  match Plugin.load_file ~read:(fun _ -> Some "(oops") "bad.sexp" with
  | Ok _ -> Alcotest.fail "expected error"
  | Error _ -> ()
;;

let one_tool name =
  "(tools (tool (name " ^ name ^ ") (prog " ^ name ^ ") (row_re \"x\")))"
;;

let merge_first_wins () =
  let read = function
    | "a.sexp" -> Some (one_tool "aa")
    | "b.sexp" ->
      Some
        "(tools (tool (name aa) (prog OTHER) (row_re \"x\")) (tool (name bb) (prog bb) \
         (row_re \"x\")))"
    | _ -> None
  in
  match Plugin.load_many ~read [ "a.sexp"; "b.sexp"; "missing.sexp" ] with
  | Error e -> Alcotest.fail e
  | Ok ts ->
    Alcotest.(check (list (pair string string)))
      "first wins, order kept"
      [ "aa", "aa"; "bb", "bb" ]
      (List.map (fun t -> t.Plugin.name, t.Plugin.prog) ts)
;;

let merge_reserved () =
  let read _ = Some (one_tool "npm") in
  match Plugin.load_many ~read ~reserved:[ "npm"; "winget" ] [ "a.sexp" ] with
  | Ok _ -> Alcotest.fail "expected reserved-name error"
  | Error _ -> ()
;;

let find_case () =
  match Plugin.load_string ~source:"t" (one_tool "MyTool") with
  | Error e -> Alcotest.fail e
  | Ok ts ->
    Alcotest.(check bool) "case-insensitive" true (Plugin.find "mytool" ts <> None);
    Alcotest.(check bool) "missing" true (Plugin.find "zz" ts = None)
;;

let scan_tool () =
  match Plugin.load_string ~source:"t" basic_file with
  | Error e -> Alcotest.fail e
  | Ok [ t ] ->
    Alcotest.(check (list app_triple))
      "runs list args"
      [ "mytool", "1.2", "mytool" ]
      (proj (Inventory.scan_tool (fun _ _ -> Some "mytool v1.2\n") t));
    Alcotest.(check (list app_triple))
      "missing binary skipped"
      []
      (proj (Inventory.scan_tool (fun _ _ -> None) t))
  | Ok _ -> Alcotest.fail "expected exactly one tool"
;;

let () =
  Alcotest.run
    "plugin"
    [ ( "rows"
      , [ Alcotest.test_case "skips" `Quick rows
        ; Alcotest.test_case "unversioned" `Quick unversioned
        ] )
    ; ( "expand"
      , [ Alcotest.test_case "subst" `Quick expand_ok
        ; Alcotest.test_case "plain" `Quick expand_plain
        ; Alcotest.test_case "unknown" `Quick expand_unknown
        ; Alcotest.test_case "unclosed" `Quick expand_unclosed
        ] )
    ; ( "load"
      , [ Alcotest.test_case "basic" `Quick load_basic
        ; Alcotest.test_case "minimal" `Quick load_minimal
        ; Alcotest.test_case "errors" `Quick load_errors
        ; Alcotest.test_case "two tools" `Quick load_two
        ] )
    ; ( "files"
      , [ Alcotest.test_case "absent" `Quick file_absent
        ; Alcotest.test_case "broken" `Quick file_broken
        ; Alcotest.test_case "first wins" `Quick merge_first_wins
        ; Alcotest.test_case "reserved" `Quick merge_reserved
        ; Alcotest.test_case "find" `Quick find_case
        ; Alcotest.test_case "scan_tool" `Quick scan_tool
        ] )
    ]
;;
