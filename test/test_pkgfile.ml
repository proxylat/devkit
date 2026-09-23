(** Tests for {!Pkgfile}: manifest parsing. *)

open Devkit.Pkgfile

let status_t =
  Alcotest.testable
    (fun ppf -> function
       | Installed -> Format.pp_print_string ppf "Installed"
       | NeedsUpdate -> Format.pp_print_string ppf "NeedsUpdate"
       | NotFound -> Format.pp_print_string ppf "NotFound"
       | New -> Format.pp_print_string ppf "New"
       | Manual -> Format.pp_print_string ppf "Manual")
    ( = )
;;

let type_t =
  Alcotest.testable
    (fun ppf -> function
       | Winget -> Format.pp_print_string ppf "Winget"
       | GitHub -> Format.pp_print_string ppf "GitHub"
       | Url -> Format.pp_print_string ppf "Url")
    ( = )
;;

let sections () =
  let r =
    parse
      "# Runtimes\n\
       winget:Microsoft.DotNet.SDK.9\n\n\
       github:owner/repo\n\
       # Setup\n\
       url:https://example.com/x.exe\n"
  in
  Alcotest.(check string) "winget path empty" "" r.winget_path;
  Alcotest.(check int) "two sections" 2 (List.length r.sections);
  let runtimes = List.nth r.sections 0 in
  Alcotest.(check string) "first section" "Runtimes" runtimes.name;
  Alcotest.(check int) "runtimes items" 2 (List.length runtimes.items);
  Alcotest.(check type_t) "winget type" Winget (List.nth runtimes.items 0).typ;
  Alcotest.(check string)
    "winget value"
    "Microsoft.DotNet.SDK.9"
    (List.nth runtimes.items 0).value;
  Alcotest.(check type_t) "github type" GitHub (List.nth runtimes.items 1).typ;
  let setup = List.nth r.sections 1 in
  Alcotest.(check string) "second section" "Setup" setup.name;
  Alcotest.(check type_t) "url type" Url (List.nth setup.items 0).typ
;;

let directive () =
  let r = parse "# winget: C:\\tools\\winget.exe\n# Runtimes\nwinget:Foo.Bar\n" in
  Alcotest.(check string) "winget path" "C:\\tools\\winget.exe" r.winget_path;
  Alcotest.(check int) "directive is not a section" 1 (List.length r.sections)
;;

let directive_case_insensitive () =
  let r = parse "# Winget:  /usr/bin/winget\nwinget:Foo.Bar\n" in
  Alcotest.(check string) "path" "/usr/bin/winget" r.winget_path;
  Alcotest.(check string) "implicit General" "General" (List.nth r.sections 0).name
;;

let bare_winget_header_is_section () =
  (* "# winget" without a path becomes a section named "winget", later
     dropped by the dashboard merge. *)
  let r = parse "# winget\nwinget:Foo.Bar\n" in
  Alcotest.(check string) "no path" "" r.winget_path;
  Alcotest.(check string) "section winget" "winget" (List.nth r.sections 0).name
;;

let general_fallback () =
  let r = parse "winget:Foo.Bar\n" in
  Alcotest.(check int) "one section" 1 (List.length r.sections);
  Alcotest.(check string) "General" "General" (List.nth r.sections 0).name
;;

let skipped_lines () =
  let r =
    parse "# Sec\nno-colon-here\nunknown:val\nwinget:\n   \nwinget:  \nwinget:Real.Id\n"
  in
  let sec = List.nth r.sections 0 in
  Alcotest.(check int) "only the valid item" 1 (List.length sec.items);
  Alcotest.(check string) "value" "Real.Id" (List.nth sec.items 0).value
;;

let empty_comment_no_section () =
  let r = parse "#\n#   \nwinget:Foo.Bar\n" in
  Alcotest.(check string) "General" "General" (List.nth r.sections 0).name
;;

let fresh_item_status () =
  (* Fresh items carry NotFound until the merge assigns. *)
  let r = parse "winget:Foo.Bar\n" in
  Alcotest.(check status_t)
    "zero status"
    NotFound
    (List.nth (List.nth r.sections 0).items 0).status
;;

let () =
  Alcotest.run
    "pkgfile"
    [ ( "parse"
      , [ Alcotest.test_case "sections and items" `Quick sections
        ; Alcotest.test_case "winget directive" `Quick directive
        ; Alcotest.test_case
            "directive case-insensitive"
            `Quick
            directive_case_insensitive
        ; Alcotest.test_case "bare winget header" `Quick bare_winget_header_is_section
        ; Alcotest.test_case "General fallback" `Quick general_fallback
        ; Alcotest.test_case "skipped lines" `Quick skipped_lines
        ; Alcotest.test_case "empty comments" `Quick empty_comment_no_section
        ; Alcotest.test_case "fresh item status" `Quick fresh_item_status
        ] )
    ]
;;
