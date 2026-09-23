open Devkit

let apps =
  [ Dashboard.{ name = "Git.Git"; version = "2.47.1"; pm = "winget" }
  ; Dashboard.{ name = "Brave.Brave"; version = ""; pm = "winget" }
  ; Dashboard.{ name = "ruff"; version = "0.16.8"; pm = "uv" }
  ]
;;

let json () = Winget_json.to_json ~now:0.0 apps

let member name = function
  | `Assoc kvs -> List.assoc_opt name kvs
  | _ -> None
;;

let shape () =
  let j = json () in
  Alcotest.(check (option string))
    "$schema"
    (Some "https://aka.ms/winget-packages.schema.2.0.json")
    (match member "$schema" j with
     | Some (`String s) -> Some s
     | _ -> None);
  Alcotest.(check (option string))
    "creation date"
    (Some "1970-01-01T00:00:00.000-00:00")
    (match member "CreationDate" j with
     | Some (`String s) -> Some s
     | _ -> None);
  let src =
    match member "Sources" j with
    | Some (`List [ s ]) -> s
    | _ -> `Assoc []
  in
  let det =
    match member "SourceDetails" src with
    | Some d -> d
    | None -> `Assoc []
  in
  Alcotest.(check (option string))
    "source name"
    (Some "winget")
    (match member "Name" det with
     | Some (`String s) -> Some s
     | _ -> None);
  let pkgs =
    match member "Packages" src with
    | Some (`List l) -> l
    | _ -> []
  in
  Alcotest.(check int) "winget only" 2 (List.length pkgs);
  let ids =
    List.filter_map
      (fun p ->
         match member "PackageIdentifier" p with
         | Some (`String s) -> Some s
         | _ -> None)
      pkgs
  in
  Alcotest.(check (list string)) "ids" [ "Git.Git"; "Brave.Brave" ] ids;
  let vers =
    List.filter_map
      (fun p ->
         match member "Version" p with
         | Some (`String s) -> Some s
         | _ -> None)
      pkgs
  in
  Alcotest.(check (list string)) "empty version omitted" [ "2.47.1" ] vers
;;

let string_ends_newline () =
  let s = Winget_json.to_string ~now:0.0 apps in
  Alcotest.(check bool)
    "trailing newline"
    true
    (String.length s > 0 && s.[String.length s - 1] = '\n')
;;

let () =
  Alcotest.run
    "winget_json"
    [ ( "export"
      , [ Alcotest.test_case "schema shape" `Quick shape
        ; Alcotest.test_case "string" `Quick string_ends_newline
        ] )
    ]
;;
