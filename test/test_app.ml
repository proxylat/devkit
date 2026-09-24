open Devkit

let winget_table =
  "Name   Id   Version   Available   Source\n\
   --------------------------------\n\
   Git    Git.Git   2.47.1   2.48.0   winget\n\
   Brave  Brave.Brave  1.80.122  winget\n"
;;

let fake_bio ?(show_out = "", false) () : Bootstrap.io =
  { getenv = (fun _ -> None)
  ; is_file = (fun _ -> false)
  ; spawn = (fun _ _ -> show_out)
  ; cwd = (fun () -> Error "no cwd")
  ; exe_dir = (fun () -> Error "no exe")
  ; mkdir_p = (fun _ -> Error "ro")
  ; cache_dir = (fun () -> ".")
  ; latest_winget_cli = (fun () -> Error "offline")
  ; download = (fun ~url:_ -> Error "offline")
  ; unzip = (fun ~zip:_ ~dir:_ -> Error "ro")
  }
;;

(** In-memory filesystem. *)
let mem_fs (files : (string, string) Hashtbl.t) : App.fs =
  { read_file = Hashtbl.find_opt files
  ; append_file =
      (fun path text ->
        let cur = Option.value ~default:"" (Hashtbl.find_opt files path) in
        Hashtbl.replace files path (cur ^ text);
        Ok ())
  ; write_file =
      (fun path text ->
        Hashtbl.replace files path text;
        Ok ())
  }
;;

let scan_run prog _args =
  match prog with
  | "winget" -> Some winget_table
  | _ -> None
;;

let env files = { App.run = scan_run; fs = mem_fs files; bio = fake_bio () }

let contains sub s =
  try
    ignore (Str.search_forward (Str.regexp_string sub) s 0);
    true
  with
  | Not_found -> false
;;

let load_missing () =
  let s, w = App.load_manifest (mem_fs (Hashtbl.create 1)) Manifest.filename in
  Alcotest.(check int) "no sections" 0 (List.length s);
  Alcotest.(check string) "no winget path" "" w
;;

let load_parses () =
  let files = Hashtbl.create 1 in
  Hashtbl.add
    files
    Manifest.filename
    "winget_path = \"C:\\\\w\\\\winget.exe\"\n\n\
     [[section]]\n\
     name = \"tools\"\n\n\
     [[section.package]]\n\
     pm = \"winget\"\n\
     id = \"Git.Git\"\n";
  let s, w = App.load_manifest (mem_fs files) Manifest.filename in
  Alcotest.(check string) "winget path" "C:\\w\\winget.exe" w;
  Alcotest.(check int) "one section" 1 (List.length s)
;;

let append_groups () =
  let files = Hashtbl.create 1 in
  let items =
    [ { Manifest.typ = Manifest.Winget
      ; value = "Brave.Brave"
      ; installed_version = ""
      ; available_version = ""
      ; status = Manifest.New
      }
    ; { Manifest.typ = Manifest.Winget
      ; value = "Git.Git"
      ; installed_version = ""
      ; available_version = ""
      ; status = Manifest.New
      }
    ]
  in
  Alcotest.(check (result unit string))
    "ok"
    (Ok ())
    (App.append_selected (mem_fs files) Manifest.filename items);
  let written = Hashtbl.find files Manifest.filename in
  Alcotest.(check bool) "section header" true (contains "Newly detected" written);
  Alcotest.(check bool)
    "both ids"
    true
    (contains "Brave.Brave" written && contains "Git.Git" written)
;;

let append_empty () =
  let files = Hashtbl.create 1 in
  Alcotest.(check (result unit string))
    "ok"
    (Ok ())
    (App.append_selected (mem_fs files) Manifest.filename []);
  Alcotest.(check bool) "no write" false (Hashtbl.mem files Manifest.filename)
;;

let add () =
  let files = Hashtbl.create 1 in
  Hashtbl.add
    files
    Manifest.filename
    "[[section]]\n\
     name = \"tools\"\n\n\
     [[section.package]]\n\
     pm = \"winget\"\n\
     id = \"Git.Git\"\n";
  let msgs = App.run_add (env files) [ "Git.Git"; "Nope.Nope"; "Brave.Brave" ] in
  Alcotest.(check (list string))
    "messages"
    [ "  skip (already in manifest): Git.Git"
    ; "  skip (not installed): Nope.Nope"
    ; "  appended 1 app(s) to " ^ Manifest.filename
    ]
    msgs;
  Alcotest.(check bool)
    "brave persisted"
    true
    (contains "Brave.Brave" (Hashtbl.find files Manifest.filename))
;;

let add_nothing () =
  let files = Hashtbl.create 1 in
  let msgs = App.run_add (env files) [ "Nope.Nope" ] in
  Alcotest.(check (list string))
    "nothing"
    [ "  skip (not installed): Nope.Nope"; "  nothing to append" ]
    msgs
;;

let export () =
  let files = Hashtbl.create 1 in
  let msgs = App.run_export (env files) "out.toml" in
  Alcotest.(check (list string))
    "messages"
    [ "wrote out.toml (2 apps)"
    ; "wrote out.json (2 winget apps, winget import -i ready)"
    ]
    msgs;
  let r = Devkit.Manifest.parse (Hashtbl.find files "out.toml") in
  let ids =
    List.concat_map
      (fun (sec : Devkit.Manifest.section) ->
         List.map (fun i -> i.Devkit.Manifest.value) sec.items)
      r.sections
  in
  Alcotest.(check (list string)) "exported ids" [ "Brave.Brave"; "Git.Git" ] ids;
  let j = Yojson.Basic.from_string (Hashtbl.find files "out.json") in
  let pkgs =
    match j with
    | `Assoc kvs ->
      (match List.assoc "Sources" kvs with
       | `List [ `Assoc src ] ->
         (match List.assoc "Packages" src with
          | `List l -> l
          | _ -> [])
       | _ -> [])
    | _ -> []
  in
  Alcotest.(check int) "json packages" 2 (List.length pkgs)
;;

let export_empty () =
  let files = Hashtbl.create 1 in
  let e = { (env files) with run = (fun _ _ -> None) } in
  Alcotest.(check (list string))
    "empty"
    [ "  no installed software found" ]
    (App.run_export e "out.toml")
;;

let export_no_winget () =
  let files = Hashtbl.create 1 in
  let e =
    { (env files) with
      run = (fun prog _ -> if prog = "npm" then Some "x@1.0.0\n" else None)
    }
  in
  let msgs = App.run_export e "out.toml" in
  Alcotest.(check bool)
    "skip message"
    true
    (List.mem "  no winget apps, json skipped" msgs);
  Alcotest.(check bool) "no json file" false (Hashtbl.mem files "out.json")
;;

let new_items () =
  let mk status =
    { Manifest.typ = Manifest.Winget
    ; value = "x"
    ; installed_version = ""
    ; available_version = ""
    ; status
    }
  in
  let secs =
    [ { Manifest.name = "Newly detected"; items = [ mk Manifest.New ] }
    ; { Manifest.name = "Pending updates"; items = [ mk Manifest.New ] }
    ]
  in
  Alcotest.(check int) "newly only" 1 (List.length (App.new_items secs));
  Alcotest.(check int) "with pending" 2 (List.length (App.new_items ~extra:true secs))
;;

let show () =
  let bio = fake_bio ~show_out:("Version: 9.9\n", true) () in
  Alcotest.(check string) "version" "9.9" (App.winget_show bio "w" "Some.Id");
  let bad = fake_bio () in
  Alcotest.(check string) "failure" "" (App.winget_show bad "w" "Some.Id");
  Alcotest.(check string) "no winget" "" (App.winget_show bad "" "Some.Id")
;;

let default () =
  let files = Hashtbl.create 1 in
  Hashtbl.add
    files
    Manifest.filename
    "[[section]]\n\
     name = \"tools\"\n\n\
     [[section.package]]\n\
     pm = \"winget\"\n\
     id = \"Git.Git\"\n";
  let s = App.default_view ~tools:[] (env files) in
  Alcotest.(check bool) "mentions app" true (contains "Git.Git" s)
;;

let () =
  Alcotest.run
    "app"
    [ ( "manifest"
      , [ Alcotest.test_case "missing" `Quick load_missing
        ; Alcotest.test_case "parses" `Quick load_parses
        ] )
    ; ( "append"
      , [ Alcotest.test_case "groups" `Quick append_groups
        ; Alcotest.test_case "empty" `Quick append_empty
        ; Alcotest.test_case "new_items" `Quick new_items
        ] )
    ; ( "commands"
      , [ Alcotest.test_case "add" `Quick add
        ; Alcotest.test_case "add_nothing" `Quick add_nothing
        ; Alcotest.test_case "export" `Quick export
        ; Alcotest.test_case "export_empty" `Quick export_empty
        ; Alcotest.test_case "export_no_winget" `Quick export_no_winget
        ; Alcotest.test_case "show" `Quick show
        ; Alcotest.test_case "default" `Quick default
        ] )
    ]
;;
