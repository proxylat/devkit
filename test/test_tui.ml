open Devkit

let item status value =
  { Manifest.typ = Manifest.Winget
  ; value
  ; installed_version = "1.0"
  ; available_version = ""
  ; status
  ; upstream = ""
  }
;;

let sections () =
  [ { Manifest.name = "tools"
    ; items = [ item Manifest.Installed "a"; item Manifest.NeedsUpdate "b" ]
    }
  ; { Manifest.name = "Newly detected"; items = [ item Manifest.New "c" ] }
  ; { Manifest.name = "empty"; items = [] }
  ; { Manifest.name = "more"; items = [ item Manifest.Manual "https://x" ] }
  ]
;;

let build_skips () =
  let e = Tui.build_entries (sections ()) in
  Alcotest.(check (list string))
    "values"
    [ "a"; "b"; "https://x" ]
    (List.map (fun x -> x.Tui.item.Manifest.value) e)
;;

let nav_clamp () =
  let s = Tui.make (sections ()) ~height:10 ~width:80 in
  let s = Tui.step s Tui.Up in
  Alcotest.(check int) "stays at 0" 0 s.Tui.cursor;
  let s = Tui.step s Tui.End in
  Alcotest.(check int) "end" 2 s.Tui.cursor;
  let s = Tui.step s Tui.Down in
  Alcotest.(check int) "stays at end" 2 s.Tui.cursor;
  let s = Tui.step s Tui.Home in
  Alcotest.(check int) "home" 0 s.Tui.cursor
;;

let paging () =
  let s = Tui.make (sections ()) ~height:2 ~width:80 in
  let s = Tui.step s Tui.Page_down in
  Alcotest.(check int) "cursor" 2 s.Tui.cursor;
  Alcotest.(check int) "offset follows" 1 s.Tui.offset;
  Alcotest.(check int) "visible" 2 (List.length (Tui.visible s))
;;

let enter_mapping () =
  let s = Tui.make (sections ()) ~height:10 ~width:80 in
  (match Tui.enter_action s with
   | Tui.Do_nothing -> ()
   | _ -> Alcotest.fail "installed is noop");
  let s = Tui.step s Tui.Down in
  (match Tui.enter_action s with
   | Tui.Do_install it -> Alcotest.(check string) "value" "b" it.Manifest.value
   | _ -> Alcotest.fail "needsupdate installs");
  let s = Tui.step s Tui.Down in
  match Tui.enter_action s with
  | Tui.Do_open (_, u) -> Alcotest.(check string) "url" "https://x" u
  | _ -> Alcotest.fail "manual opens"
;;

let litem typ status value =
  { Manifest.typ
  ; value
  ; installed_version = "1.0"
  ; available_version = ""
  ; status
  ; upstream = ""
  }
;;

let enter_opens_links () =
  let open_at typ status value =
    let s =
      Tui.make
        [ { Manifest.name = "t"; items = [ litem typ status value ] } ]
        ~height:10
        ~width:80
    in
    Tui.enter_action s
  in
  (match open_at Manifest.Url Manifest.Installed "https://example.com/dl" with
   | Tui.Do_open (it, u) ->
     Alcotest.(check string) "row kept" "https://example.com/dl" it.Manifest.value;
     Alcotest.(check string) "url opens" "https://example.com/dl" u
   | _ -> Alcotest.fail "installed url opens");
  (match open_at Manifest.GitHub Manifest.Installed "owner/tool" with
   | Tui.Do_open (_, u) ->
     Alcotest.(check string) "repo page" "https://github.com/owner/tool" u
   | _ -> Alcotest.fail "installed github opens");
  (match open_at Manifest.GitHub Manifest.Manual "owner/tool" with
   | Tui.Do_open (_, u) ->
     Alcotest.(check string) "manual repo page" "https://github.com/owner/tool" u
   | _ -> Alcotest.fail "manual github opens");
  (match open_at (Manifest.GitLab "gitlab.com") Manifest.Installed "group/proj" with
   | Tui.Do_open (_, u) ->
     Alcotest.(check string) "gitlab page" "https://gitlab.com/group/proj" u
   | _ -> Alcotest.fail "installed gitlab opens");
  (match
     open_at
       (Manifest.Forgejo "git.example.com")
       Manifest.Manual
       "https://git.example.com/owner/repo"
   with
   | Tui.Do_open (_, u) ->
     Alcotest.(check string) "forgejo page" "https://git.example.com/owner/repo" u
   | _ -> Alcotest.fail "manual forgejo opens");
  match open_at Manifest.Winget Manifest.Installed "A.B" with
  | Tui.Do_nothing -> ()
  | _ -> Alcotest.fail "installed winget is noop"
;;

let enter_registry_noop () =
  (* Registry rows are discovery-only: Enter never acts on them, in any
     status. Installed (not New) so the row is actually an entry. *)
  let s =
    Tui.make
      [ { Manifest.name = "t"
        ; items = [ litem Manifest.Registry Manifest.Installed "GitHub CLI" ]
        }
      ]
      ~height:10
      ~width:80
  in
  match Tui.enter_action s with
  | Tui.Do_nothing -> ()
  | _ -> Alcotest.fail "registry rows are not actionable"
;;

let open_keeps_status () =
  (* Opening a link must not demote an installed row to manual. *)
  let s =
    Tui.make
      [ { Manifest.name = "t"
        ; items = [ litem Manifest.Url Manifest.Installed "https://x" ]
        }
      ]
      ~height:10
      ~width:80
  in
  let s =
    Tui.apply_outcome
      s
      "https://x"
      { Install.value = "https://x"; status = Install.Opened }
  in
  (match Tui.selected s with
   | Some e ->
     (match e.Tui.item.Manifest.status with
      | Manifest.Installed -> ()
      | _ -> Alcotest.fail "open keeps installed")
   | None -> Alcotest.fail "no selection");
  Alcotest.(check (option string)) "logs opened" (Some "https://x: opened") s.Tui.message
;;

let enter_notfound () =
  let s =
    Tui.make
      [ { Manifest.name = "t"; items = [ item Manifest.NotFound "n" ] } ]
      ~height:10
      ~width:80
  in
  match Tui.enter_action s with
  | Tui.Do_install _ -> ()
  | _ -> Alcotest.fail "notfound installs"
;;

let apply_success () =
  let s = Tui.make (sections ()) ~height:10 ~width:80 in
  let s = Tui.step s Tui.Down in
  let o = { Install.value = "b"; status = Install.Updated } in
  let s = Tui.apply_outcome s "b" o in
  (match Tui.selected s with
   | Some e ->
     Alcotest.(check bool)
       "now installed"
       true
       (e.Tui.item.Manifest.status = Manifest.Installed)
   | None -> Alcotest.fail "no selection");
  Alcotest.(check int) "one log line" 1 (List.length s.Tui.log)
;;

let apply_failure_keeps () =
  let s = Tui.make (sections ()) ~height:10 ~width:80 in
  let s = Tui.step s Tui.Down in
  let o = { Install.value = "b"; status = Install.Failed "denied" } in
  let s = Tui.apply_outcome s "b" o in
  match Tui.selected s with
  | Some e ->
    Alcotest.(check bool)
      "status kept"
      true
      (e.Tui.item.Manifest.status = Manifest.NeedsUpdate)
  | None -> Alcotest.fail "no selection"
;;

let frame_shape () =
  let s = Tui.make (sections ()) ~height:10 ~width:80 in
  let f = Tui.frame s in
  (* header + legend + TOOLS divider + a + b + MORE divider + url + footer *)
  Alcotest.(check int) "lines" 8 (List.length f);
  Alcotest.(check string) "divider uppercased" "TOOLS" (List.nth f 2);
  Alcotest.(check bool)
    "cursor marked"
    true
    (let row = List.nth f 3 in
     String.length row >= 2 && String.sub row 0 2 = "> ");
  Alcotest.(check bool)
    "bracket symbol"
    true
    (let row = List.nth f 3 in
     String.length row >= 8 && String.sub row 2 6 = "[✓] ")
;;

let frame_fits_height () =
  (* 5 single-item sections, viewport 3 lines: body must fit and the
     cursor row must stay visible at both ends. *)
  let secs =
    List.init 5 (fun i ->
      { Manifest.name = Printf.sprintf "s%d" i
      ; items = [ item Manifest.Installed (Printf.sprintf "v%d" i) ]
      })
  in
  let body_lines f = List.filter (fun l -> l <> "") f in
  let s = Devkit.Tui.make secs ~height:3 ~width:80 in
  let f = Devkit.Tui.frame s in
  Alcotest.(check int) "2 head + 3 body + foot" 6 (List.length f);
  Alcotest.(check int) "no blanks counted" 6 (List.length (body_lines f));
  let s = Devkit.Tui.step s Devkit.Tui.End in
  let f = Devkit.Tui.frame s in
  Alcotest.(check int) "still fits at end" 6 (List.length f);
  Alcotest.(check bool)
    "cursor row visible"
    true
    (List.exists (fun l -> String.length l >= 2 && String.sub l 0 2 = "> ") f)
;;

let colors () =
  let open Devkit.Tui in
  let open Devkit.Manifest in
  Alcotest.(check bool) "installed green" true (color_of Installed = Green);
  Alcotest.(check bool) "update yellow" true (color_of NeedsUpdate = Yellow);
  Alcotest.(check bool) "missing red" true (color_of NotFound = Red);
  Alcotest.(check bool) "new cyan" true (color_of New = Cyan);
  Alcotest.(check bool) "manual plain" true (color_of Manual = Plain)
;;

let scroll () =
  let s = Devkit.Tui.make (sections ()) ~height:2 ~width:80 in
  let s = Devkit.Tui.step s Devkit.Tui.Scroll_down in
  Alcotest.(check int) "scroll moves 1" 1 s.Devkit.Tui.cursor;
  let s = Devkit.Tui.step s Devkit.Tui.Scroll_up in
  Alcotest.(check int) "scroll back clamps" 0 s.Devkit.Tui.cursor
;;

let set_message () =
  let s = Tui.make (sections ()) ~height:10 ~width:80 in
  let s = Tui.set_message s "installing b ..." in
  Alcotest.(check (option string)) "footer" (Some "installing b ...") s.Tui.message;
  Alcotest.(check bool) "logged" true (List.mem "installing b ..." s.Tui.log)
;;

let apply_failure_logs_reason () =
  let s = Tui.make (sections ()) ~height:10 ~width:80 in
  let s = Tui.step s Tui.Down in
  let s =
    Tui.apply_outcome s "b" { Install.value = "b"; status = Install.Failed "denied" }
  in
  Alcotest.(check string)
    "full reason kept"
    "b: error: denied"
    (List.hd (List.rev s.Tui.log));
  let s = Tui.make (sections ()) ~height:10 ~width:80 in
  let s =
    Tui.apply_outcome
      s
      "a"
      { Install.value = "a"; status = Install.Skipped "no template" }
  in
  Alcotest.(check string)
    "skip reason kept"
    "a: skip: no template"
    (List.hd (List.rev s.Tui.log))
;;

let apply_to_sections_reflects_session () =
  let secs = sections () in
  let s = Tui.make secs ~height:10 ~width:80 in
  let s = Tui.step s Tui.Down in
  let s = Tui.apply_outcome s "b" { Install.value = "b"; status = Install.Updated } in
  let out = Tui.apply_to_sections secs s in
  let b =
    List.concat_map (fun sec -> sec.Manifest.items) out
    |> List.find (fun it -> it.Manifest.value = "b")
  in
  Alcotest.(check bool) "installed at end" true (b.Manifest.status = Manifest.Installed);
  Alcotest.(check bool) "renders green" true (Tui.color_of b.Manifest.status = Tui.Green)
;;

let () =
  Alcotest.run
    "tui"
    [ "entries", [ Alcotest.test_case "skips" `Quick build_skips ]
    ; ( "nav"
      , [ Alcotest.test_case "clamp" `Quick nav_clamp
        ; Alcotest.test_case "paging" `Quick paging
        ; Alcotest.test_case "scroll" `Quick scroll
        ] )
    ; ( "enter"
      , [ Alcotest.test_case "mapping" `Quick enter_mapping
        ; Alcotest.test_case "notfound" `Quick enter_notfound
        ; Alcotest.test_case "opens links" `Quick enter_opens_links
        ; Alcotest.test_case "registry noop" `Quick enter_registry_noop
        ] )
    ; ( "outcome"
      , [ Alcotest.test_case "success" `Quick apply_success
        ; Alcotest.test_case "failure keeps" `Quick apply_failure_keeps
        ; Alcotest.test_case "failure logs reason" `Quick apply_failure_logs_reason
        ; Alcotest.test_case "open keeps status" `Quick open_keeps_status
        ; Alcotest.test_case "end state green" `Quick apply_to_sections_reflects_session
        ; Alcotest.test_case "set message" `Quick set_message
        ] )
    ; ( "frame"
      , [ Alcotest.test_case "shape" `Quick frame_shape
        ; Alcotest.test_case "fits height" `Quick frame_fits_height
        ; Alcotest.test_case "colors" `Quick colors
        ] )
    ]
;;
