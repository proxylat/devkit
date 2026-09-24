open Devkit

let item status value =
  { Manifest.typ = Manifest.Winget
  ; value
  ; installed_version = "1.0"
  ; available_version = ""
  ; status
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
  | Tui.Do_open u -> Alcotest.(check string) "url" "https://x" u
  | _ -> Alcotest.fail "manual opens"
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
        ] )
    ; ( "outcome"
      , [ Alcotest.test_case "success" `Quick apply_success
        ; Alcotest.test_case "failure keeps" `Quick apply_failure_keeps
        ] )
    ; ( "frame"
      , [ Alcotest.test_case "shape" `Quick frame_shape
        ; Alcotest.test_case "fits height" `Quick frame_fits_height
        ; Alcotest.test_case "colors" `Quick colors
        ] )
    ]
;;
