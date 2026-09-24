(** Lambda-term frontend for the dashboard state machine.

    Draws {!Devkit.Tui.frame} lines, feeds key events in, runs installs
    on Enter. On exit the plain-text dashboard plus the install log go
    to stdout, so redirected output stays usable. Lambda-term (unlike
    notty) has a real Windows backend, so this frontend serves both
    OSes. Installs run synchronously and freeze the UI while they run;
    mouse/wheel input is ignored (keyboard scroll covers it). *)

open Devkit
open Lwt.Infix

let style_of_color : Tui.color -> LTerm_style.t = function
  | Tui.Plain -> LTerm_style.none
  | Tui.Green -> { LTerm_style.none with foreground = Some LTerm_style.green }
  | Tui.Yellow -> { LTerm_style.none with foreground = Some LTerm_style.yellow }
  | Tui.Red -> { LTerm_style.none with foreground = Some LTerm_style.red }
  | Tui.Cyan -> { LTerm_style.none with foreground = Some LTerm_style.cyan }
;;

let styled_of_line (i : int) : Tui.line -> LTerm_text.t = function
  | Tui.Head s when i = 0 ->
    LTerm_text.stylise s { LTerm_style.none with bold = Some true }
  | Tui.Head s -> LTerm_text.of_utf8 s
  | Tui.Divider _ as d ->
    LTerm_text.stylise
      (" " ^ Tui.render_line d)
      { LTerm_style.none with bold = Some true; foreground = Some LTerm_style.lblack }
  | Tui.Row (e, cursor) as row ->
    let style = style_of_color (Tui.color_of e.Tui.item.Manifest.status) in
    let style = if cursor then { style with reverse = Some true } else style in
    LTerm_text.stylise (Tui.render_line row) style
;;

type ev =
  | Quit
  | Enter
  | Action of Tui.action
  | Resized of LTerm_geom.size
  | Nothing

let is_ctrl_q : Uchar.t -> bool =
  fun c -> Uchar.equal c (Uchar.of_char 'q') || Uchar.equal c (Uchar.of_char 'Q')
;;

let is_nav : Uchar.t -> Tui.action option =
  fun c ->
  if Uchar.equal c (Uchar.of_char 'k')
  then Some Tui.Up
  else if Uchar.equal c (Uchar.of_char 'j')
  then Some Tui.Down
  else None
;;

let classify : LTerm_event.t -> ev = function
  | LTerm_event.Resize size -> Resized size
  | LTerm_event.Key k when k.LTerm_key.control -> Quit
  | LTerm_event.Key { code = LTerm_key.Escape; _ } -> Quit
  | LTerm_event.Key { code = LTerm_key.Char c; _ } when is_ctrl_q c -> Quit
  | LTerm_event.Key { code = LTerm_key.Enter; _ } -> Enter
  | LTerm_event.Key { code = LTerm_key.Up; _ } -> Action Tui.Up
  | LTerm_event.Key { code = LTerm_key.Down; _ } -> Action Tui.Down
  | LTerm_event.Key { code = LTerm_key.Prev_page; _ } -> Action Tui.Page_up
  | LTerm_event.Key { code = LTerm_key.Next_page; _ } -> Action Tui.Page_down
  | LTerm_event.Key { code = LTerm_key.Home; _ } -> Action Tui.Home
  | LTerm_event.Key { code = LTerm_key.End; _ } -> Action Tui.End
  | LTerm_event.Key { code = LTerm_key.Char c; _ } ->
    (match is_nav c with
     | Some a -> Action a
     | None -> Nothing)
  | LTerm_event.Key _ | LTerm_event.Sequence _ | LTerm_event.Mouse _ -> Nothing
;;

let kind_of : Manifest.item_type -> string = function
  | Manifest.Winget -> "winget"
  | Manifest.GitHub -> "github"
  | Manifest.Url -> "url"
  | Manifest.Pm s -> s
;;

let press_enter (deps : Install.deps) (st : Tui.state) : Tui.state =
  match Tui.enter_action st with
  | Tui.Do_nothing -> st
  | Tui.Do_install item ->
    let update = item.Manifest.status = Manifest.NeedsUpdate in
    let o = Install.install deps (kind_of item.Manifest.typ) item.Manifest.value update in
    Tui.apply_outcome st item.Manifest.value o
  | Tui.Do_open url ->
    (match deps.Install.open_browser url with
     | Ok () ->
       let o = { Install.value = url; status = Install.Opened } in
       Tui.apply_outcome st url o
     | Error e ->
       let o = { Install.value = url; status = Install.Failed e } in
       Tui.apply_outcome st url o)
;;

let viewport (w : int) (h : int) : int * int = max 1 w, max 1 (h - 3)

let draw_all (term : LTerm.t) (st : Tui.state) : unit Lwt.t =
  LTerm.clear_screen term
  >>= fun () ->
  LTerm.goto term { row = 0; col = 0 }
  >>= fun () ->
  Lwt_list.iter_s
    (fun text -> LTerm.fprintls term text)
    (List.mapi styled_of_line (Tui.lines st))
;;

let run ~(tools : Plugin.tool list) (env : App.env) : unit =
  let sections, winget_path = App.load_manifest env.App.fs Manifest.filename in
  let s = App.scan env ~override_path:winget_path ~extra:tools in
  let show id =
    match App.winget_show env.App.bio s.App.winget id with
    | "" -> None
    | v -> Some v
  in
  let dash = Dashboard.build_sections ~show s.App.apps sections s.App.info in
  let fetch = Fetch.curl_fetch Proc.default_runner in
  let deps = Install.real_deps fetch ~winget_override:winget_path () in
  let final =
    Lwt_main.run
      (Lazy.force LTerm.stdout
       >>= fun term ->
       LTerm.enter_raw_mode term
       >>= fun mode ->
       LTerm.hide_cursor term
       >>= fun () ->
       (* Alternate screen: redraws paint off-scrollback, and exiting
          restores the shell view (like the old notty frontend). *)
       LTerm.save_state term
       >>= fun () ->
       let geom = LTerm.size term in
       let vw, vh = viewport geom.LTerm_geom.cols geom.LTerm_geom.rows in
       let rec loop (st : Tui.state) : Tui.state Lwt.t =
         draw_all term st
         >>= fun () ->
         LTerm.read_event term
         >>= fun ev ->
         match classify ev with
         | Quit -> Lwt.return st
         | Enter -> loop (press_enter deps st)
         | Action a -> loop (Tui.step st a)
         | Resized g ->
           let vw, vh = viewport g.LTerm_geom.cols g.LTerm_geom.rows in
           loop (Tui.resize st ~height:vh ~width:vw)
         | Nothing -> loop st
       in
       loop (Tui.make dash ~height:vh ~width:vw)
       >>= fun st ->
       LTerm.load_state term
       >>= fun () ->
       LTerm.leave_raw_mode term mode
       >>= fun () -> LTerm.show_cursor term >>= fun () -> Lwt.return st)
  in
  print_string (Dashboard.render dash);
  List.iter print_endline final.Tui.log
;;
