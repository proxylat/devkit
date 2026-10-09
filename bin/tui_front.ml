(** Lambda-term frontend for the dashboard state machine.

    Draws {!Devkit.Tui.frame} lines, feeds key events in, runs installs
    on Enter. On exit the plain-text dashboard plus the install log go
    to stdout, so redirected output stays usable. Lambda-term (unlike
    notty) has a real Windows backend, so this frontend     serves both
    OSes. The scan runs on a preemptive worker behind an instant loading
    frame (quit/resize stay live); installs and update checks run
    synchronously (the event loop freezes) but paint a status line first,
    so the screen states what is running instead of looking dead.
    Install failures log the full reason ([id: error: ...]), not the
    bare status.
    Mouse reporting is on for wheel-scroll only; other mouse events are
    ignored. *)

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
  | Update
  | Action of Tui.action
  | Resized of LTerm_geom.size
  | Nothing

let is_ctrl_q : Uchar.t -> bool =
  fun c -> Uchar.equal c (Uchar.of_char 'q') || Uchar.equal c (Uchar.of_char 'Q')
;;

let is_update_key : Uchar.t -> bool =
  fun c -> Uchar.equal c (Uchar.of_char 'u') || Uchar.equal c (Uchar.of_char 'U')
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
  | LTerm_event.Key { code = LTerm_key.Char c; _ } when is_update_key c -> Update
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
  | LTerm_event.Mouse m when m.LTerm_mouse.button = LTerm_mouse.Button4 ->
    Action Tui.Scroll_up
  | LTerm_event.Mouse m when m.LTerm_mouse.button = LTerm_mouse.Button5 ->
    Action Tui.Scroll_down
  | LTerm_event.Key _ | LTerm_event.Sequence _ | LTerm_event.Mouse _ -> Nothing
;;

let kind_of : Manifest.item_type -> string = function
  | Manifest.Winget -> "winget"
  | Manifest.GitHub -> "github"
  | Manifest.GitLab _ -> "gitlab"
  | Manifest.Forgejo _ -> "forgejo"
  | Manifest.Url -> "url"
  | Manifest.Registry -> "registry"
  | Manifest.Pm s -> s
;;

(** Self-hosted forge host for gitlab/forgejo rows; anything else
    passes [""] and the callee defaults. *)
let host_of : Manifest.item_type -> string = function
  | Manifest.GitLab h -> h
  | Manifest.Forgejo h -> h
  | _ -> ""
;;

let press_enter ~draw (deps : Install.deps) (st : Tui.state) : Tui.state Lwt.t =
  match Tui.enter_action st with
  | Tui.Do_nothing -> Lwt.return st
  | Tui.Do_install item ->
    let update = item.Manifest.status = Manifest.NeedsUpdate in
    draw (Tui.set_message st ("installing " ^ item.Manifest.value ^ " ..."))
    >>= fun () ->
    (* Let the repaint reach the screen before the blocking call. *)
    Lwt.pause ()
    >>= fun () ->
    Lwt.return
      (Tui.apply_outcome
         st
         item.Manifest.value
         (Install.install
            deps
            ~upstream:item.Manifest.upstream
            ~host:(host_of item.Manifest.typ)
            (kind_of item.Manifest.typ)
            item.Manifest.value
            update))
  | Tui.Do_open (item, url) ->
    (match deps.Install.open_browser url with
     | Ok () ->
       let value = item.Manifest.value in
       let o = { Install.value; status = Install.Opened; warnings = [] } in
       Lwt.return (Tui.apply_outcome st value o)
     | Error e ->
       let value = item.Manifest.value in
       let o = { Install.value; status = Install.Failed e; warnings = [] } in
       Lwt.return (Tui.apply_outcome st value o))
;;

let viewport (w : int) (h : int) : int * int = max 1 w, max 1 (h - 3)

(** [u]: check npm/pipx/uv/cargo updates for the Installed rows, then
    mark the hits. Paints a status line first, then runs synchronously
    like installs (UI freezes). *)
let press_u ~draw (run : Proc.runner) (fetch : Fetch.fetch) (st : Tui.state)
  : Tui.state Lwt.t
  =
  let items = List.map (fun e -> e.Tui.item) st.Tui.entries in
  draw (Tui.set_message st "checking updates ...")
  >>= fun () ->
  Lwt.pause ()
  >>= fun () -> Lwt.return (Tui.apply_updates st (Update.check_all ~run ~fetch items))
;;

(** Repaint in place, one addressed line at a time: no full-screen
    clear, so scrolling does not flash. Line count only changes on
    resize, which repaints from a cleared screen. *)
let draw_all (term : LTerm.t) (st : Tui.state) : unit Lwt.t =
  Lwt_list.iteri_s
    (fun i text ->
       LTerm.goto term { row = i; col = 0 }
       >>= fun () -> LTerm.clear_line term >>= fun () -> LTerm.fprints term text)
    (List.mapi styled_of_line (Tui.lines st))
  >>= fun () ->
  (* LTerm buffers through Lwt_io: without this, repaints (and the
     first frame) never reach the screen. *)
  LTerm.flush term
;;

let empty_scan_message (s : App.scan) : string option =
  if s.App.sources_error <> ""
  then Some ("bad sources; " ^ s.App.sources_error)
  else if s.App.apps <> []
  then None
  else if s.App.winget <> ""
  then Some ("empty scan; winget at " ^ s.App.winget)
  else if s.App.winget_error <> ""
  then Some ("empty scan; " ^ s.App.winget_error)
  else Some "empty scan; winget not found"
;;

let run ~(tools : Plugin.tool list) (env : App.env) : unit =
  (* First frame draws instantly with a loading row; the scan below
     (winget + 4 PMs, ~2s on Windows) runs on a preemptive worker while
     the event loop stays responsive to quit/resize. Stderr stays visible
     so the wait looks alive past the alternate screen. *)
  prerr_endline "scanning installed software...";
  flush stderr;
  let do_scan () =
    let sections, winget_path, sources = App.load_manifest env.App.fs Manifest.filename in
    let s = App.scan env ~override_path:winget_path ~extra:tools ~sources () in
    let show id =
      match App.winget_show env.App.bio s.App.winget id with
      | "" -> None
      | v -> Some v
    in
    let upstream_ver = App.upstream_ver_of_fetch env.App.fetch in
    let dash =
      Dashboard.build_sections
        ~show
        ~upstream_ver
        ~run:(Some env.App.run)
        s.App.apps
        sections
        s.App.info
    in
    dash, s, winget_path
  in
  let cleanup term mode =
    LTerm.disable_mouse term
    >>= fun () ->
    LTerm.load_state term
    >>= fun () -> LTerm.leave_raw_mode term mode >>= fun () -> LTerm.show_cursor term
  in
  let outcome =
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
       LTerm.enable_mouse term
       >>= fun () ->
       let geom = LTerm.size term in
       let vw, vh = viewport geom.LTerm_geom.cols geom.LTerm_geom.rows in
       (* Main loop once the scan lands: draws, then waits. Events that
          change nothing skip the repaint. *)
       let main_loop deps fetch init =
         let rec draw_loop (st : Tui.state) : Tui.state Lwt.t =
           draw_all term st >>= fun () -> wait_loop st
         and wait_loop (st : Tui.state) : Tui.state Lwt.t =
           LTerm.read_event term
           >>= fun ev ->
           match classify ev with
           | Quit -> Lwt.return st
           | Nothing -> wait_loop st
           | Enter -> press_enter ~draw:(draw_all term) deps st >>= draw_loop
           | Update -> press_u ~draw:(draw_all term) env.App.run fetch st >>= draw_loop
           | Action a -> draw_loop (Tui.step st a)
           | Resized g ->
             LTerm.clear_screen term
             >>= fun () ->
             let vw, vh = viewport g.LTerm_geom.cols g.LTerm_geom.rows in
             draw_loop (Tui.resize st ~height:vh ~width:vw)
         in
         draw_loop init
       in
       let loading =
         Tui.set_message
           (Tui.make [] ~height:vh ~width:vw)
           "scanning installed software..."
       in
       draw_all term loading
       >>= fun () ->
       let scan_job = Lwt_preemptive.detach do_scan () in
       let rec wait_scan (st : Tui.state)
         : [ `Done of Manifest.section list * App.scan * Tui.state | `Quit ] Lwt.t
         =
         Lwt.pick
           [ (scan_job >|= fun r -> `Scan r)
           ; (LTerm.read_event term >|= fun e -> `Event e)
           ]
         >>= function
         | `Scan (dash, s, winget_path) ->
           let fetch = Fetch.curl_fetch Proc.default_runner in
           let deps = Install.real_deps fetch ~winget_override:winget_path () in
           let init = Tui.make dash ~height:st.Tui.height ~width:st.Tui.width in
           let init =
             match empty_scan_message s with
             | None -> init
             | Some m -> Tui.set_message init m
           in
           main_loop deps fetch init
           >>= fun final ->
           cleanup term mode >>= fun () -> Lwt.return (`Done (dash, s, final))
         | `Event ev ->
           (match classify ev with
            | Quit ->
              Lwt.cancel scan_job;
              cleanup term mode >>= fun () -> Lwt.return `Quit
            | Resized g ->
              LTerm.clear_screen term
              >>= fun () ->
              let vw, vh = viewport g.LTerm_geom.cols g.LTerm_geom.rows in
              let st = Tui.resize st ~height:vh ~width:vw in
              draw_all term st >>= fun () -> wait_scan st
            | _ -> wait_scan st)
       in
       wait_scan loading)
  in
  match outcome with
  | `Quit -> ()
  | `Done (dash, s, final) ->
    (* Re-render from the end state, so successful installs show green
       instead of the pre-session snapshot. *)
    let dash = Tui.apply_to_sections dash final in
    print_string (Dashboard.render dash);
    List.iter print_endline final.Tui.log;
    (* An empty scan behind a double-clicked window would vanish with the
     console: leave the diagnostics readable until a keypress. Normal
     runs (and piped stdin) never pause. *)
    if s.App.apps = []
    then (
      let diag =
        if s.App.sources_error <> ""
        then "sources error: " ^ s.App.sources_error
        else if s.App.winget <> ""
        then "winget: " ^ s.App.winget
        else if s.App.winget_error <> ""
        then "winget error: " ^ s.App.winget_error
        else "winget: not found"
      in
      print_endline ("  no installed software found\n  " ^ diag);
      try
        if Unix.isatty Unix.stdin
        then (
          print_string "Press Enter to exit...";
          flush stdout;
          ignore (input_line stdin))
      with
      | _ -> ())
;;
