(** Lambda-term frontend for the dashboard state machine.

    Draws {!Devkit.Tui.frame} lines, feeds key events in, runs installs
    on Enter. On exit the plain-text dashboard plus the install log go
    to stdout, so redirected output stays usable. Lambda-term (unlike
    notty) has a real Windows backend, so this frontend serves both
    OSes. First paint is instant (cached inventory or a loading row);
    a progressive refresh then repaints as each source lands on
    parallel workers, followed by winget-show enrichment and upstream
    tags — quit/resize stay live throughout. Installs and [u]
    rescans run synchronously (the event loop freezes) but paint a
    status line first, so the screen states what is running instead of
    looking dead. Install failures log the full reason ([id: error:
    ...]), not the bare status.
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

(** One background result: a landed source (apps plus the raw [winget
    list] output, if it was winget's turn) or the resolved winget
    binary. *)
type landing =
  | Src of string * Dashboard.app list * string option
  | Ens of string * string

(** Progressive refresh accumulator: landed source results, the raw
    winget output, the resolved winget binary, and jobs in flight. *)
type prog =
  { landed : (string * Dashboard.app list) list
  ; winget_out : string option
  ; winget : string
  ; winget_error : string
  ; pending : int
  }

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
            ~quarantine_days:item.Manifest.quarantine_days
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

(** [u]: full rescan (sources, show enrichment, upstream tags), then
    the Installed-row update check for the non-winget PMs. Yank
    warnings ride the log, not the footer. Paints a status line first,
    then runs synchronously like installs (UI freezes). The fresh
    inventory also rewrites the disk cache, so the next launch paints
    current data instantly. *)
let press_u
      ~draw
      (env : App.env)
      (tools : Plugin.tool list)
      (load_lock : unit -> Lockfile.t)
      (cache_file : string option)
      (st : Tui.state)
  : Tui.state Lwt.t
  =
  draw (Tui.set_message st "rescanning ...")
  >>= fun () ->
  Lwt.pause ()
  >>= fun () ->
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
  (match cache_file with
   | None -> ()
   | Some f -> Scan_cache.save f s.App.apps s.App.info);
  let st = Tui.remake st dash in
  let items = List.map (fun e -> e.Tui.item) st.Tui.entries in
  let updates, warns =
    Update.check_all ~run:env.App.run ~fetch:env.App.fetch ~lock:(load_lock ()) items
  in
  Lwt.return (Tui.log_lines (Tui.apply_updates st updates) warns)
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
  let sections, winget_path, sources = App.load_manifest env.App.fs Manifest.filename in
  let cache_file = Scan_cache.cache_path () in
  let cached = Option.bind cache_file Scan_cache.load in
  let fetch_exe, _ = Proc.winget_list env.App.run in
  let fetch = Fetch.curl_fetch Proc.default_runner in
  (* Worker bodies never raise: a failed source contributes nothing,
     exactly like {!Inventory.scan_all}. *)
  let guarded : type a. string -> a -> (unit -> a) -> a =
    fun what dflt f ->
    try f () with
    | e ->
      prerr_endline ("devkit: " ^ what ^ " failed: " ^ Printexc.to_string e);
      dflt
  in
  (* One source on a worker thread: blocking spawns only, no Lwt.
     [winget_exe] is the ensure-resolved binary ("" = PATH lookup). *)
  let scan_one (winget_exe : string) (name : string) : Dashboard.app list * string option =
    Inventory.time_src name (fun () ->
      if name = "winget"
      then (
        match fetch_exe winget_exe with
        | None -> [], None
        | Some out -> Inventory.parse_winget out, Some out)
      else if name = "registry"
      then
        if Sys.os_type = "Win32" then Inventory.scan_reg env.App.run, None else [], None
      else (
        match
          List.find_opt
            (fun (t : Plugin.tool) -> t.Plugin.name = name)
            (Inventory.built_ins @ tools)
        with
        | None -> [], None
        | Some t -> Inventory.scan_tool env.App.run t, None))
  in
  let ensure_one () : string * string =
    Inventory.time_src "ensure" (fun () ->
      match Bootstrap.ensure ~override_path:winget_path env.App.bio with
      | Ok w -> w, ""
      | Error err -> "", err)
  in
  let apps_of (srcs : string list) (p : prog) : Dashboard.app list =
    List.concat_map (fun n -> List.assoc_opt n p.landed |> Option.value ~default:[]) srcs
  in
  (* Cached paint: the previous run's inventory merged without any
     spawn (no show, no upstream, no PATH probe). Guarded so a
     corrupt cache degrades to the loading row instead of killing
     startup. *)
  let cached_dash =
    guarded "cached merge" [] (fun () ->
      match cached with
      | None -> []
      | Some snap ->
        let info =
          if snap.Scan_cache.info = []
          then None
          else Some (Scan_cache.to_info_map snap.Scan_cache.info)
        in
        Dashboard.build_sections snap.Scan_cache.apps sections info)
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
       let main_loop deps load_lock init =
         let rec draw_loop (st : Tui.state) : Tui.state Lwt.t =
           draw_all term st >>= fun () -> wait_loop st
         and wait_loop (st : Tui.state) : Tui.state Lwt.t =
           LTerm.read_event term
           >>= fun ev ->
           match classify ev with
           | Quit -> Lwt.return st
           | Nothing -> wait_loop st
           | Enter -> press_enter ~draw:(draw_all term) deps st >>= draw_loop
           | Update ->
             press_u ~draw:(draw_all term) env tools load_lock cache_file st >>= draw_loop
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
         match cached with
         | None ->
           Tui.set_message
             (Tui.make [] ~height:vh ~width:vw)
             "scanning installed software..."
         | Some _ ->
           Tui.set_message (Tui.make cached_dash ~height:vh ~width:vw) "refreshing..."
       in
       let cur_dash = ref cached_dash in
       draw_all term loading
       >>= fun () ->
       let stream, push = Lwt_stream.create () in
       let cancels = ref [] in
       (* One worker per job; each pushes exactly one landing, then
           the forwarding fiber ends. The [>>=] continuation runs on
           the Lwt thread, so [push] is safe there. *)
       let spawn (f : unit -> landing) : unit =
         let job = Lwt_preemptive.detach f () in
         cancels := (fun () -> Lwt.cancel job) :: !cancels;
         Lwt.async (fun () ->
           Lwt.catch
             (fun () ->
                job
                >>= fun r ->
                push (Some r);
                Lwt.return_unit)
             (function
               | Lwt.Canceled -> Lwt.return_unit
               | e -> Lwt.fail e))
       in
       let cancel_all () = List.iter (fun c -> c ()) !cancels in
       let make_deps () =
         let read_file p =
           match env.App.fs.read_file p with
           | None -> Error "not found"
           | Some t -> Ok t
         in
         let load_lock () =
           match env.App.fs.read_file Lockfile.filename with
           | None -> []
           | Some t -> Lockfile.parse t
         in
         let deps =
           Install.real_deps
             fetch
             ~winget_override:winget_path
             ~read_file
             ~write_file:env.App.fs.write_file
             ()
         in
         deps, load_lock
       in
       (* Single-job wait (show / upstream phases): repaint once when
           the merge lands, quit/resize stay live. *)
       let rec wait_one
                 (st : Tui.state)
                 (job : Manifest.section list Lwt.t)
                 (apply : Manifest.section list -> Tui.state -> Tui.state)
         : [ `Done of Tui.state | `Quit ] Lwt.t
         =
         Lwt.pick
           [ (job >|= fun r -> `Job r); (LTerm.read_event term >|= fun e -> `Event e) ]
         >>= function
         | `Job r ->
           let st = apply r st in
           draw_all term st >>= fun () -> Lwt.return (`Done st)
         | `Event ev ->
           (match classify ev with
            | Quit ->
              Lwt.cancel job;
              cancel_all ();
              cleanup term mode >>= fun () -> Lwt.return `Quit
            | Resized g ->
              LTerm.clear_screen term
              >>= fun () ->
              let vw, vh = viewport g.LTerm_geom.cols g.LTerm_geom.rows in
              let st = Tui.resize st ~height:vh ~width:vw in
              draw_all term st >>= fun () -> wait_one st job apply
            | _ -> wait_one st job apply)
       in
       (* Source phase: repaint per landing until every worker has
           reported. Winget waits for ensure (portable installs live
           off PATH), everything else races it. *)
       let rec wait_sources (srcs : string list) (p : prog) (st : Tui.state)
         : [ `Done of prog * Tui.state | `Quit ] Lwt.t
         =
         if p.pending = 0
         then Lwt.return (`Done (p, st))
         else
           Lwt.pick
             [ (Lwt_stream.get stream >|= fun r -> `Data r)
             ; (LTerm.read_event term >|= fun e -> `Event e)
             ]
           >>= function
           | `Data None -> Lwt.return (`Done (p, st))
           | `Data (Some (Src (name, apps, out))) ->
             let p =
               { p with
                 landed = (name, apps) :: p.landed
               ; winget_out =
                   (match out with
                    | None -> p.winget_out
                    | Some _ -> out)
               ; pending = p.pending - 1
               }
             in
             let apps_all = apps_of srcs p in
             let info = App.info_of p.winget_out apps_all in
             let dash =
               Inventory.time_src "merge" (fun () ->
                 Dashboard.build_sections ~run:(Some env.App.run) apps_all sections info)
             in
             cur_dash := dash;
             let st = Tui.set_message (Tui.remake st dash) "refreshing..." in
             draw_all term st >>= fun () -> wait_sources srcs p st
           | `Data (Some (Ens (w, err))) ->
             let p = { p with winget = w; winget_error = err; pending = p.pending - 1 } in
             let p =
               if List.mem "winget" srcs
               then (
                 spawn (fun () ->
                   let apps, out =
                     guarded "scan:winget" ([], None) (fun () -> scan_one w "winget")
                   in
                   Src ("winget", apps, out));
                 { p with pending = p.pending + 1 })
               else p
             in
             wait_sources srcs p st
           | `Event ev ->
             (match classify ev with
              | Quit ->
                cancel_all ();
                cleanup term mode >>= fun () -> Lwt.return `Quit
              | Resized g ->
                LTerm.clear_screen term
                >>= fun () ->
                let vw, vh = viewport g.LTerm_geom.cols g.LTerm_geom.rows in
                let st = Tui.resize st ~height:vh ~width:vw in
                draw_all term st >>= fun () -> wait_sources srcs p st
              | _ -> wait_sources srcs p st)
       in
       match App.resolve_sources sources ~tools with
       | Error err ->
         (* No workers: straight to the main loop with the error. *)
         let s =
           { App.apps = []
           ; info = None
           ; winget = ""
           ; winget_error = ""
           ; sources_error = err
           }
         in
         let dash = Dashboard.build_sections [] sections None in
         cur_dash := dash;
         let init = Tui.make dash ~height:loading.Tui.height ~width:loading.Tui.width in
         let init =
           match empty_scan_message s with
           | None -> init
           | Some m -> Tui.set_message init m
         in
         let deps, load_lock = make_deps () in
         main_loop deps load_lock init
         >>= fun final ->
         cleanup term mode >>= fun () -> Lwt.return (`Done (dash, s, final))
       | Ok srcs ->
         spawn (fun () ->
           let w, err = guarded "ensure" ("", "ensure failed") ensure_one in
           Ens (w, err));
         let non_winget = List.filter (fun n -> n <> "winget") srcs in
         List.iter
           (fun n ->
              spawn (fun () ->
                let apps, out =
                  guarded ("scan:" ^ n) ([], None) (fun () -> scan_one "" n)
                in
                Src (n, apps, out)))
           non_winget;
         let p =
           { landed = []
           ; winget_out = None
           ; winget = ""
           ; winget_error = ""
           ; pending = 1 + List.length non_winget
           }
         in
         wait_sources srcs p loading
         >>= (function
          | `Quit -> Lwt.return `Quit
          | `Done (p, st) ->
            let apps = apps_of srcs p in
            let info = App.info_of p.winget_out apps in
            (match cache_file with
             | None -> ()
             | Some f -> Scan_cache.save f apps info);
            let s =
              { App.apps
              ; info
              ; winget = p.winget
              ; winget_error = p.winget_error
              ; sources_error = ""
              }
            in
            let show id =
              match App.winget_show env.App.bio p.winget id with
              | "" -> None
              | v -> Some v
            in
            let upstream_ver = App.upstream_ver_of_fetch fetch in
            let show_job =
              Lwt_preemptive.detach
                (fun () ->
                   guarded "show" !cur_dash (fun () ->
                     Inventory.time_src "show" (fun () ->
                       Dashboard.build_sections
                         ~show
                         ~run:(Some env.App.run)
                         apps
                         sections
                         info)))
                ()
            in
            wait_one st show_job (fun dash st ->
              cur_dash := dash;
              Tui.set_message (Tui.remake st dash) "refreshing...")
            >>= (function
             | `Quit -> Lwt.return `Quit
             | `Done st ->
               let up_job =
                 Lwt_preemptive.detach
                   (fun () ->
                      guarded "upstream" !cur_dash (fun () ->
                        Inventory.time_src "upstream" (fun () ->
                          Dashboard.build_sections
                            ~show
                            ~upstream_ver
                            ~run:(Some env.App.run)
                            apps
                            sections
                            info)))
                   ()
               in
               wait_one st up_job (fun dash st ->
                 cur_dash := dash;
                 Tui.set_message (Tui.remake st dash) "refreshing...")
               >>= (function
                | `Quit -> Lwt.return `Quit
                | `Done st ->
                  let st = { st with Tui.message = None } in
                  let deps, load_lock = make_deps () in
                  let init =
                    match empty_scan_message s with
                    | None -> st
                    | Some m -> Tui.set_message st m
                  in
                  main_loop deps load_lock init
                  >>= fun final ->
                  cleanup term mode >>= fun () -> Lwt.return (`Done (!cur_dash, s, final)))))
      )
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
