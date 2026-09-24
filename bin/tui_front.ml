(** Notty frontend for the dashboard state machine.

    Draws {!Devkit.Tui.frame} lines, feeds key events in, runs installs on
    Enter. On exit the plain-text dashboard plus the install log go to
    stdout, so redirected output stays usable. *)

open Devkit
open Notty
open Notty_unix

let kind_of : Manifest.item_type -> string = function
  | Manifest.Winget -> "winget"
  | Manifest.GitHub -> "github"
  | Manifest.Url -> "url"
  | Manifest.Pm s -> s
;;

let color_attr : Tui.color -> attr = function
  | Tui.Plain -> A.empty
  | Tui.Green -> A.(fg green)
  | Tui.Yellow -> A.(fg yellow)
  | Tui.Red -> A.(fg red)
  | Tui.Cyan -> A.(fg cyan)
;;

let draw (st : Tui.state) : image =
  let rows = Tui.visible st in
  let img_of_line i line =
    if i = 0
    then I.string A.(st bold) line
    else if i = 1
    then I.string A.empty line
    else (
      match List.nth_opt rows (i - 2) with
      | None -> I.string A.empty line
      | Some e ->
        let attr = color_attr (Tui.color_of e.Tui.item.Manifest.status) in
        let attr =
          if st.Tui.offset + i - 2 = st.Tui.cursor then A.(attr ++ st reverse) else attr
        in
        I.string attr line)
  in
  I.vcat (List.mapi img_of_line (Tui.frame st))
;;

type ev =
  [ Notty.Unescape.event
  | `Resize of int * int
  | `End
  ]

let quit_key : ev -> bool = function
  | `End -> true
  | `Key (`Escape, _) -> true
  | `Key (`ASCII 'q', _) | `Key (`ASCII 'Q', _) -> true
  | `Key (_, mods) -> List.mem `Ctrl mods
  | _ -> false
;;

let nav : ev -> Tui.action option = function
  | `Key (`Arrow `Up, _) | `Key (`ASCII 'k', _) -> Some Tui.Up
  | `Key (`Arrow `Down, _) | `Key (`ASCII 'j', _) -> Some Tui.Down
  | `Key (`Page `Up, _) -> Some Tui.Page_up
  | `Key (`Page `Down, _) -> Some Tui.Page_down
  | `Key (`Home, _) -> Some Tui.Home
  | `Key (`End, _) -> Some Tui.End
  | `Mouse (`Press (`Scroll `Up), _, _) -> Some Tui.Scroll_up
  | `Mouse (`Press (`Scroll `Down), _, _) -> Some Tui.Scroll_down
  | _ -> None
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
  let term = Term.create () in
  let w, h = Term.size term in
  let vw, vh = viewport w h in
  let rec loop (st : Tui.state) : Tui.state =
    Term.image term (draw st);
    match Term.event term with
    | ev when quit_key ev -> st
    | `Key (`Enter, _) -> loop (press_enter deps st)
    | `Resize (w, h) ->
      let vw, vh = viewport w h in
      loop (Tui.resize st ~height:vh ~width:vw)
    | ev ->
      (match nav ev with
       | Some a -> loop (Tui.step st a)
       | None -> loop st)
  in
  let final = loop (Tui.make dash ~height:vh ~width:vw) in
  Term.release term;
  print_string (Dashboard.render dash);
  List.iter print_endline final.Tui.log
;;
