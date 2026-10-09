(** Dashboard: merging scan results with the manifest, plus plain-text render.

    The [winget show] cross-reference in [build_sections] spawns processes;
    here it is an injectable [?show] function ([None] by default) so the
    merge stays pure and testable. *)

open Manifest

type app =
  { name : string
  ; version : string
  ; pm : string
  }

let status_symbol = function
  | Installed -> "✓"
  | NeedsUpdate -> "!"
  | NotFound -> "+"
  | Manual -> "~"
  | New -> "?"
;;

(* NOTE: [New] renders as [?]. Whether "Newly detected" deserves its own
   symbol is an open nit (like the hidden-section question below). *)

let format_ver (it : item) : string =
  if it.installed_version = ""
  then if it.available_version <> "" then " " ^ it.available_version else ""
  else if it.available_version <> "" && it.installed_version <> it.available_version
  then " " ^ it.installed_version ^ " -> " ^ it.available_version
  else " " ^ it.installed_version
;;

(** Candidate scan-names for a manifest value: the full lowercased value
    plus a short form. Dotted/slashed ids ([Git.Git], [owner/tool])
    collapse to their last segment; URLs contribute the filename and
    its stem (no query/fragment/extension). The PM scan or PATH probe
    sees command names, not publisher prefixes or installer URLs. *)
let after_last seps s =
  let idx =
    let rec go i =
      if i < 0 then None else if List.mem s.[i] seps then Some i else go (i - 1)
    in
    go (String.length s - 1)
  in
  match idx with
  | None -> s
  | Some i -> String.sub s (i + 1) (String.length s - i - 1)
;;

let cut_at chars s =
  let idx =
    let rec go i =
      if i >= String.length s
      then None
      else if List.mem s.[i] chars
      then Some i
      else go (i + 1)
    in
    go 0
  in
  match idx with
  | None -> s
  | Some i -> String.sub s 0 i
;;

let strip_ext s =
  match String.rindex_opt s '.' with
  | None -> s
  | Some i -> String.sub s 0 i
;;

let candidates (it : item) : string list =
  let v = String.lowercase_ascii it.value in
  let shorts =
    match it.typ with
    | Url ->
      let file = cut_at [ '?'; '#' ] (after_last [ '/'; '\\' ] v) in
      [ file; strip_ext file ]
    | Registry ->
      (* Display names carry spaces and never match scan keys or PATH
         entries; the token rescue below handles them. *)
      []
    | Winget | GitHub | GitLab _ | Forgejo _ | Pm _ -> [ after_last [ '.'; '/'; '\\' ] v ]
  in
  List.sort_uniq String.compare (List.filter (fun s -> s <> "") (v :: shorts))
;;

(** Word tokens: lowercase alphanumeric runs. Registry [DisplayName]s
    ("VLC media player") and manifest nicks ("vlc") meet here. *)
let toks_of_words (s : string) : string list =
  s
  |> String.lowercase_ascii
  |> String.map (fun c ->
    if ('a' <= c && c <= 'z') || ('0' <= c && c <= '9') then c else ' ')
  |> String.split_on_char ' '
  |> List.filter (fun w -> w <> "")
;;

(** Short matchable form: the id tail for repo/winget rows, the file
    stem for links. ["VideoLAN.VLC" → "vlc"]. *)
let nick (it : item) : string =
  match it.typ with
  | Url -> strip_ext (cut_at [ '?'; '#' ] (after_last [ '/'; '\\' ] it.value))
  | _ -> after_last [ '.'; '/'; '\\' ] it.value
;;

let subset (small : string list) (big : string list) : bool =
  small <> [] && List.for_all (fun t -> List.mem t big) small
;;

(** Merge the machine scan with manifest sections into dashboard sections:
    1. "Pending updates": manifest items needing update, plus
       non-manifest installed apps that have an Available version.
    2. "Newly detected": other installed apps missing from the manifest.
    3. The manifest sections minus their updated and installed items.
    4. "Installed": the manifest items already at their latest version,
       trailing the table so the run ends on green.
    A manifest section literally named "winget" is dropped (it is a
    leftover artifact, never real content).

    Rows with an [upstream] pin — plus forge rows (GitHub, GitLab,
    Forgejo), self-pinned to their own id — compare against the
    vendor's release tag ([?upstream_ver]) instead of their PM's
    reading; a failed lookup keeps the PM reading. *)
let build_sections
      ?(show : string -> string option = fun _ -> None)
      ?(upstream_ver : Provider.t -> string -> string option = fun _ _ -> None)
      ?(run : Proc.runner option = None)
      ?(os : string = Sys.os_type)
      (apps : app list)
      (pkgs_sections : section list)
      (winget_info : Winget_parse.info Winget_parse.IdMap.t option)
  : section list
  =
  let lower s = String.lowercase_ascii s in
  let scan_lookup : (string, app) Hashtbl.t = Hashtbl.create 64 in
  (* [apps] arrives in sources order; earliest source wins ties, so the
     manifest's order picks the label. *)
  List.iter (fun a -> Hashtbl.replace scan_lookup (lower a.name) a) (List.rev apps);
  (* Registry word index for the token rescue below. *)
  let reg_idx : (string list * app) list =
    List.filter_map
      (fun (a : app) ->
         if a.pm = "registry" then Some (toks_of_words a.name, a) else None)
      apps
  in
  let reg_hit (it : item) : app option =
    let small = toks_of_words (nick it) in
    List.find_map (fun (toks, a) -> if subset small toks then Some a else None) reg_idx
  in
  (* PATH probe: one spawn over every candidate of every manifest item.
     Skipped entirely when no runner is passed (tests, offline use). *)
  let on_path : (string, unit) Hashtbl.t = Hashtbl.create 64 in
  (match run with
   | None -> ()
   | Some run ->
     let cands =
       List.concat_map (fun sec -> List.concat_map candidates sec.items) pkgs_sections
     in
     List.iter (fun n -> Hashtbl.replace on_path n ()) (Proc.which ~os run cands));
  let scan_hit (it : item) : app option =
    List.find_map (fun c -> Hashtbl.find_opt scan_lookup c) (candidates it)
  in
  let path_hit (it : item) : bool = List.exists (Hashtbl.mem on_path) (candidates it) in
  let info_of name =
    match winget_info with
    | None -> None
    | Some m -> Winget_parse.IdMap.find_opt (lower name) m
  in
  (* 1. Pre-set status for manifest items. *)
  let pkgs_sections =
    List.map
      (fun sec ->
         { sec with
           items =
             List.map
               (fun it ->
                  let name = lower it.value in
                  let it =
                    match scan_hit it with
                    | Some a ->
                      { it with installed_version = a.version; status = Installed }
                    | None -> it
                  in
                  let it =
                    match it.typ with
                    | Winget ->
                      (match info_of name with
                       | Some inf ->
                         let it = { it with installed_version = inf.version } in
                         if inf.available <> "" && inf.version <> inf.available
                         then
                           { it with
                             available_version = inf.available
                           ; status = NeedsUpdate
                           }
                         else { it with status = Installed }
                       | None -> { it with status = NotFound })
                    | GitHub | GitLab _ | Forgejo _ | Url | Pm _ | Registry ->
                      { it with status = Manual }
                  in
                  let it =
                    if it.status <> Installed && it.status <> NeedsUpdate
                    then (
                      match scan_hit it with
                      | Some a ->
                        { it with installed_version = a.version; status = Installed }
                      | None ->
                        (match reg_hit it with
                         | Some a ->
                           { it with installed_version = a.version; status = Installed }
                         | None ->
                           if path_hit it then { it with status = Installed } else it))
                    else it
                  in
                  if it.typ = Winget && it.status = Installed
                  then (
                    match info_of name with
                    | Some inf when inf.available <> "" && inf.version <> inf.available ->
                      { it with available_version = inf.available; status = NeedsUpdate }
                    | _ -> it)
                  else it)
               sec.items
         })
      pkgs_sections
  in
  (* 2. `winget show` fallback for NotFound winget items: every missing
     id is queried up front on up to 8 domains (each call is one process
     spawn, ~1s on Windows; sequential was the startup bottleneck), then
     the merge below reads from the table. *)
  let show_ids =
    List.concat_map
      (fun sec ->
         List.filter_map
           (fun it ->
              if it.typ = Winget && it.status = NotFound then Some it.value else None)
           sec.items)
      pkgs_sections
  in
  let show_table : (string, string option) Hashtbl.t =
    Hashtbl.create (max 1 (List.length show_ids))
  in
  List.iter2
    (fun id ver -> Hashtbl.replace show_table id ver)
    show_ids
    (Proc.par_map8 show show_ids);
  let pkgs_sections =
    List.map
      (fun sec ->
         { sec with
           items =
             List.map
               (fun it ->
                  if it.typ <> Winget || it.status <> NotFound
                  then it
                  else (
                    match Hashtbl.find_opt show_table it.value with
                    | None | Some None -> it
                    | Some (Some ver) ->
                      let it = { it with available_version = ver } in
                      (match scan_hit it with
                       | Some a ->
                         { it with
                           installed_version = a.version
                         ; status = (if a.version <> ver then NeedsUpdate else Installed)
                         }
                       | None -> it)))
               sec.items
         })
      pkgs_sections
  in
  (* 2b. Upstream truth: pinned rows, plus forge rows self-pinned to
     their own id, compare their installed version against the vendor's
     release tag instead of trusting their PM's reading. Each repo is
     queried once, up to 8 in flight; a failed query keeps the PM
     reading (offline fallback). Rows without an installed version skip
     the check: with nothing installed there is nothing to compare. *)
  let upstream_ids =
    List.concat_map
      (fun sec ->
         List.filter_map
           (fun it ->
              match effective_upstream it with
              | Some pr when it.status = Installed || it.status = NeedsUpdate -> Some pr
              | _ -> None)
           sec.items)
      pkgs_sections
    |> List.sort_uniq compare
  in
  let upstream_table : (Provider.t * string, string option) Hashtbl.t =
    Hashtbl.create (max 1 (List.length upstream_ids))
  in
  List.iter2
    (fun (prov, repo) tag -> Hashtbl.replace upstream_table (prov, repo) tag)
    upstream_ids
    (Proc.par_map8 (fun (prov, repo) -> upstream_ver prov repo) upstream_ids);
  let pkgs_sections =
    List.map
      (fun sec ->
         { sec with
           items =
             List.map
               (fun it ->
                  match effective_upstream it with
                  | None -> it
                  | Some pr ->
                    if it.installed_version = ""
                    then it
                    else (
                      match Hashtbl.find_opt upstream_table pr with
                      | None | Some None -> it
                      | Some (Some tag) ->
                        if Strutil.is_newer_version it.installed_version tag
                        then { it with available_version = tag; status = NeedsUpdate }
                        else { it with available_version = ""; status = Installed }))
               sec.items
         })
      pkgs_sections
  in
  (* 3. Non-manifest installed apps with updates → pending updates. *)
  let value_in_manifest name =
    List.exists
      (fun sec -> List.exists (fun it -> lower it.value = name) sec.items)
      pkgs_sections
  in
  let man_updates = ref [] in
  (match winget_info with
   | None -> ()
   | Some m ->
     let ids =
       Winget_parse.IdMap.fold (fun k _ acc -> k :: acc) m [] |> List.sort String.compare
     in
     List.iter
       (fun id ->
          let inf = Winget_parse.IdMap.find id m in
          if inf.available = "" || inf.version = inf.available
          then ()
          else if value_in_manifest (lower id)
          then ()
          else (
            let pkg_id = if inf.id = "" then id else inf.id in
            man_updates
            := { typ = Winget
               ; value = pkg_id
               ; installed_version = inf.version
               ; available_version = inf.available
               ; status = NeedsUpdate
               ; upstream = ""
               ; quarantine_days = 0
               }
               :: !man_updates))
       ids);
  (* 4. Split manifest updates and installed apps out, preserving name
     and order: updates join "Pending updates" up top, installed rows
     join the trailing "Installed" section. *)
  let man_rest = ref [] in
  let done_items = ref [] in
  List.iter
    (fun sec ->
       let rest_items = ref [] in
       List.iter
         (fun it ->
            if it.status = NeedsUpdate
            then man_updates := it :: !man_updates
            else if it.status = Installed
            then done_items := it :: !done_items
            else rest_items := it :: !rest_items)
         sec.items;
       let rest_items = List.rev !rest_items in
       if rest_items <> [] then man_rest := { sec with items = rest_items } :: !man_rest)
    pkgs_sections;
  (* NOTE: man_updates and done_items accumulate in reverse in steps 3
     and 4. Go appends non-manifest updates first, then manifest updates
     in section order; reversing once at the end reproduces exactly that.
     Installed rows likewise un-reverse into section order. *)
  let man_updates = List.rev !man_updates in
  let done_items = List.rev !done_items in
  (* 5. Newly-detected installed apps. *)
  let pkg_names : (string, unit) Hashtbl.t = Hashtbl.create 64 in
  List.iter
    (fun sec ->
       List.iter (fun it -> Hashtbl.replace pkg_names (lower it.value) ()) sec.items)
    pkgs_sections;
  let new_items = ref [] in
  (match winget_info with
   | None -> ()
   | Some m ->
     let ids =
       Winget_parse.IdMap.fold (fun k _ acc -> k :: acc) m [] |> List.sort String.compare
     in
     List.iter
       (fun id ->
          if Hashtbl.mem pkg_names (lower id)
          then ()
          else (
            let inf = Winget_parse.IdMap.find id m in
            let pkg_id = if inf.id = "" then id else inf.id in
            if
              String.contains pkg_id '\t'
              || Strutil.contains_double_space pkg_id
              || Strutil.contains_substring "ARP\\" pkg_id
            then ()
            else (
              let avail =
                if inf.available <> "" && inf.version <> inf.available
                then inf.available
                else ""
              in
              new_items
              := { typ = Winget
                 ; value = pkg_id
                 ; installed_version = inf.version
                 ; available_version = avail
                 ; status = New
                 ; upstream = ""
                 ; quarantine_days = 0
                 }
                 :: !new_items)))
       ids);
  (* 5b. Registry orphans: uninstall entries matching no manifest row
     join Newly detected as discovery-only [Registry] rows (they carry
     no install id, so append/export skip them). Suppressed when the
     entry already covers a manifest row or a winget orphan — same
     token rule as the rescue, so one machine app yields one row. *)
  let man_nicks =
    List.concat_map
      (fun sec -> List.map (fun it -> toks_of_words (nick it)) sec.items)
      pkgs_sections
  in
  let new_nicks = List.map (fun it -> toks_of_words (nick it)) !new_items in
  List.iter
    (fun (a : app) ->
       if a.pm <> "registry"
       then ()
       else (
         let toks = toks_of_words a.name in
         let covered =
           Hashtbl.mem pkg_names (lower a.name)
           || List.exists (fun small -> subset small toks) man_nicks
           || List.exists (fun small -> subset small toks) new_nicks
         in
         if not covered
         then
           new_items
           := { typ = Registry
              ; value = a.name
              ; installed_version = a.version
              ; available_version = ""
              ; status = New
              ; upstream = ""
              ; quarantine_days = 0
              }
              :: !new_items))
    apps;
  let new_items = List.rev !new_items in
  (* 6. Final order, minus the raw "winget" artifact section.
     man_rest was consed in section order, so it is reversed back here.
     Installed trails the table so the run ends on green. *)
  let sections : section list ref = ref [] in
  if man_updates <> []
  then sections := [ { name = "Pending updates"; items = (man_updates : item list) } ];
  if new_items <> []
  then sections := !sections @ [ { name = "Newly detected"; items = new_items } ];
  sections := !sections @ List.rev !man_rest;
  if done_items <> []
  then
    sections := !sections @ [ { name = "Installed"; items = (done_items : item list) } ];
  List.filter
    (fun (sec : section) -> String.lowercase_ascii sec.name <> "winget")
    !sections
;;

(** Plain-text dashboard, copyable with the terminal's native scroll.
    Empty sections and "Newly detected" are skipped, including the
    hidden-section wart (open nit: show vs drop). *)
let render (sections : section list) : string =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "DEVKIT\n";
  Buffer.add_string buf "! update   + not installed   ~ manual\n\n";
  List.iter
    (fun sec ->
       if sec.items = [] || String.lowercase_ascii sec.name = "newly detected"
       then ()
       else (
         Buffer.add_string buf (String.uppercase_ascii sec.name ^ "\n");
         List.iter
           (fun it ->
              Buffer.add_string
                buf
                (Printf.sprintf
                   "  [%s] %s%s\n"
                   (status_symbol it.status)
                   it.value
                   (format_ver it)))
           sec.items;
         Buffer.add_char buf '\n'))
    sections;
  Buffer.contents buf
;;
