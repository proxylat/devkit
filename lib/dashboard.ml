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

(** Merge the machine scan with manifest sections into dashboard sections:
    1. "Pending updates": manifest items needing update, plus
       non-manifest installed apps that have an Available version.
    2. "Newly detected": other installed apps missing from the manifest.
    3. The manifest sections minus their updated items.
    A manifest section literally named "winget" is dropped (it is a
    leftover artifact, never real content). *)
let build_sections
      ?(show : string -> string option = fun _ -> None)
      (apps : app list)
      (pkgs_sections : section list)
      (winget_info : Winget_parse.info Winget_parse.IdMap.t option)
  : section list
  =
  let lower s = String.lowercase_ascii s in
  let scan_lookup : (string, app) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun a -> Hashtbl.replace scan_lookup (lower a.name) a) apps;
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
                    match Hashtbl.find_opt scan_lookup name with
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
                    | GitHub | Url | Pm _ -> { it with status = Manual }
                  in
                  let it =
                    if it.status <> Installed && it.status <> NeedsUpdate
                    then (
                      match Hashtbl.find_opt scan_lookup name with
                      | Some a ->
                        { it with installed_version = a.version; status = Installed }
                      | None -> it)
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
  (* 2. `winget show` fallback for NotFound winget items (sequential here;
     Go fans out to 8 workers; parallelism can return if this proves slow). *)
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
                    match show it.value with
                    | None -> it
                    | Some ver ->
                      let it = { it with available_version = ver } in
                      let name = lower it.value in
                      (match Hashtbl.find_opt scan_lookup name with
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
               }
               :: !man_updates))
       ids);
  (* 4. Split manifest updates out, preserving name and order. *)
  let man_rest = ref [] in
  List.iter
    (fun sec ->
       let rest_items = ref [] in
       List.iter
         (fun it ->
            if it.status = NeedsUpdate
            then man_updates := it :: !man_updates
            else rest_items := it :: !rest_items)
         sec.items;
       let rest_items = List.rev !rest_items in
       if rest_items <> [] then man_rest := { sec with items = rest_items } :: !man_rest)
    pkgs_sections;
  (* NOTE: man_updates is accumulated in reverse in steps 3 and 4. Go appends
     non-manifest updates first, then manifest updates in section order;
     reversing once at the end reproduces exactly that. *)
  let man_updates = List.rev !man_updates in
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
                 }
                 :: !new_items)))
       ids);
  let new_items = List.rev !new_items in
  (* 6. Final order, minus the raw "winget" artifact section.
     man_rest was consed in section order, so it is reversed back here. *)
  let sections : section list ref = ref [] in
  if man_updates <> []
  then sections := [ { name = "Pending updates"; items = (man_updates : item list) } ];
  if new_items <> []
  then sections := !sections @ [ { name = "Newly detected"; items = new_items } ];
  sections := !sections @ List.rev !man_rest;
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
