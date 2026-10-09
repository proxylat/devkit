(** Scan-result disk cache: JSON snapshot of the last scan so the TUI
    can paint before the fresh scan finishes. *)

type snapshot =
  { apps : Dashboard.app list
  ; info : (string * string * string * string) list
  ; saved_at : float
  }

let cache_path () : string option =
  match Plugin.config_file () with
  | None -> None
  | Some f -> Some (Filename.concat (Filename.dirname f) "scan-cache.json")
;;

let to_info_map (rows : (string * string * string * string) list)
  : Winget_parse.info Winget_parse.IdMap.t
  =
  List.fold_left
    (fun acc (id, name, version, available) ->
       Winget_parse.IdMap.add
         (String.lowercase_ascii id)
         Winget_parse.{ id; name; version; available }
         acc)
    Winget_parse.IdMap.empty
    rows
;;

let version = 1

let app_to_json (a : Dashboard.app) : Yojson.Basic.t =
  `Assoc [ "name", `String a.name; "version", `String a.version; "pm", `String a.pm ]
;;

let info_to_json (i : Winget_parse.info) : Yojson.Basic.t =
  `Assoc
    [ "id", `String i.id
    ; "name", `String i.name
    ; "version", `String i.version
    ; "available", `String i.available
    ]
;;

let save
      (path : string)
      (apps : Dashboard.app list)
      (info : Winget_parse.info Winget_parse.IdMap.t option)
  : unit
  =
  try
    let info_rows =
      match info with
      | None -> []
      | Some m -> Winget_parse.IdMap.bindings m |> List.map (fun (_, i) -> info_to_json i)
    in
    let json =
      `Assoc
        [ "version", `Int version
        ; "saved_at", `Float (Unix.gettimeofday ())
        ; "apps", `List (List.map app_to_json apps)
        ; "info", `List info_rows
        ]
    in
    Yojson.Basic.to_file path json
  with
  | _ -> ()
;;

let field_string (fields : (string * Yojson.Basic.t) list) (key : string) : string =
  match List.assoc_opt key fields with
  | Some (`String s) -> s
  | _ -> raise Exit
;;

let app_of_json (json : Yojson.Basic.t) : Dashboard.app =
  match json with
  | `Assoc f ->
    Dashboard.
      { name = field_string f "name"
      ; version = field_string f "version"
      ; pm = field_string f "pm"
      }
  | _ -> raise Exit
;;

let info_of_json (json : Yojson.Basic.t) : string * string * string * string =
  match json with
  | `Assoc f ->
    ( field_string f "id"
    , field_string f "name"
    , field_string f "version"
    , field_string f "available" )
  | _ -> raise Exit
;;

let load (path : string) : snapshot option =
  try
    match Yojson.Basic.from_file path with
    | `Assoc top ->
      let ver =
        match List.assoc_opt "version" top with
        | Some (`Int n) -> n
        | _ -> raise Exit
      in
      if ver <> version
      then None
      else (
        let saved_at =
          match List.assoc_opt "saved_at" top with
          | Some (`Float f) -> f
          | Some (`Int n) -> float_of_int n
          | _ -> raise Exit
        in
        let apps =
          match List.assoc_opt "apps" top with
          | Some (`List l) -> List.map app_of_json l
          | _ -> raise Exit
        in
        let info =
          match List.assoc_opt "info" top with
          | Some (`List l) -> List.map info_of_json l
          | _ -> raise Exit
        in
        Some { apps; info; saved_at })
    | _ -> None
  with
  | _ -> None
;;
