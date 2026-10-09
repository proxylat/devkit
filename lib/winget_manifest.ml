(** Community winget-pkgs manifest lookup via the GitHub contents API.

    HTTP goes through an injectable {!Fetch.fetch}; JSON through yojson.
    Errors are strings; nothing here raises. *)

(** Same header policy as {!Gh}: UA + optional [GH_TOKEN] bearer. *)
let headers () =
  [ "Accept", "application/vnd.github+json"; "User-Agent", "devkit/1.0" ]
  @
  match Sys.getenv_opt "GH_TOKEN" with
  | None | Some "" -> []
  | Some tok -> [ "Authorization", "Bearer " ^ tok ]
;;

let dir_url (id : string) : string =
  let first =
    if String.length id = 0 then "" else String.make 1 (Char.lowercase_ascii id.[0])
  in
  let path = String.split_on_char '.' id |> String.concat "/" in
  Printf.sprintf
    "https://api.github.com/repos/microsoft/winget-pkgs/contents/manifests/%s/%s"
    first
    path
;;

let member_string (j : Yojson.Basic.t) (field : string) : string =
  Yojson.Basic.Util.(j |> member field |> to_string_option |> Option.value ~default:"")
;;

let list_versions (fetch : Fetch.fetch) (id : string) : (string list, string) result =
  match fetch ~timeout_s:15 (dir_url id) (headers ()) with
  | Error e -> Error e
  | Ok body ->
    (try
       let entries = Yojson.Basic.Util.to_list (Yojson.Basic.from_string body) in
       let names =
         List.filter_map
           (fun e ->
              let typ = member_string e "type" in
              let name = member_string e "name" in
              if typ = "dir" && name <> "" then Some name else None)
           entries
       in
       Ok names
     with
     | Yojson.Json_error msg -> Error ("decode: " ^ msg)
     | Yojson.Basic.Util.Type_error (msg, _) -> Error ("decode: " ^ msg))
;;

let pick_max (versions : string list) : string option =
  List.fold_left
    (fun acc v ->
       match acc with
       | None -> Some v
       | Some cur -> if Strutil.is_newer_version cur v then Some v else Some cur)
    None
    versions
;;

(** Strip one layer of matching surrounding quotes, if present. *)
let strip_quotes (s : string) : string =
  let n = String.length s in
  if n >= 2 && ((s.[0] = '"' && s.[n - 1] = '"') || (s.[0] = '\'' && s.[n - 1] = '\''))
  then String.sub s 1 (n - 2) |> String.trim
  else s
;;

(** First whitespace-delimited field (YAML trailing comments ignored). *)
let first_token (s : string) : string =
  let n = String.length s in
  let j = ref 0 in
  while !j < n && s.[!j] <> ' ' && s.[!j] <> '\t' && s.[!j] <> '\r' do
    incr j
  done;
  String.sub s 0 !j
;;

(** Remainder after an http(s) scheme, or [None]. *)
let strip_scheme (s : string) : string option =
  let low = String.lowercase_ascii s in
  let is_prefix p =
    String.length s >= String.length p && String.sub low 0 (String.length p) = p
  in
  if is_prefix "https://"
  then Some (String.sub s 8 (String.length s - 8))
  else if is_prefix "http://"
  then Some (String.sub s 7 (String.length s - 7))
  else None
;;

(** Host = text between scheme and the next ['/' | '?' | '#']. *)
let host_of_url (url : string) : string option =
  match strip_scheme url with
  | None -> None
  | Some rest ->
    let n = String.length rest in
    let j = ref 0 in
    while !j < n && rest.[!j] <> '/' && rest.[!j] <> '?' && rest.[!j] <> '#' do
      incr j
    done;
    let host = String.sub rest 0 !j |> String.lowercase_ascii in
    if host = "" then None else Some host
;;

let installer_hosts (body : string) : string list =
  String.split_on_char '\n' body
  |> List.filter_map (fun line ->
    match String.index_opt line ':' with
    | None -> None
    | Some i ->
      let key = String.sub line 0 i |> String.trim |> String.lowercase_ascii in
      if key <> "installerurl"
      then None
      else (
        let value =
          String.sub line (i + 1) (String.length line - i - 1)
          |> String.trim
          |> strip_quotes
          |> first_token
        in
        host_of_url value))
  |> List.sort_uniq String.compare
;;

let is_installer_yaml (name : string) : bool =
  let suffix = ".installer.yaml" in
  let low = String.lowercase_ascii name in
  String.length low >= String.length suffix
  && String.sub low (String.length low - String.length suffix) (String.length suffix)
     = suffix
;;

let hosts_of (fetch : Fetch.fetch) (id : string) (version : string)
  : (string list, string) result
  =
  let url = dir_url id ^ "/" ^ version in
  match fetch ~timeout_s:15 url (headers ()) with
  | Error e -> Error e
  | Ok body ->
    (try
       let entries = Yojson.Basic.Util.to_list (Yojson.Basic.from_string body) in
       let candidates =
         entries
         |> List.filter_map (fun e ->
           let typ = member_string e "type" in
           let name = member_string e "name" in
           let dl = member_string e "download_url" in
           if typ = "file" && name <> "" && is_installer_yaml name
           then Some (name, dl)
           else None)
         |> List.sort (fun (a, _) (b, _) -> String.compare a b)
       in
       match candidates with
       | [] -> Error (Printf.sprintf "no .installer.yaml found for %s %s" id version)
       | (_, "") :: _ -> Error (Printf.sprintf "no download_url for %s %s" id version)
       | (_, dl) :: _ ->
         (match fetch ~timeout_s:15 dl (headers ()) with
          | Error e -> Error e
          | Ok yaml -> Ok (installer_hosts yaml))
     with
     | Yojson.Json_error msg -> Error ("decode: " ^ msg)
     | Yojson.Basic.Util.Type_error (msg, _) -> Error ("decode: " ^ msg))
;;
