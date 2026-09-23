(** User-defined package-manager tools.

    A tool is a named program devkit can scan (like the built-in npm,
    pipx, uv and cargo support) plus optional install/upgrade command
    templates. Tools live in s-expression files: [tools.sexp] next to
    [pkgs.txt] first, then the user config dir: the first file wins per
    tool name, and a custom tool may not shadow a built-in name.

    File format:

    {v
    (tools
     (tool
      (name mytool)
      (prog mytool)
      (list --list --short)
      (skip_prefixes ("- " "WARN"))
      (skip_res ("^\\s*$"))
      (row_re "^(\\S+)\\s+v?([0-9][^ ]*)")
      (strip_version_prefix v)
      (install (mytool install {id}))
      (upgrade (mytool upgrade {id}))))
    v}

    Only [name], [prog] and [row_re] are required. [row_re] is a PCRE
    with group 1 = tool name, group 2 = version (optional: a missing
    group means unversioned). Unknown fields are rejected, so a
    misspelled key is an error rather than a silent behavior change.
    S-expression [; comments] are allowed.

    Template variables: [{id}] only. Anything else in braces is an
    error at install time. *)

open Sexplib.Sexp

(** How to read [name]/[version] rows out of a tool's list output. *)
type row_spec =
  { skip_prefixes : string list
  ; skip_res : Re.re list
  ; row_re : Re.re
  ; strip_version_prefix : string
  }

(** A package-manager tool: scan program plus optional install/upgrade
    templates. [parse] turns list output into apps tagged with [name]. *)
type tool =
  { name : string
  ; prog : string
  ; list_args : string list
  ; install : string list option
  ; upgrade : string list option
  ; parse : string -> Dashboard.app list
  }

let starts_with ~prefix s =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix
;;

let strip_prefix ~prefix s =
  if prefix <> "" && starts_with ~prefix s
  then String.sub s (String.length prefix) (String.length s - String.length prefix)
  else s
;;

(** Row matching: blank lines, prefix/re skips, then [row_re] group 1/2.
    Lines that match nothing are ignored (like every built-in parser). *)
let parse_rows (spec : row_spec) (pm : string) (output : string) : Dashboard.app list =
  let lines = String.split_on_char '\n' output in
  List.filter_map
    (fun raw ->
       let line = String.trim raw in
       if line = ""
       then None
       else if List.exists (fun p -> starts_with ~prefix:p line) spec.skip_prefixes
       then None
       else if List.exists (fun re -> Re.execp re line) spec.skip_res
       then None
       else (
         match Re.exec_opt spec.row_re line with
         | None -> None
         | Some g ->
           (match Re.Group.get g 1 with
            | exception Not_found -> None
            | "" -> None
            | name ->
              let version =
                match Re.Group.get g 2 with
                | exception Not_found -> ""
                | v -> strip_prefix ~prefix:spec.strip_version_prefix v
              in
              Some { Dashboard.name; version; pm })))
    lines
;;

(** Expand [{key}] placeholders from [vars]. Unknown or unclosed
    placeholders are errors; they never pass through as literal text. *)
let expand (vars : (string * string) list) (argv : string list)
  : (string list, string) result
  =
  let expand_arg arg =
    let buf = Buffer.create (String.length arg) in
    let n = String.length arg in
    let rec go i =
      if i >= n
      then Ok (Buffer.contents buf)
      else if arg.[i] = '{'
      then (
        match String.index_from_opt arg (i + 1) '}' with
        | None -> Error ("unclosed '{' in template: " ^ arg)
        | Some j ->
          let key = String.sub arg (i + 1) (j - i - 1) in
          (match List.assoc_opt key vars with
           | None -> Error ("unknown placeholder {" ^ key ^ "} in template: " ^ arg)
           | Some v ->
             Buffer.add_string buf v;
             go (j + 1)))
      else (
        Buffer.add_char buf arg.[i];
        go (i + 1))
    in
    go 0
  in
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | a :: rest ->
      (match expand_arg a with
       | Error _ as e -> e
       | Ok s -> go (s :: acc) rest)
  in
  go [] argv
;;

(* s-expression loading: errors surface, nothing is skipped silently. *)

type fields = (string * string list) list

let err source ctx msg = Error (Printf.sprintf "%s: %s: %s" source ctx msg)

(** Split [(key value...)] entries; a bare atom value counts as one item,
    and a single nested list counts as the item list (so [(list a b)]
    and [(list (a b))] mean the same thing). Deeper nesting is rejected. *)
let assoc_of_sexp ~source ~ctx (sxp : t) : (fields, string) result =
  let value_strings v =
    match v with
    | Atom s -> Ok [ s ]
    | List items ->
      let rec go acc = function
        | [] -> Ok (List.rev acc)
        | Atom s :: rest -> go (s :: acc) rest
        | List _ :: _ -> err source ctx "expected atom or flat list"
      in
      go [] items
  in
  match sxp with
  | Atom _ -> err source ctx "expected (key value ...) entries"
  | List items ->
    let rec go acc = function
      | [] -> Ok (List.rev acc)
      | List (Atom k :: vs) :: rest ->
        let vs =
          match vs with
          | [ List inner ] -> inner
          | _ -> vs
        in
        (match value_strings (List vs) with
         | Error _ as e -> e
         | Ok ss -> go ((k, ss) :: acc) rest)
      | _ :: _ -> err source ctx "expected (key value ...) entries"
    in
    go [] items
;;

let allowed_keys =
  [ "name"
  ; "prog"
  ; "list"
  ; "skip_prefixes"
  ; "skip_res"
  ; "row_re"
  ; "strip_version_prefix"
  ; "install"
  ; "upgrade"
  ]
;;

let one ~source ~ctx key (fields : fields) : (string option, string) result =
  match List.assoc_opt key fields with
  | None -> Ok None
  | Some [ s ] -> Ok (Some s)
  | Some _ -> err source ctx (key ^ ": expected a single value")
;;

let many key (fields : fields) : string list =
  match List.assoc_opt key fields with
  | None -> []
  | Some ss -> ss
;;

let compile_re ~source ~ctx pattern =
  try Ok (Re.compile (Re.Pcre.re pattern)) with
  | e -> err source ctx ("bad regex " ^ pattern ^ ": " ^ Printexc.to_string e)
;;

let compile_many ~source ~ctx patterns =
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | p :: rest ->
      (match compile_re ~source ~ctx p with
       | Error _ as e -> e
       | Ok re -> go (re :: acc) rest)
  in
  go [] patterns
;;

let tool_of_sexp ~source ~index (sxp : t) : (tool, string) result =
  let ctx = "tool #" ^ string_of_int index in
  match sxp with
  | Atom _ -> err source ctx "expected (tool ...)"
  | List (Atom "tool" :: rest) ->
    (match assoc_of_sexp ~source ~ctx (List rest) with
     | Error _ as e -> e
     | Ok fields ->
       (match List.find_opt (fun (k, _) -> not (List.mem k allowed_keys)) fields with
        | Some (k, _) -> err source ctx ("unknown field: " ^ k)
        | None ->
          let ctx =
            match List.assoc_opt "name" fields with
            | Some [ n ] -> "tool " ^ n
            | _ -> ctx
          in
          (match one ~source ~ctx "name" fields, one ~source ~ctx "prog" fields with
           | Ok (Some name), Ok (Some prog) when name <> "" && prog <> "" ->
             (match one ~source ~ctx "row_re" fields with
              | Ok (Some pattern) ->
                (match compile_re ~source ~ctx pattern with
                 | Error _ as e -> e
                 | Ok row_re ->
                   (match compile_many ~source ~ctx (many "skip_res" fields) with
                    | Error _ as e -> e
                    | Ok skip_res ->
                      (match one ~source ~ctx "strip_version_prefix" fields with
                       | Error _ as e -> e
                       | Ok strip_opt ->
                         let spec =
                           { skip_prefixes = many "skip_prefixes" fields
                           ; skip_res
                           ; row_re
                           ; strip_version_prefix = Option.value strip_opt ~default:""
                           }
                         in
                         let opt = function
                           | [] -> None
                           | ss -> Some ss
                         in
                         Ok
                           { name
                           ; prog
                           ; list_args = many "list" fields
                           ; install = opt (many "install" fields)
                           ; upgrade = opt (many "upgrade" fields)
                           ; parse = parse_rows spec name
                           })))
              | Ok None -> err source ctx "missing required field: row_re"
              | Error _ as e -> e)
           | Ok _, Ok _ -> err source ctx "missing required fields: name and prog"
           | (Error _ as e), _ | _, (Error _ as e) -> e)))
  | List _ -> err source ctx "expected (tool ...)"
;;

(** Parse one whole file. The [(tools ...)] wrapper is required. *)
let load_string ~source (text : string) : (tool list, string) result =
  let sxp =
    try Ok (Sexplib.Sexp.of_string text) with
    | e -> err source "file" ("parse error: " ^ Printexc.to_string e)
  in
  match sxp with
  | Error _ as e -> e
  | Ok (List (Atom "tools" :: rest)) ->
    let rec go i acc = function
      | [] -> Ok (List.rev acc)
      | s :: tl ->
        (match tool_of_sexp ~source ~index:i s with
         | Error _ as e -> e
         | Ok t -> go (i + 1) (t :: acc) tl)
    in
    go 1 [] rest
  | Ok _ -> err source "file" "expected (tools (tool ...) ...)"
;;

(** A missing file means no custom tools; a broken one is an error. *)
let load_file ~read path : (tool list, string) result =
  match read path with
  | None -> Ok []
  | Some text -> load_string ~source:path text
;;

let lower s = String.lowercase_ascii s

(** Load several files in order (cwd first, config second); the first
    file wins per tool name. [reserved] names (built-ins) are rejected,
    since shadowing a built-in would silently change its scan. *)
let load_many ~read ?(reserved : string list = []) (paths : string list)
  : (tool list, string) result
  =
  let reserved = List.map lower reserved in
  let rec go acc = function
    | [] -> Ok (List.rev acc)
    | p :: rest ->
      (match load_file ~read p with
       | Error _ as e -> e
       | Ok tools ->
         let rec add acc = function
           | [] -> Ok acc
           | t :: tl ->
             let n = lower t.name in
             if List.mem n reserved
             then Error (p ^ ": tool " ^ t.name ^ " shadows a built-in")
             else if List.exists (fun u -> lower u.name = n) acc
             then add acc tl
             else add (t :: acc) tl
         in
         (match add acc tools with
          | Error _ as e -> e
          | Ok acc -> go acc rest))
  in
  go [] paths
;;

(** Case-insensitive lookup. *)
let find (name : string) (tools : tool list) : tool option =
  List.find_opt (fun t -> lower t.name = lower name) tools
;;

(** Config-file slot: [$XDG_CONFIG_HOME/devkit], [$HOME/.config/devkit],
    [%LOCALAPPDATA%/devkit] on Windows, else nothing. *)
let config_file () : string option =
  match Sys.getenv_opt "XDG_CONFIG_HOME" with
  | Some d when d <> "" ->
    Some (Filename.concat (Filename.concat d "devkit") "tools.sexp")
  | _ ->
    (match Sys.getenv_opt "HOME" with
     | Some h when h <> "" ->
       Some
         (Filename.concat
            (Filename.concat (Filename.concat h ".config") "devkit")
            "tools.sexp")
     | _ ->
       (match Sys.getenv_opt "LOCALAPPDATA" with
        | Some l when l <> "" ->
          Some (Filename.concat (Filename.concat l "devkit") "tools.sexp")
        | _ -> None))
;;

(** Search order: [tools.sexp] next to [pkgs.txt], then the config file. *)
let default_paths () : string list =
  match config_file () with
  | None -> [ "tools.sexp" ]
  | Some c -> [ "tools.sexp"; c ]
;;
