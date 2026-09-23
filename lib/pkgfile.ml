(** Manifest (pkgs.txt) parsing.

    The manifest is a sequence of [# Section] headers, [type:value] items
    and an optional [# winget: <path>] directive. Parsing is total and
    pure: unknown lines are skipped. *)

type item_type =
  | Winget
  | GitHub
  | Url

type status =
  | Installed
  | NeedsUpdate
  | NotFound
  | New
  | Manual

type item =
  { typ : item_type
  ; value : string
  ; installed_version : string
  ; available_version : string
  ; status : status
  }

type section =
  { name : string
  ; items : item list
  }

type parse_result =
  { sections : section list
  ; winget_path : string
  }

let make_item typ value =
  { typ
  ; value
  ; installed_version = ""
  ; available_version = ""
  ; (* Fresh items carry [NotFound]; statuses are assigned later by the
       merge step. *)
    status = NotFound
  }
;;

let starts_with ~prefix s =
  let n = String.length prefix in
  String.length s >= n && String.sub s 0 n = prefix
;;

(** [parse text] parses a pkgs.txt manifest. *)
let parse (text : string) : parse_result =
  let sections = ref [] in
  let winget_path = ref "" in
  let push_item it =
    match List.rev !sections with
    | [] -> sections := [ { name = "General"; items = [ it ] } ]
    | last :: rest ->
      sections := List.rev ({ last with items = last.items @ [ it ] } :: rest)
  in
  let lines = String.split_on_char '\n' text in
  List.iter
    (fun raw ->
       let line = String.trim raw in
       if line = ""
       then ()
       else if starts_with ~prefix:"#" line
       then (
         let rest = String.trim (String.sub line 1 (String.length line - 1)) in
         if starts_with ~prefix:"winget:" (String.lowercase_ascii rest)
         then (
           let p = String.trim (String.sub rest 7 (String.length rest - 7)) in
           if p <> ""
           then winget_path := p
           (* NOTE: a [# winget:]-with-empty-path falls through to become a
             section, exactly like the Go original. *)
           else sections := !sections @ [ { name = rest; items = [] } ])
         else if rest <> ""
         then sections := !sections @ [ { name = rest; items = [] } ])
       else (
         match String.index_opt line ':' with
         | None -> ()
         | Some colon ->
           let prefix = String.lowercase_ascii (String.trim (String.sub line 0 colon)) in
           let value =
             String.trim (String.sub line (colon + 1) (String.length line - colon - 1))
           in
           if value = ""
           then ()
           else (
             let typ =
               match prefix with
               | "winget" -> Some Winget
               | "github" -> Some GitHub
               | "url" -> Some Url
               | _ -> None
             in
             match typ with
             | None -> ()
             | Some typ -> push_item (make_item typ value))))
    lines;
  { sections = !sections; winget_path = !winget_path }
;;
