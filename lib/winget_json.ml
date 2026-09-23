(** winget import-compatible JSON export.

    Mirrors what [winget export -o] emits (schema
    [aka.ms/winget-packages.schema.2.0.json]) so [devkit export] output can
    be restored natively with [winget import -i]. Only winget-tracked apps
    are included; npm/pipx/uv/cargo rows have no winget id and are
    skipped. Versions are always the installed ones ([--include-versions]
    behavior); [winget import --ignore-versions] floats them when wanted.
    The timestamp is injected ([?now]) to keep this pure and testable. *)

open Dashboard

(** SourceDetails verbatim from a real [winget export] on Windows. *)
let source_details =
  `Assoc
    [ "Name", `String "winget"
    ; "Identifier", `String "Microsoft.Winget.Source_8wekyb3d8bbwe"
    ; "Argument", `String "https://cdn.winget.microsoft.com/cache"
    ; "Type", `String "Microsoft.PreIndexed.Package"
    ]
;;

let iso8601_utc (t : float) : string =
  let tm = Unix.gmtime t in
  Printf.sprintf
    "%04d-%02d-%02dT%02d:%02d:%02d.000-00:00"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon + 1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec
;;

let to_json ?(now : float = Unix.gettimeofday ()) (apps : app list) : Yojson.Basic.t =
  let pkgs =
    List.filter_map
      (fun (a : app) ->
         if a.pm = "winget" && a.name <> ""
         then (
           let fields =
             ("PackageIdentifier", `String a.name)
             :: (if a.version <> "" then [ "Version", `String a.version ] else [])
           in
           Some (`Assoc fields))
         else None)
      apps
  in
  `Assoc
    [ "$schema", `String "https://aka.ms/winget-packages.schema.2.0.json"
    ; "CreationDate", `String (iso8601_utc now)
    ; ( "Sources"
      , `List [ `Assoc [ "SourceDetails", source_details; "Packages", `List pkgs ] ] )
    ]
;;

let to_string ?now (apps : app list) : string =
  Yojson.Basic.pretty_to_string (to_json ?now apps) ^ "\n"
;;
