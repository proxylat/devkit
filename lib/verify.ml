(** Post-download verification for vendor-direct release assets.

    Two layers, both warn-or-block verdicts (see SecDoc.md):

    - Authenticode signature status via PowerShell, Win32 only.
    - Upstream checksum-file comparison for every asset.

    Principle: tamper evidence blocks, missing evidence warns. *)

type verdict =
  | Pass
  | Warn of string
  | Block of string

(** Escape for PowerShell single-quoted strings: a quote doubles. *)
let ps_escape (s : string) : string =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c -> if c = '\'' then Buffer.add_string buf "''" else Buffer.add_char buf c)
    s;
  Buffer.contents buf
;;

(** Authenticode status of [path]. Non-Windows passes silently;
    anything but [Valid]/[HashMismatch]/[NotTrusted] is missing
    evidence, never tamper evidence, so it warns with the raw text. *)
let signature ?(os = Sys.os_type) ~spawn (path : string) : verdict =
  if os <> "Win32"
  then Pass
  else (
    let cmd =
      Printf.sprintf "(Get-AuthenticodeSignature -FilePath '%s').Status" (ps_escape path)
    in
    let out, ok =
      spawn "powershell" [ "-NoProfile"; "-NonInteractive"; "-Command"; cmd ]
    in
    let base = Filename.basename path in
    let text = String.trim out in
    if not ok
    then
      Warn
        (Printf.sprintf
           "%s: signature check failed (%s)"
           base
           (if text = "" then "no output" else text))
    else (
      match text with
      | "Valid" -> Pass
      | "NotSigned" ->
        Warn (Printf.sprintf "%s is unsigned; installed without signature evidence" base)
      | "HashMismatch" ->
        Block
          (Printf.sprintf
             "%s: Authenticode HashMismatch, file modified after signing"
             base)
      | "NotTrusted" ->
        Block
          (Printf.sprintf
             "%s: Authenticode NotTrusted (untrusted root); install manually if you \
              trust it"
             base)
      | _ ->
        Warn
          (Printf.sprintf
             "%s: inconclusive signature status (%s)"
             base
             (if text = "" then "empty" else text))))
;;

let starts_with (s : string) (prefix : string) : bool =
  String.length s >= String.length prefix
  && String.sub s 0 (String.length prefix) = prefix
;;

(** Checksum-file names from SecDoc: [SHA256SUMS*], [*checksums*.txt],
    [*.sha256], all case-insensitive. *)
let is_checksum_file (name : string) : bool =
  let n = String.lowercase_ascii name in
  starts_with n "sha256sums"
  || (Strutil.contains_substring "checksums" n && Filename.check_suffix n ".txt")
  || Filename.check_suffix n ".sha256"
;;

let is_hex64 (s : string) : bool =
  String.length s = 64
  && String.for_all
       (function
         | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
         | _ -> false)
       s
;;

(** Parse [hex  filename] / [hex *filename] lines into
    [(hex, filename)] pairs, skipping malformed lines (a garbled
    checksum file is missing evidence, not a mismatch). *)
let parse_sums (body : string) : (string * string) list =
  String.split_on_char '\n' body
  |> List.filter_map (fun line ->
    match
      String.split_on_char ' ' (String.trim line) |> List.filter (fun w -> w <> "")
    with
    | hex :: name :: _ when is_hex64 hex ->
      let name =
        if String.length name > 0 && name.[0] = '*'
        then String.sub name 1 (String.length name - 1)
        else name
      in
      Some (hex, name)
    | _ -> None)
;;

(** Compare [path] (downloaded as [asset]) against the release's
    checksum file. Only a hash mismatch blocks; a missing file, a
    fetch failure, or no line for our asset warns and continues. *)
let checksum
      ~(fetch : Fetch.fetch)
      (assets : Gh.asset list)
      ~(asset : string)
      ~(path : string)
  : verdict
  =
  match List.find_opt (fun (a : Gh.asset) -> is_checksum_file a.Gh.name) assets with
  | None ->
    Warn
      (Printf.sprintf
         "no checksum file published for %s; installed without hash evidence"
         asset)
  | Some sums ->
    (match fetch sums.Gh.browser_download_url [] with
     | Error e -> Warn (Printf.sprintf "could not fetch %s: %s" sums.Gh.name e)
     | Ok body ->
       let want = String.lowercase_ascii asset in
       let same (name : string) =
         let n = String.lowercase_ascii name in
         n = want || String.lowercase_ascii (Filename.basename name) = want
       in
       (match List.find_opt (fun (_, name) -> same name) (parse_sums body) with
        | None ->
          Warn
            (Printf.sprintf
               "%s has no entry for %s; installed without hash evidence"
               sums.Gh.name
               asset)
        | Some (hex, _) ->
          (match Hash.verify_file (Hash.Hex hex) path with
           | Ok () -> Pass
           | Error e -> Block (Printf.sprintf "%s: %s" asset e))))
;;

(** Delete a blocked download and its temp dir, ignoring errors
    (same cleanup {!Hash.download_and_verify} does on mismatch). *)
let discard (path : string) : unit =
  try
    Sys.remove path;
    Unix.rmdir (Filename.dirname path)
  with
  | _ -> ()
;;
