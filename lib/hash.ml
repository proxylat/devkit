(** SHA-256 file hashing and verification (digestif backend).

    Verification is explicit: [verify_file] takes the expected hex digest
    and reports a mismatch as an error. Callers that genuinely have no
    hash (winget bundle, GitHub assets) say so via [No_expected]. *)

type expectation =
  | No_expected
  | Hex of string

(** SHA-256 hex digest of a file's contents. *)
let sha256_file (path : string) : (string, string) result =
  try
    let ic = open_in_bin path in
    let finally () = close_in_noerr ic in
    let h = ref Digestif.SHA256.empty in
    try
      let buf = Bytes.create 65536 in
      let rec loop () =
        let n = input ic buf 0 65536 in
        if n = 0
        then ()
        else (
          h := Digestif.SHA256.feed_bytes !h buf ~off:0 ~len:n;
          loop ())
      in
      loop ();
      finally ();
      Ok (Digestif.SHA256.to_hex (Digestif.SHA256.get !h))
    with
    | e ->
      finally ();
      Error (Printexc.to_string e)
  with
  | Sys_error msg -> Error msg
;;

(** Compare [expected] against [path]'s digest. [No_expected] passes (with
    the caller's knowledge); [Hex ""] is rejected: an empty hash never
    verifies, closing the Go hole where [""] silently skipped the check. *)
let verify_file (expected : expectation) (path : string) : (unit, string) result =
  match expected with
  | No_expected -> Ok ()
  | Hex "" -> Error "refusing to verify against an empty hash"
  | Hex want ->
    (match sha256_file path with
     | Error e -> Error e
     | Ok got ->
       if String.lowercase_ascii got = String.lowercase_ascii want
       then Ok ()
       else Error (Printf.sprintf "hash mismatch: expected %s, got %s" want got))
;;

(** Download [url] into a fresh temp dir, verify, return the file path.
    Filename comes from the URL's last segment (query string stripped:
    keeping it would produce names like [x.msix?a=b]). *)
let download_and_verify (fetch : Fetch.fetch) (expectation : expectation) (url : string)
  : (string, string) result
  =
  let base =
    match String.rindex_opt url '/' with
    | None -> "download"
    | Some i ->
      let tail = String.sub url (i + 1) (String.length url - i - 1) in
      (match String.index_opt tail '?' with
       | None -> tail
       | Some q -> String.sub tail 0 q)
  in
  let base = if base = "" then "download" else base in
  let dir = Filename.temp_dir "devkit-" "" in
  let path = Filename.concat dir base in
  match fetch url [] with
  | Error e -> Error e
  | Ok body ->
    (try
       let oc = open_out_bin path in
       output_string oc body;
       close_out oc;
       Ok ()
     with
     | Sys_error msg -> Error msg)
    |> (function
     | Error e -> Error e
     | Ok () ->
       (match verify_file expectation path with
        | Ok () -> Ok path
        | Error e ->
          (try
             Sys.remove path;
             Unix.rmdir dir
           with
           | _ -> ());
          Error e))
;;
