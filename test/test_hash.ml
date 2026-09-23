(** Tests for {!Devkit.Hash}: known vector, expectation handling, download
    naming (query-string strip). *)
open Devkit

let with_temp_file contents f =
  let path = Filename.temp_file "devkit-hash-" ".bin" in
  let oc = open_out_bin path in
  output_string oc contents;
  close_out oc;
  let r = f path in
  (try Sys.remove path with
   | _ -> ());
  r
;;

let test_sha256_abc () =
  with_temp_file "abc" (fun path ->
    match Hash.sha256_file path with
    | Error e -> Alcotest.fail e
    | Ok got ->
      Alcotest.(check string)
        "sha256(abc)"
        "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        got)
;;

let test_verify_ok_case_insensitive () =
  with_temp_file "abc" (fun path ->
    Alcotest.(check bool)
      "upper hex verifies"
      true
      (Hash.verify_file
         (Hash.Hex "BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD")
         path
       = Ok ()))
;;

let test_verify_mismatch () =
  with_temp_file "abc" (fun path ->
    match Hash.verify_file (Hash.Hex (String.make 64 '0')) path with
    | Ok () -> Alcotest.fail "expected mismatch"
    | Error e ->
      Alcotest.(check bool)
        "mismatch message"
        true
        (Strutil.contains_substring "hash mismatch" e))
;;

let test_empty_hex_refused () =
  with_temp_file "abc" (fun path ->
    match Hash.verify_file (Hash.Hex "") path with
    | Ok () -> Alcotest.fail "empty hash must never verify"
    | Error _ -> ())
;;

let test_no_expected_passes_without_fs () =
  Alcotest.(check bool)
    "No_expected passes"
    true
    (Hash.verify_file Hash.No_expected "/nonexistent/file" = Ok ())
;;

let fake_fetch body seen : Fetch.fetch =
  fun ?(timeout_s = 0) url headers ->
  ignore timeout_s;
  seen := (url, headers) :: !seen;
  Ok body
;;

let test_download_strips_query () =
  let seen = ref [] in
  match
    Hash.download_and_verify
      (fake_fetch "payload" seen)
      Hash.No_expected
      "https://example.com/dir/setup.exe?dl=1"
  with
  | Error e -> Alcotest.fail e
  | Ok path ->
    Alcotest.(check string) "query stripped" "setup.exe" (Filename.basename path);
    let ic = open_in_bin path in
    let got = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Alcotest.(check string) "body" "payload" got;
    let dir = Filename.dirname path in
    (try Sys.remove path with
     | _ -> ());
    (try Unix.rmdir dir with
     | _ -> ())
;;

let test_download_empty_tail () =
  let seen = ref [] in
  match
    Hash.download_and_verify (fake_fetch "p" seen) Hash.No_expected "https://example.com/"
  with
  | Error e -> Alcotest.fail e
  | Ok path ->
    Alcotest.(check string) "empty tail" "download" (Filename.basename path);
    let dir = Filename.dirname path in
    (try Sys.remove path with
     | _ -> ());
    (try Unix.rmdir dir with
     | _ -> ())
;;

let test_download_records_url () =
  let seen = ref [] in
  match
    Hash.download_and_verify
      (fake_fetch "p" seen)
      Hash.No_expected
      "https://example.com/a.bin"
  with
  | Error e -> Alcotest.fail e
  | Ok path ->
    Alcotest.(check string)
      "fetched url"
      "https://example.com/a.bin"
      (fst (List.hd !seen));
    let dir = Filename.dirname path in
    (try Sys.remove path with
     | _ -> ());
    (try Unix.rmdir dir with
     | _ -> ())
;;

let () =
  Alcotest.run
    "hash"
    [ ( "sha256"
      , [ Alcotest.test_case "known vector" `Quick test_sha256_abc
        ; Alcotest.test_case
            "case-insensitive verify"
            `Quick
            test_verify_ok_case_insensitive
        ; Alcotest.test_case "mismatch errors" `Quick test_verify_mismatch
        ; Alcotest.test_case "empty hex refused" `Quick test_empty_hex_refused
        ; Alcotest.test_case
            "No_expected passes"
            `Quick
            test_no_expected_passes_without_fs
        ] )
    ; ( "download"
      , [ Alcotest.test_case "strips query string" `Quick test_download_strips_query
        ; Alcotest.test_case "empty tail name" `Quick test_download_empty_tail
        ; Alcotest.test_case "fetches url" `Quick test_download_records_url
        ] )
    ]
;;
