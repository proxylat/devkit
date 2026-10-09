(** Tests for {!Devkit.Verify}: Authenticode status mapping and
    checksum-file comparison (SecDoc status table). *)
open Devkit

let is_pass = function
  | Verify.Pass -> true
  | _ -> false
;;

let warn_text = function
  | Verify.Warn w -> w
  | _ -> Alcotest.fail "expected Warn"
;;

let block_text = function
  | Verify.Block m -> m
  | _ -> Alcotest.fail "expected Block"
;;

let sig_of (out : string) ?(ok = true) () =
  let seen = ref ("", []) in
  let spawn prog args =
    seen := prog, args;
    out, ok
  in
  let v = Verify.signature ~os:"Win32" ~spawn "/t/Tool.exe" in
  v, !seen
;;

let signature_table () =
  let check_pass name out =
    Alcotest.(check bool) name true (is_pass (fst (sig_of out ())))
  in
  check_pass "valid" "Valid";
  check_pass "valid with newline" "Valid\r\n";
  let w = warn_text (fst (sig_of "NotSigned" ())) in
  Alcotest.(check bool)
    "unsigned names file"
    true
    (Strutil.contains_substring "Tool.exe" w);
  let m = block_text (fst (sig_of "HashMismatch" ())) in
  Alcotest.(check bool)
    "mismatch blocks"
    true
    (Strutil.contains_substring "HashMismatch" m);
  let m = block_text (fst (sig_of "NotTrusted" ())) in
  Alcotest.(check bool)
    "untrusted blocks"
    true
    (Strutil.contains_substring "NotTrusted" m);
  Alcotest.(check bool)
    "untrusted advises manual"
    true
    (Strutil.contains_substring "manually" m);
  let w = warn_text (fst (sig_of "UnknownError: 0x80070002" ())) in
  Alcotest.(check bool) "unknown warns" true (Strutil.contains_substring "0x80070002" w);
  let w = warn_text (fst (sig_of "" ~ok:false ())) in
  Alcotest.(check bool) "spawn fail warns" true (Strutil.contains_substring "Tool.exe" w)
;;

let signature_invocation () =
  let _, (prog, args) = sig_of "Valid" () in
  Alcotest.(check string) "powershell" "powershell" prog;
  Alcotest.(check bool)
    "one-liner"
    true
    (List.exists (fun a -> Strutil.contains_substring "Get-AuthenticodeSignature" a) args)
;;

let signature_skipped_off_windows () =
  let v =
    Verify.signature
      ~os:"Unix"
      ~spawn:(fun _ _ -> Alcotest.fail "must not spawn")
      "/t/Tool.exe"
  in
  Alcotest.(check bool) "passes silently" true (is_pass v)
;;

let signer_of (out : string) ?(ok = true) () =
  let seen = ref ("", []) in
  let spawn prog args =
    seen := prog, args;
    out, ok
  in
  let r = Verify.signer ~os:"Win32" ~spawn "/t/Tool.exe" in
  r, !seen
;;

let signer_table () =
  let r, (prog, args) = signer_of "  ABCDEF1234\r\n" () in
  Alcotest.(check (option string)) "trimmed thumbprint" (Some "ABCDEF1234") r;
  Alcotest.(check string) "powershell" "powershell" prog;
  Alcotest.(check bool)
    "thumbprint one-liner"
    true
    (List.exists
       (fun a -> Strutil.contains_substring "SignerCertificate.Thumbprint" a)
       args);
  Alcotest.(check (option string)) "empty -> none" None (fst (signer_of "" ()));
  Alcotest.(check (option string)) "blank -> none" None (fst (signer_of "  \r\n" ()));
  Alcotest.(check (option string))
    "spawn fail -> none"
    None
    (fst (signer_of "ABC" ~ok:false ()))
;;

let signer_skipped_off_windows () =
  let r =
    Verify.signer
      ~os:"Unix"
      ~spawn:(fun _ _ -> Alcotest.fail "must not spawn")
      "/t/Tool.exe"
  in
  Alcotest.(check (option string)) "none silently" None r
;;

let attest ~os ~probe ~verify =
  let calls = ref [] in
  let spawn prog args =
    calls := (prog, args) :: !calls;
    match prog with
    | "gh" -> verify
    | _ -> probe
  in
  let v = Verify.attestation ~os ~spawn ~repo:"owner/repo" "/t/Tool.exe" in
  v, List.rev !calls
;;

let attestation_gh_missing () =
  let v, calls = attest ~os:"Unix" ~probe:("", false) ~verify:("", true) in
  Alcotest.(check bool) "passes" true (is_pass v);
  Alcotest.(check bool)
    "gh never invoked"
    false
    (List.exists (fun (prog, _) -> prog = "gh") calls);
  let v, _ = attest ~os:"Unix" ~probe:("  \n", true) ~verify:("", true) in
  Alcotest.(check bool) "blank probe passes" true (is_pass v)
;;

let attestation_verify_ok () =
  let v, calls =
    attest ~os:"Unix" ~probe:("/usr/bin/gh\n", true) ~verify:("verified", true)
  in
  Alcotest.(check bool) "passes" true (is_pass v);
  match calls with
  | (prog, args) :: _ ->
    Alcotest.(check string) "probe prog" "command" prog;
    Alcotest.(check (list string)) "probe args" [ "-v"; "gh" ] args
  | [] -> Alcotest.fail "expected probe call"
;;

let attestation_no_attestation () =
  let v, _ =
    attest
      ~os:"Unix"
      ~probe:("/usr/bin/gh", true)
      ~verify:("No attestation found for Tool.exe", false)
  in
  let w = warn_text v in
  Alcotest.(check bool) "names file" true (Strutil.contains_substring "Tool.exe" w);
  Alcotest.(check bool) "names repo" true (Strutil.contains_substring "owner/repo" w)
;;

let attestation_inconclusive () =
  let v, _ =
    attest ~os:"Unix" ~probe:("/usr/bin/gh", true) ~verify:("network unreachable", false)
  in
  Alcotest.(check bool)
    "quotes output"
    true
    (Strutil.contains_substring "network unreachable" (warn_text v));
  let v, _ = attest ~os:"Unix" ~probe:("/usr/bin/gh", true) ~verify:("", false) in
  Alcotest.(check bool)
    "empty says no output"
    true
    (Strutil.contains_substring "no output" (warn_text v))
;;

let attestation_win32_probe () =
  let v, calls =
    attest ~os:"Win32" ~probe:("C:\\tools\\gh.exe", true) ~verify:("", true)
  in
  Alcotest.(check bool) "passes" true (is_pass v);
  match calls with
  | (prog, args) :: _ ->
    Alcotest.(check string) "where probe" "where" prog;
    Alcotest.(check (list string)) "where args" [ "gh" ] args
  | [] -> Alcotest.fail "expected probe call"
;;

let checksum_names () =
  let yes =
    [ "SHA256SUMS"
    ; "SHA256SUMS.txt"
    ; "tool.checksums.txt"
    ; "CHECKSUMS.TXT"
    ; "tool.sha256"
    ; "Tool.SHA256"
    ]
  in
  let no = [ "tool.exe"; "checksums"; "tool.sha1"; "sums.md" ] in
  List.iter
    (fun n -> Alcotest.(check bool) ("yes " ^ n) true (Verify.is_checksum_file n))
    yes;
  List.iter
    (fun n -> Alcotest.(check bool) ("no " ^ n) false (Verify.is_checksum_file n))
    no
;;

let parse_sums_shapes () =
  let hex = String.make 64 'a' in
  let body =
    String.concat
      "\n"
      [ hex ^ "  Tool.exe"
      ; String.make 64 'b' ^ " *other.exe"
      ; "garbage line"
      ; "short  Tool.exe"
      ; ""
      ]
  in
  Alcotest.(check (list (pair string string)))
    "pairs"
    [ hex, "Tool.exe"; String.make 64 'b', "other.exe" ]
    (Verify.parse_sums body)
;;

let with_temp (content : string) (f : string -> unit) : unit =
  let path = Filename.temp_file "devkit-verify-" ".exe" in
  let oc = open_out_bin path in
  output_string oc content;
  close_out oc;
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | _ -> ())
    (fun () -> f path)
;;

let sums_asset =
  { Gh.name = "SHA256SUMS"; browser_download_url = "https://x/sums"; size = 1L }
;;

let checksum_missing_file () =
  with_temp "x" (fun path ->
    let v =
      Verify.checksum
        ~fetch:(fun ?timeout_s:_ _ _ -> Error "no net")
        []
        ~asset:"T.exe"
        ~path
    in
    Alcotest.(check bool) "warns" true (String.length (warn_text v) > 0))
;;

let checksum_fetch_fail () =
  with_temp "x" (fun path ->
    let v =
      Verify.checksum
        ~fetch:(fun ?timeout_s:_ _ _ -> Error "denied")
        [ sums_asset ]
        ~asset:"T.exe"
        ~path
    in
    Alcotest.(check bool)
      "names file"
      true
      (Strutil.contains_substring "SHA256SUMS" (warn_text v)))
;;

let checksum_match () =
  with_temp "payload" (fun path ->
    let hex =
      match Hash.sha256_file path with
      | Ok h -> h
      | Error e -> Alcotest.fail e
    in
    let v =
      Verify.checksum
        ~fetch:(fun ?timeout_s:_ _ _ -> Ok (hex ^ "  Tool.exe\n"))
        [ sums_asset ]
        ~asset:"Tool.exe"
        ~path
    in
    Alcotest.(check bool) "passes" true (is_pass v))
;;

let checksum_mismatch () =
  with_temp "payload" (fun path ->
    let v =
      Verify.checksum
        ~fetch:(fun ?timeout_s:_ _ _ -> Ok (String.make 64 '0' ^ "  Tool.exe\n"))
        [ sums_asset ]
        ~asset:"Tool.exe"
        ~path
    in
    Alcotest.(check bool)
      "blocks"
      true
      (Strutil.contains_substring "mismatch" (String.lowercase_ascii (block_text v))))
;;

let checksum_no_line () =
  with_temp "x" (fun path ->
    let v =
      Verify.checksum
        ~fetch:(fun ?timeout_s:_ _ _ -> Ok (String.make 64 '0' ^ "  Other.exe\n"))
        [ sums_asset ]
        ~asset:"Tool.exe"
        ~path
    in
    Alcotest.(check bool)
      "warns"
      true
      (Strutil.contains_substring "Tool.exe" (warn_text v)))
;;

let () =
  Alcotest.run
    "verify"
    [ ( "signature"
      , [ Alcotest.test_case "status table" `Quick signature_table
        ; Alcotest.test_case "powershell invocation" `Quick signature_invocation
        ; Alcotest.test_case "skipped off windows" `Quick signature_skipped_off_windows
        ] )
    ; ( "signer"
      , [ Alcotest.test_case "table" `Quick signer_table
        ; Alcotest.test_case "skipped off windows" `Quick signer_skipped_off_windows
        ] )
    ; ( "attestation"
      , [ Alcotest.test_case "gh missing passes" `Quick attestation_gh_missing
        ; Alcotest.test_case "verify ok passes" `Quick attestation_verify_ok
        ; Alcotest.test_case "no attestation warns" `Quick attestation_no_attestation
        ; Alcotest.test_case "other failure warns" `Quick attestation_inconclusive
        ; Alcotest.test_case "win32 where probe" `Quick attestation_win32_probe
        ] )
    ; ( "sums"
      , [ Alcotest.test_case "file names" `Quick checksum_names
        ; Alcotest.test_case "parse shapes" `Quick parse_sums_shapes
        ] )
    ; ( "checksum"
      , [ Alcotest.test_case "missing file warns" `Quick checksum_missing_file
        ; Alcotest.test_case "fetch fail warns" `Quick checksum_fetch_fail
        ; Alcotest.test_case "match passes" `Quick checksum_match
        ; Alcotest.test_case "mismatch blocks" `Quick checksum_mismatch
        ; Alcotest.test_case "no line warns" `Quick checksum_no_line
        ] )
    ]
;;
