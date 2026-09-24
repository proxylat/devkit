(** Tests for {!Devkit.Bootstrap}: portable search, PATH lookup, probes,
    cert sniffing, bundle picking, size formatting, resolution order,
    download flow (against [test/fixtures/] zips). *)
open Devkit

let base_io
      ?(getenv = fun _ -> None)
      ?(is_file = fun _ -> false)
      ?(spawn = fun _ _ -> "", false)
      ?(cwd = Ok "/nowhere")
      ?(exe = Ok "/nowhere/exe")
      ?(cache = "/cache")
      ?(latest = Error "no net")
      ?(download = fun ~url:_ -> Error "no net")
      ?(unzip = fun ~zip:_ ~dir:_ -> Error "no unzip")
      ()
  : Bootstrap.io
  =
  { Bootstrap.getenv
  ; is_file
  ; spawn
  ; cwd = (fun () -> cwd)
  ; exe_dir = (fun () -> exe)
  ; mkdir_p = (fun _ -> Ok ())
  ; cache_dir = (fun () -> cache)
  ; latest_winget_cli = (fun () -> latest)
  ; download
  ; unzip
  }
;;

(* --- find_portable --- *)

let test_portable_hit () =
  let is_file = fun p -> p = "/a/winget.exe" in
  Alcotest.(check string) "hit" "/a/winget.exe" (Bootstrap.find_portable ~is_file "/a")
;;

let test_portable_prefers_winget_exe () =
  let is_file = fun p -> p = "/a/winget.exe" || p = "/a/AppInstaller.exe" in
  Alcotest.(check string) "order" "/a/winget.exe" (Bootstrap.find_portable ~is_file "/a")
;;

let test_portable_subfolder () =
  let is_file = fun p -> p = "/a/build/winget/AppInstaller.exe" in
  Alcotest.(check string)
    "subfolder"
    "/a/build/winget/AppInstaller.exe"
    (Bootstrap.find_portable ~is_file "/a")
;;

let test_portable_walks_up () =
  let is_file = fun p -> p = "/a/winget/winget.exe" in
  Alcotest.(check string)
    "walk-up"
    "/a/winget/winget.exe"
    (Bootstrap.find_portable ~is_file "/a/b/c")
;;

let test_portable_stops_at_4 () =
  let is_file = fun p -> p = "/a/winget.exe" in
  Alcotest.(check string)
    "beyond 4 levels"
    ""
    (Bootstrap.find_portable ~is_file "/a/b/c/d/e")
;;

(* --- find_on_path --- *)

let test_on_path () =
  let getenv = function
    | "PATH" -> Some "/x;/y:/z"
    | _ -> None
  in
  let is_file = fun p -> p = "/y/winget" in
  Alcotest.(check string)
    "both separators"
    "/y/winget"
    (Bootstrap.find_on_path ~getenv ~is_file "winget")
;;

let test_on_path_miss () =
  Alcotest.(check string)
    "miss"
    ""
    (Bootstrap.find_on_path ~getenv:(fun _ -> None) ~is_file:(fun _ -> false) "winget")
;;

(* --- winget_runs --- *)

let test_runs_ok () =
  Alcotest.(check bool)
    "exit 0"
    true
    (Bootstrap.winget_runs ~spawn:(fun _ _ -> "1.2.3", true) "/w/winget.exe")
;;

let test_runs_output_despite_exit () =
  Alcotest.(check bool)
    "stderr version accepted"
    true
    (Bootstrap.winget_runs ~spawn:(fun _ _ -> "v1", false) "/w/winget.exe")
;;

let test_runs_silent_fail () =
  Alcotest.(check bool)
    "silent fail"
    false
    (Bootstrap.winget_runs ~spawn:(fun _ _ -> "", false) "/w/winget.exe")
;;

let test_runs_empty_no_spawn () =
  let called = ref false in
  let spawn _ _ =
    called := true;
    "", false
  in
  Alcotest.(check bool) "empty false" false (Bootstrap.winget_runs ~spawn "");
  Alcotest.(check bool) "no spawn" false !called
;;

(* --- resolve_via_cmd --- *)

let test_via_cmd () =
  let spawn prog args =
    match prog, args with
    | "cmd", _ -> "C:\\junk\\other.exe\r\nC:\\tools\\winget.exe\r\n", true
    | p, [ "--version" ] when p = "C:\\tools\\winget.exe" -> "1.0", true
    | _ -> "", false
  in
  Alcotest.(check string)
    "alias resolved"
    "C:\\tools\\winget.exe"
    (Bootstrap.resolve_via_cmd ~spawn)
;;

let test_via_cmd_fail () =
  Alcotest.(check string)
    "where fails"
    ""
    (Bootstrap.resolve_via_cmd ~spawn:(fun _ _ -> "", false))
;;

(* --- is_cert_error --- *)

let test_cert () =
  List.iter
    (fun msg -> Alcotest.(check bool) ("cert: " ^ msg) true (Bootstrap.is_cert_error msg))
    [ "x509: certificate signed by unknown authority"
    ; "FAILED 80072f0d InternetOpenUrl"
    ; "CERT_E_UNTRUSTEDROOT"
    ];
  List.iter
    (fun msg ->
       Alcotest.(check bool) ("not cert: " ^ msg) false (Bootstrap.is_cert_error msg))
    [ "exit status 1"; ""; "connection refused" ];
  Alcotest.(check bool)
    "advice mentions proxy CA"
    true
    (Strutil.contains_substring "Trusted Root" Bootstrap.cert_advice)
;;

(* --- find_msixbundle --- *)

let mkrel tag names =
  { Gh.tag_name = tag
  ; assets =
      List.map
        (fun n -> { Gh.name = n; browser_download_url = "https://x/" ^ n; size = 1L })
        names
  }
;;

let test_msixbundle_first () =
  let rel = mkrel "v1" [ "notes.txt"; "WinGet.MSIXBUNDLE"; "other.msixbundle" ] in
  match Bootstrap.find_msixbundle rel with
  | None -> Alcotest.fail "expected hit"
  | Some a -> Alcotest.(check string) "first wins" "WinGet.MSIXBUNDLE" a.Gh.name
;;

let test_msixbundle_none () =
  Alcotest.(check bool)
    "none"
    true
    (Bootstrap.find_msixbundle (mkrel "v1" [ "a.zip" ]) = None)
;;

(* --- format_size --- *)

let test_sizes () =
  let cases =
    [ 0L, "0 B"
    ; 999L, "999 B"
    ; 1_024L, "1.0 KB"
    ; 1_536L, "1.5 KB"
    ; 1_048_576L, "1.0 MB"
    ; 1_073_741_824L, "1.0 GB"
    ]
  in
  List.iter
    (fun (n, want) ->
       Alcotest.(check string) ("size " ^ want) want (Bootstrap.format_size n))
    cases
;;

(* --- resolve order --- *)

let test_resolve_cwd_wins_no_spawn () =
  let spawns = ref 0 in
  let io =
    base_io
      ~is_file:(fun p -> p = "/proj/winget.exe")
      ~spawn:(fun _ _ ->
        incr spawns;
        "", false)
      ~cwd:(Ok "/proj/sub")
      ()
  in
  Alcotest.(check string) "cwd portable" "/proj/winget.exe" (Bootstrap.resolve io);
  Alcotest.(check int) "no probes" 0 !spawns
;;

let test_resolve_path_fallback () =
  let io =
    base_io
      ~getenv:(function
        | "PATH" -> Some "/bin"
        | _ -> None)
      ~is_file:(fun p -> p = "/bin/winget")
      ~cwd:(Ok "/proj")
      ~exe:(Ok "/opt/app")
      ()
  in
  Alcotest.(check string) "PATH hit" "/bin/winget" (Bootstrap.resolve io)
;;

let test_resolve_miss () =
  Alcotest.(check string) "miss" "" (Bootstrap.resolve (base_io ()))
;;

(* --- ensure_full --- *)

let fresh_cache () = Bootstrap.make_path_cache ()

let test_ensure_override_blind () =
  let get, _ = fresh_cache () in
  let io = base_io ~is_file:(fun _ -> true) () in
  match
    Bootstrap.ensure_full ~override_path:"C:\\custom\\winget.exe" ~resolve_path:get io
  with
  | Error e -> Alcotest.fail e
  | Ok p -> Alcotest.(check string) "override verbatim" "C:\\custom\\winget.exe" p
;;

let test_ensure_resolved () =
  let get, _ = fresh_cache () in
  let io = base_io ~is_file:(fun p -> p = "/p/winget.exe") ~cwd:(Ok "/p") () in
  match Bootstrap.ensure_full ~override_path:"" ~resolve_path:get io with
  | Error e -> Alcotest.fail e
  | Ok p -> Alcotest.(check string) "resolved" "/p/winget.exe" p
;;

let test_ensure_download_cert_advice () =
  let get, _ = fresh_cache () in
  let io = base_io ~latest:(Error "x509: certificate signed by unknown authority") () in
  match Bootstrap.ensure_full ~override_path:"" ~resolve_path:get ~os:"Win32" io with
  | Ok _ -> Alcotest.fail "expected download error"
  | Error e ->
    Alcotest.(check bool)
      "cert advice appended"
      true
      (Strutil.contains_substring "Trusted Root" e)
;;

let test_ensure_no_download_off_windows () =
  let get, _ = fresh_cache () in
  let io =
    base_io
      ~latest:(Error "must not be called")
      ~download:(fun ~url:_ -> Error "must not be called")
      ()
  in
  match Bootstrap.ensure_full ~override_path:"" ~resolve_path:get ~os:"Unix" io with
  | Ok _ -> Alcotest.fail "expected error"
  | Error e ->
    Alcotest.(check string) "fast failure" "winget is only available on Windows" e
;;

(* --- download_winget against fixtures --- *)

let copy_to_temp src name =
  let ic = open_in_bin src in
  let n = in_channel_length ic in
  let b = Bytes.create n in
  really_input ic b 0 n;
  close_in ic;
  let dir = Filename.temp_dir "devkit-dl-" "" in
  let dst = Filename.concat dir name in
  let oc = open_out_bin dst in
  output_bytes oc b;
  close_out oc;
  dst
;;

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s
;;

let test_download_happy () =
  let cache = Filename.temp_dir "devkit-cache-" "" in
  let copied = ref "" in
  let io =
    base_io
      ~is_file:Sys.file_exists
      ~cache
      ~latest:(Ok (mkrel "v9.9" [ "bundle.msixbundle" ]))
      ~download:(fun ~url:_ ->
        let dst = copy_to_temp "fixtures/bundle.zip" "dl.msixbundle" in
        copied := dst;
        Ok dst)
      ~unzip:(fun ~zip ~dir -> Zipx.extract ~zip_path:zip ~dest_dir:dir)
      ()
  in
  (match Bootstrap.download_winget io with
   | Error e -> Alcotest.fail ("download: " ^ e)
   | Ok exe ->
     Alcotest.(check string) "cache exe" (Filename.concat cache "AppInstaller.exe") exe;
     Alcotest.(check string) "first msix wins" "FROM-FIRST" (read_file exe));
  (* Temp download cleaned up, cache kept. *)
  Alcotest.(check bool) "temp removed" false (Sys.file_exists !copied)
;;

let test_download_no_bundle () =
  let io = base_io ~latest:(Ok (mkrel "v9.9" [ "a.zip" ])) () in
  match Bootstrap.download_winget io with
  | Ok _ -> Alcotest.fail "expected no-bundle error"
  | Error e ->
    Alcotest.(check bool)
      "message names tag"
      true
      (Strutil.contains_substring "v9.9" e && Strutil.contains_substring ".msixbundle" e)
;;

let test_download_no_x64 () =
  let cache = Filename.temp_dir "devkit-cache-" "" in
  let io =
    base_io
      ~is_file:Sys.file_exists
      ~cache
      ~latest:(Ok (mkrel "v9.9" [ "b.msixbundle" ]))
      ~download:(fun ~url:_ -> Ok (copy_to_temp "fixtures/nox64.zip" "dl.msixbundle"))
      ~unzip:(fun ~zip ~dir -> Zipx.extract ~zip_path:zip ~dest_dir:dir)
      ()
  in
  match Bootstrap.download_winget io with
  | Ok _ -> Alcotest.fail "expected no-x64 error"
  | Error e ->
    Alcotest.(check bool)
      "wrapped"
      true
      (Strutil.contains_substring "extract: no x64 .msix" e)
;;

let test_download_unzip_fails () =
  let cache = Filename.temp_dir "devkit-cache-" "" in
  let io =
    base_io
      ~cache
      ~latest:(Ok (mkrel "v9.9" [ "b.msixbundle" ]))
      ~download:(fun ~url:_ -> Ok (copy_to_temp "fixtures/bundle.zip" "dl.msixbundle"))
      ~unzip:(fun ~zip:_ ~dir:_ -> Error "boom")
      ()
  in
  match Bootstrap.download_winget io with
  | Ok _ -> Alcotest.fail "expected unzip error"
  | Error e -> Alcotest.(check string) "wrapped" "extract: boom" e
;;

let test_download_missing_exe () =
  let cache = Filename.temp_dir "devkit-cache-" "" in
  let io =
    base_io
      ~is_file:Sys.file_exists
      ~cache
      ~latest:(Ok (mkrel "v9.9" [ "b.msixbundle" ]))
      ~download:(fun ~url:_ -> Ok (copy_to_temp "fixtures/bundle.zip" "dl.msixbundle"))
      ~unzip:(fun ~zip:_ ~dir:_ -> Ok ())
      ()
  in
  match Bootstrap.download_winget io with
  | Ok _ -> Alcotest.fail "expected missing-exe error"
  | Error e ->
    Alcotest.(check string)
      "missing-exe message"
      "winget.exe not found after extraction"
      e
;;

let test_cache_memoizes () =
  let get, reset = fresh_cache () in
  let spawns = ref 0 in
  let io =
    base_io
      ~spawn:(fun _ _ ->
        incr spawns;
        "", false)
      ~getenv:(function
        | "PATH" -> Some "/bin"
        | _ -> None)
      ~is_file:(fun p -> p = "/bin/winget")
      ~cwd:(Ok "/proj")
      ~exe:(Ok "/opt/app")
      ()
  in
  ignore (get io);
  ignore (get io);
  Alcotest.(check int) "probed once" 0 !spawns;
  (* PATH lookup is pure (no spawn). The memo test that matters: a second
     call returns the cached value without re-resolving, even after the
     filesystem changes. *)
  let io2 = { io with Bootstrap.is_file = (fun _ -> false) } in
  Alcotest.(check string) "memoized" "/bin/winget" (get io2);
  reset ();
  Alcotest.(check string) "reset clears" "" (get io2)
;;

let () =
  Alcotest.run
    "bootstrap"
    [ ( "find_portable"
      , [ Alcotest.test_case "hit" `Quick test_portable_hit
        ; Alcotest.test_case "prefers winget.exe" `Quick test_portable_prefers_winget_exe
        ; Alcotest.test_case "subfolder" `Quick test_portable_subfolder
        ; Alcotest.test_case "walks up" `Quick test_portable_walks_up
        ; Alcotest.test_case "stops at 4" `Quick test_portable_stops_at_4
        ] )
    ; ( "find_on_path"
      , [ Alcotest.test_case "both separators" `Quick test_on_path
        ; Alcotest.test_case "miss" `Quick test_on_path_miss
        ] )
    ; ( "winget_runs"
      , [ Alcotest.test_case "exit 0" `Quick test_runs_ok
        ; Alcotest.test_case "output despite exit" `Quick test_runs_output_despite_exit
        ; Alcotest.test_case "silent fail" `Quick test_runs_silent_fail
        ; Alcotest.test_case "empty no spawn" `Quick test_runs_empty_no_spawn
        ] )
    ; ( "via_cmd"
      , [ Alcotest.test_case "alias resolved" `Quick test_via_cmd
        ; Alcotest.test_case "where fails" `Quick test_via_cmd_fail
        ] )
    ; "cert", [ Alcotest.test_case "markers + advice" `Quick test_cert ]
    ; ( "msixbundle"
      , [ Alcotest.test_case "first wins" `Quick test_msixbundle_first
        ; Alcotest.test_case "none" `Quick test_msixbundle_none
        ] )
    ; "format_size", [ Alcotest.test_case "boundaries" `Quick test_sizes ]
    ; ( "resolve"
      , [ Alcotest.test_case "cwd wins, no spawn" `Quick test_resolve_cwd_wins_no_spawn
        ; Alcotest.test_case "PATH fallback" `Quick test_resolve_path_fallback
        ; Alcotest.test_case "miss" `Quick test_resolve_miss
        ] )
    ; ( "ensure"
      , [ Alcotest.test_case "override blind" `Quick test_ensure_override_blind
        ; Alcotest.test_case "resolved" `Quick test_ensure_resolved
        ; Alcotest.test_case "cert advice" `Quick test_ensure_download_cert_advice
        ; Alcotest.test_case
            "no download off Windows"
            `Quick
            test_ensure_no_download_off_windows
        ] )
    ; ( "download"
      , [ Alcotest.test_case "happy path" `Quick test_download_happy
        ; Alcotest.test_case "no bundle" `Quick test_download_no_bundle
        ; Alcotest.test_case "no x64 msix" `Quick test_download_no_x64
        ; Alcotest.test_case "unzip fails" `Quick test_download_unzip_fails
        ; Alcotest.test_case "missing exe" `Quick test_download_missing_exe
        ] )
    ; "cache", [ Alcotest.test_case "memoizes + resets" `Quick test_cache_memoizes ]
    ]
;;
