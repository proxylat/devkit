(** Tests for {!Scan_cache}: snapshot round-trip and failure modes. *)

open Devkit

let apps =
  [ Dashboard.{ name = "Git.Git"; version = "2.47.1"; pm = "winget" }
  ; Dashboard.{ name = "typescript"; version = "5.3.3"; pm = "npm" }
  ]
;;

let info =
  Winget_parse.IdMap.add
    "git.git"
    Winget_parse.
      { id = "Git.Git"; name = "Git"; version = "2.47.1"; available = "2.48.0" }
    Winget_parse.IdMap.empty
;;

let with_temp f =
  let path = Filename.temp_file "scan_cache" ".json" in
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | _ -> ())
    (fun () -> f path)
;;

let write_file path text =
  let oc = open_out path in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () -> output_string oc text)
;;

let check_app (a : Dashboard.app) (b : Dashboard.app) =
  Alcotest.(check string) "name" a.name b.name;
  Alcotest.(check string) "version" a.version b.version;
  Alcotest.(check string) "pm" a.pm b.pm
;;

let round_trip () =
  with_temp (fun path ->
    Scan_cache.save path apps (Some info);
    match Scan_cache.load path with
    | None -> Alcotest.fail "load returned None"
    | Some (snap : Scan_cache.snapshot) ->
      Alcotest.(check int) "two apps" 2 (List.length snap.apps);
      List.iter2 check_app apps snap.apps;
      Alcotest.(check int) "one info row" 1 (List.length snap.info);
      let id, name, version, available = List.nth snap.info 0 in
      Alcotest.(check string) "info id" "Git.Git" id;
      Alcotest.(check string) "info name" "Git" name;
      Alcotest.(check string) "info version" "2.47.1" version;
      Alcotest.(check string) "info available" "2.48.0" available;
      Alcotest.(check bool) "saved_at set" true (snap.saved_at > 0.0);
      let back = Scan_cache.to_info_map snap.info in
      (match Winget_parse.IdMap.find_opt "git.git" back with
       | None -> Alcotest.fail "info map lost git.git"
       | Some i ->
         Alcotest.(check string) "original case kept" "Git.Git" i.Winget_parse.id;
         Alcotest.(check string) "available kept" "2.48.0" i.available))
;;

let empty_none_round_trip () =
  with_temp (fun path ->
    Scan_cache.save path [] None;
    match Scan_cache.load path with
    | None -> Alcotest.fail "load returned None"
    | Some (snap : Scan_cache.snapshot) ->
      Alcotest.(check int) "no apps" 0 (List.length snap.apps);
      Alcotest.(check int) "info serializes as []" 0 (List.length snap.info))
;;

let corrupt_file () =
  with_temp (fun path ->
    write_file path "not json{{{";
    Alcotest.(check bool) "corrupt is None" true (Scan_cache.load path = None))
;;

let missing_file () =
  let path = Filename.temp_file "scan_cache" ".json" in
  Sys.remove path;
  Alcotest.(check bool) "missing is None" true (Scan_cache.load path = None)
;;

let version_mismatch () =
  with_temp (fun path ->
    write_file path {|{"version":999,"saved_at":0.0,"apps":[],"info":[]}|};
    Alcotest.(check bool) "version 999 is None" true (Scan_cache.load path = None);
    write_file path {|{"saved_at":0.0,"apps":[],"info":[]}|};
    Alcotest.(check bool) "missing version is None" true (Scan_cache.load path = None))
;;

let save_never_raises () =
  (* A directory path cannot be opened for writing: save must swallow
     that (and anything else) instead of raising. *)
  Scan_cache.save (Filename.get_temp_dir_name ()) apps (Some info);
  Scan_cache.save
    (Filename.concat (Filename.get_temp_dir_name ()) "no-such-dir/x.json")
    apps
    None;
  Alcotest.(check bool) "survived" true true
;;

let to_info_map_lowercase () =
  let m = Scan_cache.to_info_map [ "Git.Git", "Git", "2.47.1", "" ] in
  match Winget_parse.IdMap.find_opt "git.git" m with
  | None -> Alcotest.fail "key should be lowercase id"
  | Some (i : Winget_parse.info) ->
    Alcotest.(check string) "original case kept" "Git.Git" i.id
;;

let () =
  Alcotest.run
    "scan_cache"
    [ ( "cache"
      , [ Alcotest.test_case "round trip" `Quick round_trip
        ; Alcotest.test_case "empty none round trip" `Quick empty_none_round_trip
        ; Alcotest.test_case "corrupt file" `Quick corrupt_file
        ; Alcotest.test_case "missing file" `Quick missing_file
        ; Alcotest.test_case "version mismatch" `Quick version_mismatch
        ; Alcotest.test_case "save never raises" `Quick save_never_raises
        ; Alcotest.test_case "to_info_map lowercase" `Quick to_info_map_lowercase
        ] )
    ]
;;
