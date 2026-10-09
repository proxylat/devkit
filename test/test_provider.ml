(** Tests for {!Devkit.Provider}: forge inference, prefixes, API URLs,
    release parsing, and upstream pin parsing. *)
open Devkit

let infer_url () =
  let open Provider in
  Alcotest.(check (option (pair string string)))
    "gitlab bare url"
    (Some ("gitlab", "group/sub/proj"))
    (Option.map
       (fun (p, r) -> to_string p, r)
       (infer_url "https://gitlab.com/group/sub/proj"));
  Alcotest.(check (option (pair string string)))
    "gitlab .git stripped"
    (Some ("gitlab", "group/proj"))
    (Option.map
       (fun (p, r) -> to_string p, r)
       (infer_url "https://gitlab.com/group/proj.git"));
  Alcotest.(check (option (pair string string)))
    "codeberg bare url"
    (Some ("forgejo", "owner/repo"))
    (Option.map
       (fun (p, r) -> to_string p, r)
       (infer_url "https://codeberg.org/owner/repo"));
  Alcotest.(check (option (pair string string)))
    "codeberg subgroups rejected"
    None
    (Option.map (fun (p, r) -> to_string p, r) (infer_url "https://codeberg.org/a/b/c"));
  Alcotest.(check (option (pair string string)))
    "github stays out"
    None
    (Option.map (fun (p, r) -> to_string p, r) (infer_url "https://github.com/o/r"));
  Alcotest.(check (option (pair string string)))
    "self-hosted stays out"
    None
    (Option.map
       (fun (p, r) -> to_string p, r)
       (infer_url "https://git.example.com/owner/repo"));
  Alcotest.(check (option (pair string string)))
    "bare path stays out"
    None
    (Option.map (fun (p, r) -> to_string p, r) (infer_url "group/proj"))
;;

let of_prefix () =
  let open Provider in
  let show = function
    | Ok (p, r) -> "ok:" ^ to_string p ^ ":" ^ host_of p ^ ":" ^ r
    | Error _ -> "err"
  in
  Alcotest.(check string)
    "gitlab bare path"
    "ok:gitlab:gitlab.com:group/sub/x"
    (show (of_prefix "gitlab" "group/sub/x"));
  Alcotest.(check string)
    "gitlab self-hosted url"
    "ok:gitlab:git.example.com:group/x"
    (show (of_prefix "gitlab" "https://git.example.com/group/x"));
  Alcotest.(check string)
    "gitlab shallow rejected"
    "err"
    (show (of_prefix "gitlab" "onlyone"));
  Alcotest.(check string)
    "codeberg bare"
    "ok:forgejo:codeberg.org:o/r"
    (show (of_prefix "codeberg" "o/r"));
  Alcotest.(check string) "forgejo needs host" "err" (show (of_prefix "forgejo" "o/r"));
  Alcotest.(check string)
    "forgejo self-hosted"
    "ok:forgejo:git.example.com:o/r"
    (show (of_prefix "forgejo" "https://git.example.com/o/r"));
  Alcotest.(check string)
    "gitea alias"
    "ok:forgejo:git.example.com:o/r"
    (show (of_prefix "gitea" "https://git.example.com/o/r"));
  Alcotest.(check string) "bogus" "err" (show (of_prefix "bitbucket" "o/r"))
;;

let api_url () =
  Alcotest.(check string)
    "github"
    "https://api.github.com/repos/o/r/releases/latest"
    (Provider.api_url Provider.GitHub "o/r");
  Alcotest.(check string)
    "gitlab encodes slashes"
    "https://gitlab.com/api/v4/projects/group%2Fsub%2Fproj/releases?per_page=1"
    (Provider.api_url (Provider.GitLab "gitlab.com") "group/sub/proj");
  Alcotest.(check string)
    "forgejo"
    "https://git.example.com/api/v1/repos/o/r/releases/latest"
    (Provider.api_url (Provider.Forgejo "git.example.com") "o/r")
;;

let parse () =
  (match
     Provider.parse
       (Provider.GitLab "gitlab.com")
       {|[{"tag_name": "v2.0", "released_at": "2026-01-02T03:04:05Z", "assets": {"links": [{"name": "setup.exe", "url": "https://x/setup.exe"}]}}]|}
   with
   | Ok rel ->
     Alcotest.(check string) "tag" "v2.0" rel.Gh.tag_name;
     Alcotest.(check string) "published_at" "2026-01-02T03:04:05Z" rel.Gh.published_at;
     Alcotest.(check int) "one link" 1 (List.length rel.Gh.assets);
     Alcotest.(check string)
       "link url"
       "https://x/setup.exe"
       (List.hd rel.Gh.assets).Gh.browser_download_url
   | Error e -> Alcotest.fail ("gitlab parse: " ^ e));
  (match Provider.parse (Provider.GitLab "gitlab.com") "[]" with
   | Ok _ -> Alcotest.fail "empty list must fail"
   | Error _ -> ());
  match
    Provider.parse
      (Provider.Forgejo "codeberg.org")
      {|{"tag_name": "v3.0", "assets": [{"name": "a.exe", "browser_download_url": "https://y/a.exe"}]}|}
  with
  | Ok rel ->
    Alcotest.(check string) "tag" "v3.0" rel.Gh.tag_name;
    Alcotest.(check int) "one asset" 1 (List.length rel.Gh.assets)
  | Error e -> Alcotest.fail ("forgejo parse: " ^ e)
;;

let page_url () =
  Alcotest.(check string)
    "github"
    "https://github.com/o/r"
    (Provider.page_url Provider.GitHub "o/r");
  Alcotest.(check string)
    "gitlab subgroup"
    "https://gitlab.com/group/sub/x"
    (Provider.page_url (Provider.GitLab "gitlab.com") "group/sub/x");
  Alcotest.(check string)
    "forgejo full url value"
    "https://git.example.com/o/r"
    (Provider.page_url (Provider.Forgejo "git.example.com") "https://git.example.com/o/r")
;;

let of_upstream () =
  let show (p, r) = Provider.to_string p ^ ":" ^ r in
  Alcotest.(check string) "bare repo" "github:o/r" (show (Provider.of_upstream "o/r"));
  Alcotest.(check string)
    "github url"
    "github:o/r"
    (show (Provider.of_upstream "https://github.com/o/r/releases"));
  Alcotest.(check string)
    "gitlab url"
    "gitlab:group/sub/x"
    (show (Provider.of_upstream "https://gitlab.com/group/sub/x"));
  Alcotest.(check string)
    "gitlab prefix"
    "gitlab:group/x"
    (show (Provider.of_upstream "gitlab:group/x"));
  Alcotest.(check string)
    "forgejo prefix url"
    "forgejo:o/r"
    (show (Provider.of_upstream "forgejo:https://git.example.com/o/r"));
  Alcotest.(check string)
    "bare word falls back"
    "github:notaurl"
    (show (Provider.of_upstream "notaurl"))
;;

let tag_exists_hit () =
  let urls = ref [] in
  let fetch ?timeout_s url _ =
    Alcotest.(check (option int)) "15s timeout" (Some 15) timeout_s;
    urls := url :: !urls;
    if Strutil.contains_substring "gitlab.com" url
    then Ok {|{"tag_name": "releases/v1", "assets": {"links": []}}|}
    else Ok {|{"tag_name": "v1.0", "assets": []}|}
  in
  (match Provider.tag_exists fetch Provider.GitHub "o/r" "v1.0" with
   | Ok true -> ()
   | Ok false -> Alcotest.fail "github tag should exist"
   | Error e -> Alcotest.fail ("github tag_exists: " ^ e));
  (match Provider.tag_exists fetch (Provider.Forgejo "git.example.com") "o/r" "v1.0" with
   | Ok true -> ()
   | Ok false -> Alcotest.fail "forgejo tag should exist"
   | Error e -> Alcotest.fail ("forgejo tag_exists: " ^ e));
  (match
     Provider.tag_exists
       fetch
       (Provider.GitLab "gitlab.com")
       "group/sub/proj"
       "releases/v1"
   with
   | Ok true -> ()
   | Ok false -> Alcotest.fail "gitlab tag should exist"
   | Error e -> Alcotest.fail ("gitlab tag_exists: " ^ e));
  Alcotest.(check bool)
    "github tag url"
    true
    (List.mem "https://api.github.com/repos/o/r/releases/tags/v1.0" !urls);
  Alcotest.(check bool)
    "forgejo tag url"
    true
    (List.mem "https://git.example.com/api/v1/repos/o/r/releases/tags/v1.0" !urls);
  Alcotest.(check bool)
    "gitlab tag url encodes slashes"
    true
    (List.mem
       "https://gitlab.com/api/v4/projects/group%2Fsub%2Fproj/releases/releases%2Fv1"
       !urls)
;;

let tag_exists_errors () =
  let not_found ?timeout_s:_ _ _ =
    Error "curl: (22) The requested URL returned error: 404"
  in
  (match Provider.tag_exists not_found Provider.GitHub "o/r" "v1.0" with
   | Ok false -> ()
   | Ok true -> Alcotest.fail "404 must read as yanked"
   | Error e -> Alcotest.fail ("404 must not error: " ^ e));
  let offline ?timeout_s:_ _ _ =
    Error "curl: (6) Could not resolve host: api.github.com"
  in
  (match Provider.tag_exists offline Provider.GitHub "o/r" "v1.0" with
   | Error e ->
     Alcotest.(check bool)
       "passthrough"
       true
       (Strutil.contains_substring "Could not resolve" e)
   | Ok _ -> Alcotest.fail "non-404 must not read as yanked");
  let mismatch ?timeout_s:_ _ _ = Ok {|{"tag_name": "v9.9", "assets": []}|} in
  (match Provider.tag_exists mismatch Provider.GitHub "o/r" "v1.0" with
   | Ok false -> ()
   | Ok true -> Alcotest.fail "mismatched tag must be false"
   | Error e -> Alcotest.fail ("mismatch must not error: " ^ e));
  let bad ?timeout_s:_ _ _ = Ok "{oops" in
  match Provider.tag_exists bad Provider.GitHub "o/r" "v1.0" with
  | Error e ->
    Alcotest.(check bool) "decode prefix" true (Strutil.contains_substring "decode" e)
  | Ok _ -> Alcotest.fail "bad body must error"
;;

let () =
  Alcotest.run
    "provider"
    [ "infer", [ Alcotest.test_case "known hosts" `Quick infer_url ]
    ; "prefix", [ Alcotest.test_case "explicit kinds" `Quick of_prefix ]
    ; "urls", [ Alcotest.test_case "api shape" `Quick api_url ]
    ; "parse", [ Alcotest.test_case "release bodies" `Quick parse ]
    ; "pages", [ Alcotest.test_case "repo pages" `Quick page_url ]
    ; "pins", [ Alcotest.test_case "of_upstream" `Quick of_upstream ]
    ; ( "tags"
      , [ Alcotest.test_case "tag hit + urls" `Quick tag_exists_hit
        ; Alcotest.test_case "404 vs errors" `Quick tag_exists_errors
        ] )
    ]
;;
