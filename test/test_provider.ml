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
       {|[{"tag_name": "v2.0", "assets": {"links": [{"name": "setup.exe", "url": "https://x/setup.exe"}]}}]|}
   with
   | Ok rel ->
     Alcotest.(check string) "tag" "v2.0" rel.Gh.tag_name;
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

let () =
  Alcotest.run
    "provider"
    [ "infer", [ Alcotest.test_case "known hosts" `Quick infer_url ]
    ; "prefix", [ Alcotest.test_case "explicit kinds" `Quick of_prefix ]
    ; "urls", [ Alcotest.test_case "api shape" `Quick api_url ]
    ; "parse", [ Alcotest.test_case "release bodies" `Quick parse ]
    ; "pages", [ Alcotest.test_case "repo pages" `Quick page_url ]
    ; "pins", [ Alcotest.test_case "of_upstream" `Quick of_upstream ]
    ]
;;
