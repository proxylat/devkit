(** Tests for {!Strutil}: repo canonicalization. *)

open Devkit.Strutil

let test_canon_repo () =
  let cases =
    [ "owner/repo", "owner/repo"
    ; "  owner/repo  ", "owner/repo"
    ; "owner/repo/", "owner/repo"
    ; "https://github.com/owner/repo", "owner/repo"
    ; "https://github.com/owner/repo/", "owner/repo"
    ; "http://github.com/owner/repo", "owner/repo"
    ; "https://www.github.com/owner/repo", "owner/repo"
    ; "https://github.com/owner/repo/releases/tag/v1.2", "owner/repo"
    ; "https://github.com/owner/repo.git", "owner/repo"
    ; "git@github.com:owner/repo.git", "owner/repo"
    ; "HTTPS://GITHUB.COM/Owner/Repo", "Owner/Repo"
    ; "https://gitlab.com/owner/repo", "https://gitlab.com/owner/repo"
    ; "https://github.com/owner", "https://github.com/owner"
    ; "justone", "justone"
    ; "", ""
    ]
  in
  List.iter
    (fun (input, expected) -> Alcotest.(check string) input expected (canon_repo input))
    cases
;;

let () =
  Alcotest.run
    "strutil"
    [ "canon_repo", [ Alcotest.test_case "url forms" `Quick test_canon_repo ] ]
;;
