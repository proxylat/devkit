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

let date = Alcotest.(triple int int int)
let opt_date = Alcotest.(option date)

let test_parse_iso_date () =
  let cases =
    [ "2026-10-05T12:34:56Z", Some (2026, 10, 5)
    ; "2026-10-05", Some (2026, 10, 5)
    ; "2026-01-31 ", Some (2026, 1, 31)
    ; "", None
    ; "oops", None
    ; "2026-13-01", None
    ; "2026-00-10", None
    ; "2026-10-32", None
    ; "2026-10-00", None
    ; "2026/10/05", None
    ; "abcd-ef-gh", None
    ; "2026-1-5", None
    ]
  in
  List.iter
    (fun (input, expected) ->
       Alcotest.(check opt_date) input expected (parse_iso_date input))
    cases
;;

let test_days_between () =
  let cases =
    [ (2026, 10, 5), (2026, 10, 5), 0
    ; (2026, 10, 1), (2026, 10, 9), 8
    ; (2026, 9, 30), (2026, 10, 1), 1
    ; (2026, 2, 27), (2026, 3, 1), 2
    ; (2024, 2, 28), (2024, 3, 1), 2
    ; (1970, 1, 1), (1970, 1, 2), 1
    ; (2026, 10, 9), (2026, 10, 1), -8
    ; (2025, 12, 31), (2026, 1, 1), 1
    ]
  in
  List.iter
    (fun (a, b, want) ->
       let y1, m1, d1 = a
       and y2, m2, d2 = b in
       Alcotest.(check int)
         (Printf.sprintf "%04d-%02d-%02d to %04d-%02d-%02d" y1 m1 d1 y2 m2 d2)
         want
         (days_between a b))
    cases
;;

let () =
  Alcotest.run
    "strutil"
    [ "canon_repo", [ Alcotest.test_case "url forms" `Quick test_canon_repo ]
    ; ( "iso_date"
      , [ Alcotest.test_case "parse" `Quick test_parse_iso_date
        ; Alcotest.test_case "days_between" `Quick test_days_between
        ] )
    ]
;;
