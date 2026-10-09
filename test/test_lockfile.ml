(** Tests for {!Lockfile}: tag-immutability lockfile parsing and rendering. *)

open Devkit
open Lockfile

let contains = Strutil.contains_substring

let mk ?(thumbprint = "") ?(host = "") id tag sha256 =
  { id; tag; sha256; thumbprint; host }
;;

let pp_entry fmt e =
  Format.fprintf
    fmt
    "{id=%S;tag=%S;sha256=%S;thumbprint=%S;host=%S}"
    e.id
    e.tag
    e.sha256
    e.thumbprint
    e.host
;;

let entry_t = Alcotest.testable pp_entry ( = )

let round_trip () =
  let entries =
    [ mk ~thumbprint:"THUMB" ~host:"github.com" "Git.Git" "v2.48.1" "abc123"
    ; mk "npm:pyright" "v1.2.3" "def456"
    ]
  in
  let text = to_string entries in
  Alcotest.(check bool) "entry tables" true (contains "[[entry]]" text);
  Alcotest.(check bool) "keeps thumbprint" true (contains "THUMB" text);
  Alcotest.(check bool) "keeps host" true (contains "github.com" text);
  Alcotest.(check (list entry_t)) "round trip" entries (parse text)
;;

let optionals_omitted () =
  let entries = [ mk "Git.Git" "v2.48.1" "abc" ] in
  let text = to_string entries in
  Alcotest.(check bool) "no thumbprint line" false (contains "thumbprint" text);
  Alcotest.(check bool) "no host line" false (contains "host =" text);
  match parse text with
  | [ e ] ->
    Alcotest.(check string) "thumbprint empty" "" e.thumbprint;
    Alcotest.(check string) "host empty" "" e.host
  | _ -> Alcotest.fail "expected one entry"
;;

let upsert_replace () =
  let a = mk "a" "t1" "s1" in
  let b = mk "b" "t1" "s1" in
  let c = mk "c" "t1" "s1" in
  let b2 = { b with tag = "t2"; sha256 = "s2" } in
  Alcotest.(check (list entry_t)) "replaced in place" [ a; b2; c ] (upsert [ a; b; c ] b2)
;;

let upsert_append () =
  let a = mk "a" "t1" "s1" in
  let b = mk "b" "t1" "s1" in
  Alcotest.(check (list entry_t)) "appended" [ a; b ] (upsert [ a ] b)
;;

let find_hit_miss () =
  let entries = [ mk "a" "t1" "s1"; mk "b" "t2" "s2" ] in
  Alcotest.(check (option entry_t)) "hit" (Some (mk "a" "t1" "s1")) (find entries "a");
  Alcotest.(check (option entry_t)) "miss" None (find entries "zzz")
;;

let malformed () =
  Alcotest.(check (list entry_t)) "malformed is []" [] (parse "[[entry\nid = ");
  Alcotest.(check (list entry_t)) "empty doc is []" [] (parse "");
  Alcotest.(check (option entry_t)) "find on []" None (find [] "a");
  Alcotest.(check (list entry_t))
    "upsert on []"
    [ mk "a" "t1" "s1" ]
    (upsert [] (mk "a" "t1" "s1"))
;;

let missing_keys () =
  match parse "[[entry]]\nid = \"Git.Git\"\n" with
  | [ e ] ->
    Alcotest.(check string) "id" "Git.Git" e.id;
    Alcotest.(check string) "tag" "" e.tag;
    Alcotest.(check string) "sha256" "" e.sha256;
    Alcotest.(check string) "thumbprint" "" e.thumbprint;
    Alcotest.(check string) "host" "" e.host
  | _ -> Alcotest.fail "expected one entry"
;;

let () =
  Alcotest.run
    "lockfile"
    [ ( "lockfile"
      , [ Alcotest.test_case "round_trip" `Quick round_trip
        ; Alcotest.test_case "optionals_omitted" `Quick optionals_omitted
        ; Alcotest.test_case "upsert_replace" `Quick upsert_replace
        ; Alcotest.test_case "upsert_append" `Quick upsert_append
        ; Alcotest.test_case "find" `Quick find_hit_miss
        ; Alcotest.test_case "malformed" `Quick malformed
        ; Alcotest.test_case "missing_keys" `Quick missing_keys
        ] )
    ]
;;
