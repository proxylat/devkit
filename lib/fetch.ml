(** HTTP fetching.

    OCaml has no stock HTTPS client, and pulling cohttp into [lib]
    would drag async into the stdlib-only core, so production fetching
    shells out to [curl]: inbox on Windows 10 1803+ and present on most
    Linux systems.
    The call shape stays injectable: tests and future backends substitute
    [fetch]. *)

(** [fetch url headers] performs GET [url] with [headers] ([name, value]
    pairs), returning the response body. API calls get 15s ([?timeout_s]);
    bulk downloads are unbounded. *)
type fetch = ?timeout_s:int -> string -> (string * string) list -> (string, string) result

(** curl backend: [-fsSL], caller-supplied timeout, headers via [-H]. *)
let curl_fetch (run : Proc.runner) : fetch =
  fun ?(timeout_s = 0) url headers ->
  let args =
    [ "-fsSL"; url ]
    @ (if timeout_s > 0 then [ "--max-time"; string_of_int timeout_s ] else [])
    @ List.concat_map (fun (k, v) -> [ "-H"; k ^ ": " ^ v ]) headers
  in
  match run "curl" args with
  | Some body -> Ok body
  | None -> Error ("curl failed: " ^ url)
;;
