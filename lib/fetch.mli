(** HTTP fetching via an injectable backend. *)

(** [fetch url headers] performs GET [url] with [headers] ([name, value]
    pairs), returning the response body. *)
type fetch = ?timeout_s:int -> string -> (string * string) list -> (string, string) result

(** curl backend: [-fsSL], caller-supplied timeout, headers via [-H]. *)
val curl_fetch : Proc.runner -> fetch
