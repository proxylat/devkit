(** Forge provider interface: GitHub plus GitLab and Forgejo-family
    (Codeberg, self-hosted) release truth.

    GitHub is fixed to github.com; GitLab and Forgejo carry their host
    so self-hosted instances work. Codeberg runs Forgejo, so [codeberg:]
    is a Forgejo row on codeberg.org. Releases reuse {!Gh.release}:
    GitHub and Gitea/Forgejo share the object shape ([tag_name] plus
    [assets[]]); GitLab's list endpoint answers newest-first with
    installer links under [assets.links[]].

    Bare self-hosted URLs stay plain links: without a probe no static
    rule can tell a Forgejo repo page from a GitLab one (or from any
    installer download URL), so self-hosted repos take an explicit
    [gitlab:] / [forgejo:] prefix with the full URL. Only gitlab.com
    and codeberg.org infer from a bare URL. *)

type t =
  | GitHub
  | GitLab of string (** host, e.g. ["gitlab.com"] *)
  | Forgejo of string (** host, e.g. ["codeberg.org"] *)

let to_string = function
  | GitHub -> "github"
  | GitLab _ -> "gitlab"
  | Forgejo _ -> "forgejo"
;;

let host_of = function
  | GitHub -> "github.com"
  | GitLab h -> h
  | Forgejo h -> h
;;

let default_host = function
  | GitHub -> "github.com"
  | GitLab _ -> "gitlab.com"
  | Forgejo _ -> "codeberg.org"
;;

(** Split [https?://host/path] into a lowercased host plus non-empty
    path segments. Anything without a scheme is [None]. *)
let split_url (s : string) : (string * string list) option =
  let s = String.trim s in
  let low = String.lowercase_ascii s in
  let rest =
    if String.length s >= 8 && String.sub low 0 8 = "https://"
    then Some (String.sub s 8 (String.length s - 8))
    else if String.length s >= 7 && String.sub low 0 7 = "http://"
    then Some (String.sub s 7 (String.length s - 7))
    else None
  in
  match rest with
  | None -> None
  | Some rest ->
    (match String.index_opt rest '/' with
     | None -> None
     | Some i ->
       let host = String.sub rest 0 i |> String.trim |> String.lowercase_ascii in
       let host =
         if String.length host > 4 && String.sub host 0 4 = "www."
         then String.sub host 4 (String.length host - 4)
         else host
       in
       let segs =
         String.sub rest (i + 1) (String.length rest - i - 1)
         |> String.split_on_char '/'
         |> List.filter (fun x -> x <> "")
       in
       if host = "" || segs = [] then None else Some (host, segs))
;;

let has_blank s =
  String.contains s ' ' || String.contains s '\t' || String.contains s '\n'
;;

let strip_git seg =
  let low = String.lowercase_ascii seg in
  let n = String.length seg in
  if n > 4 && String.sub low (n - 4) 4 = ".git" then String.sub seg 0 (n - 4) else seg
;;

let check_path segs =
  List.for_all
    (fun x -> x <> "" && (not (has_blank x)) && not (String.contains x '\\'))
    segs
;;

(** Drop an http(s) scheme + host, leaving the repo path; bare paths
    pass through. Lets pins hold full page URLs whatever the row kind. *)
let path_of_url (s : string) : string =
  match split_url s with
  | None -> s
  | Some (_, segs) -> String.concat "/" segs
;;

(** Known-host bare URLs: gitlab.com and codeberg.org infer their
    forge; github.com stays with {!Strutil.canon_repo}, anything else
    is [None] (a plain link until given an explicit prefix). *)
let infer_url (s : string) : (t * string) option =
  match split_url s with
  | None -> None
  | Some ("gitlab.com", segs) ->
    let segs =
      match List.rev segs with
      | last :: rest -> List.rev (strip_git last :: rest)
      | [] -> []
    in
    if List.length segs >= 2 && check_path segs
    then Some (GitLab "gitlab.com", String.concat "/" segs)
    else None
  | Some ("codeberg.org", [ owner; repo ]) ->
    let repo = strip_git repo in
    if check_path [ owner; repo ]
    then Some (Forgejo "codeberg.org", owner ^ "/" ^ repo)
    else None
  | Some _ -> None
;;

(** Explicit [gitlab:] / [codeberg:] / [gitea:] / [forgejo:] prefixes.
    gitlab.com and codeberg.org take bare paths; self-hosted instances
    take the full URL so the host is never guessed. *)
let of_prefix (pre : string) (rest : string) : (t * string, string) result =
  let rest = String.trim rest in
  match pre with
  | "gitlab" ->
    (match split_url rest with
     | Some (host, segs) ->
       let segs =
         match List.rev segs with
         | last :: rev -> List.rev (strip_git last :: rev)
         | [] -> []
       in
       if List.length segs >= 2 && check_path segs
       then Ok (GitLab host, String.concat "/" segs)
       else Error "want https://host/group/project"
     | None ->
       let segs = List.filter (fun x -> x <> "") (String.split_on_char '/' rest) in
       if List.length segs >= 2 && check_path segs
       then Ok (GitLab "gitlab.com", String.concat "/" segs)
       else Error "want group/project")
  | "codeberg" ->
    (match split_url rest with
     | Some ("codeberg.org", [ owner; repo ]) ->
       let repo = strip_git repo in
       if check_path [ owner; repo ]
       then Ok (Forgejo "codeberg.org", owner ^ "/" ^ repo)
       else Error "want owner/repo"
     | Some _ ->
       Error "want owner/repo (self-hosted hosts use forgejo:https://host/owner/repo)"
     | None ->
       (match List.filter (fun x -> x <> "") (String.split_on_char '/' rest) with
        | [ owner; repo ] when check_path [ owner; repo ] ->
          Ok (Forgejo "codeberg.org", owner ^ "/" ^ repo)
        | _ -> Error "want owner/repo"))
  | "gitea" | "forgejo" ->
    (match split_url rest with
     | Some (host, [ owner; repo ]) ->
       let repo = strip_git repo in
       if check_path [ owner; repo ]
       then Ok (Forgejo host, owner ^ "/" ^ repo)
       else Error "want https://host/owner/repo"
     | Some _ -> Error "want https://host/owner/repo"
     | None ->
       Error
         "need a host: forgejo:https://host/owner/repo (codeberg.org uses \
          codeberg:owner/repo)")
  | _ -> Error ("unsupported forge \"" ^ pre ^ "\"")
;;

(** Parse an [upstream] pin into [(provider, repo)]: a bare forge URL
    infers known hosts (gitlab.com, codeberg.org), a [gitlab:] /
    [forgejo:] / [codeberg:] / [gitea:] prefix routes explicitly,
    anything else is a GitHub [owner/repo] (full page URLs included).
    Never fails: unknown shapes fall back to GitHub truth. *)
let of_upstream (s : string) : t * string =
  let s = String.trim s in
  match infer_url s with
  | Some pr -> pr
  | None ->
    (match String.index_opt s ':' with
     | Some i ->
       let pre = String.lowercase_ascii (String.trim (String.sub s 0 i)) in
       let rest = String.sub s (i + 1) (String.length s - i - 1) in
       if pre = "github"
       then GitHub, Strutil.canon_repo rest
       else (
         match of_prefix pre rest with
         | Ok pr -> pr
         | Error _ -> GitHub, Strutil.canon_repo s)
     | None -> GitHub, Strutil.canon_repo s)
;;

let page_url (p : t) (repo : string) : string =
  "https://" ^ host_of p ^ "/" ^ path_of_url repo
;;

(** Where the "no installer asset" fallback opens: the release feed
    for GitHub-family forges, the repo page for GitLab (no stable
    latest-release permalink to aim at). *)
let release_suffix = function
  | GitHub | Forgejo _ -> "/releases/latest"
  | GitLab _ -> ""
;;

let pct_byte b = Printf.sprintf "%%%02X" b

(** Percent-encode a repo path for the GitLab project id: every byte
    outside the unreserved set goes hex (notably [/] → [%2F], so
    subgroups survive). *)
let encode_path (s : string) : string =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
       match c with
       | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' | '~' ->
         Buffer.add_char buf c
       | _ -> Buffer.add_string buf (pct_byte (Char.code c)))
    s;
  Buffer.contents buf
;;

let api_url (p : t) (repo : string) : string =
  let path = path_of_url repo in
  match p with
  | GitHub -> Printf.sprintf "https://api.github.com/repos/%s/releases/latest" path
  | GitLab h ->
    Printf.sprintf
      "https://%s/api/v4/projects/%s/releases?per_page=1"
      h
      (encode_path path)
  | Forgejo h -> Printf.sprintf "https://%s/api/v1/repos/%s/releases/latest" h path
;;

let link_asset (j : Yojson.Basic.t) : Gh.asset option =
  let open Yojson.Basic.Util in
  try
    let name = j |> member "name" |> to_string_option |> Option.value ~default:"" in
    let url = j |> member "url" |> to_string_option |> Option.value ~default:"" in
    if url = "" then None else Some { Gh.name; browser_download_url = url; size = 0L }
  with
  | _ -> None
;;

(** GitLab answers a list, newest-first: the head is the release and
    its installer links live under [assets.links]. *)
let gitlab_of_yojson (j : Yojson.Basic.t) : Gh.release =
  let open Yojson.Basic.Util in
  let tag = j |> member "tag_name" |> to_string_option |> Option.value ~default:"" in
  let published_at =
    j |> member "released_at" |> to_string_option |> Option.value ~default:""
  in
  let links =
    try j |> member "assets" |> member "links" |> to_list with
    | _ -> []
  in
  { Gh.tag_name = tag; published_at; assets = List.filter_map link_asset links }
;;

let parse (p : t) (body : string) : (Gh.release, string) result =
  try
    match p with
    | GitHub | Forgejo _ -> Gh.parse_release body
    | GitLab _ ->
      (match Yojson.Basic.from_string body with
       | `List (first :: _) -> Ok (gitlab_of_yojson first)
       | `List [] -> Error "decode: no releases"
       | _ -> Error "decode: expected a release list")
  with
  | Yojson.Json_error msg -> Error ("decode: " ^ msg)
  | Yojson.Basic.Util.Type_error (msg, _) -> Error ("decode: " ^ msg)
;;

(** Shared request headers: forge Accept plus a devkit user agent,
    [GH_TOKEN] bearer auth to GitHub, [GITLAB_TOKEN] as
    [PRIVATE-TOKEN] to GitLab; Forgejo-family needs no token for
    public repos. *)
let headers_for (p : t) : (string * string) list =
  (match p with
   | GitHub -> [ "Accept", "application/vnd.github+json" ]
   | GitLab _ | Forgejo _ -> [ "Accept", "application/json" ])
  @ [ "User-Agent", "devkit/1.0" ]
  @
  match p with
  | GitHub ->
    (match Sys.getenv_opt "GH_TOKEN" with
     | None | Some "" -> []
     | Some tok -> [ "Authorization", "Bearer " ^ tok ])
  | GitLab _ ->
    (match Sys.getenv_opt "GITLAB_TOKEN" with
     | None | Some "" -> []
     | Some tok -> [ "PRIVATE-TOKEN", tok ])
  | Forgejo _ -> []
;;

(** Tag lookup URL: the per-tag release endpoint. GitLab encodes the
    tag ([%2F]) since tags may hold slashes; GitHub/Forgejo take it
    raw, so slash-tags there are a known limitation. *)
let tag_url (p : t) (repo : string) (tag : string) : string =
  let path = path_of_url repo in
  match p with
  | GitHub -> Printf.sprintf "https://api.github.com/repos/%s/releases/tags/%s" path tag
  | GitLab h ->
    Printf.sprintf
      "https://%s/api/v4/projects/%s/releases/%s"
      h
      (encode_path path)
      (encode_path tag)
  | Forgejo h -> Printf.sprintf "https://%s/api/v1/repos/%s/releases/tags/%s" h path tag
;;

(** Parse one single-release object: GitHub/Forgejo share
    {!Gh.parse_release}; GitLab's object feeds {!gitlab_of_yojson}. *)
let parse_single (p : t) (body : string) : (Gh.release, string) result =
  try
    match p with
    | GitHub | Forgejo _ -> Gh.parse_release body
    | GitLab _ -> Ok (gitlab_of_yojson (Yojson.Basic.from_string body))
  with
  | Yojson.Json_error msg -> Error ("decode: " ^ msg)
  | Yojson.Basic.Util.Type_error (msg, _) -> Error ("decode: " ^ msg)
;;

(** Latest release for [repo] (a bare path or a full page URL) on [p].
    Sends [GH_TOKEN] bearer auth to GitHub and [GITLAB_TOKEN] as
    [PRIVATE-TOKEN] to GitLab; Forgejo-family needs no token for
    public repos. An empty tag is an [Error], so callers never compare
    against nothing. *)
let latest (fetch : Fetch.fetch) (p : t) (repo : string) : (Gh.release, string) result =
  match fetch ~timeout_s:15 (api_url p repo) (headers_for p) with
  | Error e -> Error e
  | Ok body ->
    (match parse p body with
     | Ok rel when rel.Gh.tag_name <> "" -> Ok rel
     | Ok _ -> Error "decode: empty tag_name"
     | Error _ as err -> err)
;;

(** [tag_exists fetch p repo tag] is whether release [tag] still
    exists upstream (yank detection). A fetch error holding ["404"]
    (curl [-f] surfaces HTTP 404 in the error text) is the yank
    signal and answers [Ok false]; any other fetch error is unknown
    (offline must never read as yanked) and passes through as
    [Error]. A parseable body whose tag matches answers [Ok true], a
    mismatched tag [Ok false], an empty tag or unparseable body an
    [Error]. *)
let tag_exists (fetch : Fetch.fetch) (p : t) (repo : string) (tag : string)
  : (bool, string) result
  =
  match fetch ~timeout_s:15 (tag_url p repo tag) (headers_for p) with
  | Error e -> if Strutil.contains_substring "404" e then Ok false else Error e
  | Ok body ->
    (match parse_single p body with
     | Ok rel when rel.Gh.tag_name = "" -> Error "decode: empty tag_name"
     | Ok rel -> Ok (rel.Gh.tag_name = tag)
     | Error _ as err -> err)
;;
