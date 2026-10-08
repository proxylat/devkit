(** GitHub releases API.

    HTTP goes through an injectable {!Fetch.fetch}; JSON through yojson.
    Sends [GH_TOKEN] bearer auth and a [devkit/1.0] user agent. *)

type asset =
  { name : string
  ; browser_download_url : string
  ; size : int64
  }

type release =
  { tag_name : string
  ; assets : asset list
  }

let asset_of_yojson (j : Yojson.Basic.t) : asset =
  let open Yojson.Basic.Util in
  { name = j |> member "name" |> to_string_option |> Option.value ~default:""
  ; browser_download_url =
      j |> member "browser_download_url" |> to_string_option |> Option.value ~default:""
  ; size =
      j
      |> member "size"
      |> to_int_option
      |> Option.map Int64.of_int
      |> Option.value ~default:0L
  }
;;

let release_of_yojson (j : Yojson.Basic.t) : release =
  let open Yojson.Basic.Util in
  { tag_name = j |> member "tag_name" |> to_string_option |> Option.value ~default:""
  ; assets = j |> member "assets" |> to_list |> List.map asset_of_yojson
  }
;;

let parse_release (body : string) : (release, string) result =
  try Ok (release_of_yojson (Yojson.Basic.from_string body)) with
  | Yojson.Json_error msg -> Error ("decode: " ^ msg)
;;

(** [canon_repo] accepts a bare [owner/repo] or a full GitHub URL — repo
    page, deep link, or clone URL — and returns the canonical [owner/repo].
    Anything unrecognized is returned unchanged. *)
let canon_repo (s : string) : string =
  let s = String.trim s in
  let drop n str = String.sub str n (String.length str - n) in
  let chop p str =
    let low = String.lowercase_ascii str in
    let n = String.length p in
    if String.length str >= n && String.sub low 0 n = p then Some (drop n str) else None
  in
  let strip_git r =
    let low = String.lowercase_ascii r in
    let n = String.length r in
    if n > 4 && String.sub low (n - 4) 4 = ".git" then String.sub r 0 (n - 4) else r
  in
  let segs p = List.filter (fun x -> x <> "") (String.split_on_char '/' p) in
  let path =
    match chop "git@github.com:" s with
    | Some rest -> Some (`Url rest)
    | None ->
      let noscheme =
        match chop "https://" s with
        | Some _ as hit -> hit
        | None -> chop "http://" s
      in
      (match noscheme with
       | None -> Some (`Bare s)
       | Some rest ->
         (match chop "github.com/" rest with
          | Some _ as hit -> hit
          | None -> chop "www.github.com/" rest)
         |> Option.map (fun p -> `Url p))
  in
  match path with
  | None -> s
  | Some (`Bare b) ->
    (match segs b with
     | [ owner; repo ] -> owner ^ "/" ^ repo
     | _ -> s)
  | Some (`Url u) ->
    (match segs u with
     | owner :: repo :: _ -> owner ^ "/" ^ strip_git repo
     | _ -> s)
;;

(** Latest release for [owner/repo] (or a full GitHub URL, canonicalized). *)
let latest_release (fetch : Fetch.fetch) (repo : string) : (release, string) result =
  let repo = canon_repo repo in
  let url = Printf.sprintf "https://api.github.com/repos/%s/releases/latest" repo in
  let headers =
    [ "Accept", "application/vnd.github+json"; "User-Agent", "devkit/1.0" ]
    @
    match Sys.getenv_opt "GH_TOKEN" with
    | None | Some "" -> []
    | Some tok -> [ "Authorization", "Bearer " ^ tok ]
  in
  match fetch ~timeout_s:15 url headers with
  | Error e -> Error e
  | Ok body -> parse_release body
;;

(** Pick the best Windows installer asset: prefer arch-tagged
    (amd64/x64/x86_64) Windows builds, fall back to any .exe/.msi. *)
let match_by_arch (assets : asset list) : asset option =
  let lower s = String.lowercase_ascii s in
  let ext name =
    match String.rindex_opt name '.' with
    | None -> ""
    | Some i -> String.sub name i (String.length name - i) |> lower
  in
  let is_win (a : asset) =
    let name = lower a.name in
    let e = ext a.name in
    Strutil.contains_substring "windows" name
    || Strutil.contains_substring "win" name
    || e = ".exe"
    || e = ".msi"
  in
  let archs = [ "amd64"; "x64"; "x86_64" ] in
  let has_arch (a : asset) =
    let name = lower a.name in
    List.exists (fun arch -> Strutil.contains_substring arch name) archs
  in
  match List.find_opt (fun a -> is_win a && has_arch a) assets with
  | Some _ as hit -> hit
  | None ->
    List.find_opt
      (fun (a : asset) ->
         let e = ext a.name in
         e = ".exe" || e = ".msi")
      assets
;;
