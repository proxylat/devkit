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

(** Latest release for [owner/repo]. *)
let latest_release (fetch : Fetch.fetch) (repo : string) : (release, string) result =
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
