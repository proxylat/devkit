(** Forge provider interface: GitHub plus GitLab and Forgejo-family releases. *)

type t =
  | GitHub
  | GitLab of string (** host, e.g. ["gitlab.com"] *)
  | Forgejo of string (** host, e.g. ["codeberg.org"] *)

(** ["github"] / ["gitlab"] / ["forgejo"]: the install-dispatch kind. *)
val to_string : t -> string

(** ["github.com"] / the GitLab or Forgejo host. *)
val host_of : t -> string

(** The host a bare path pins to: github.com, gitlab.com, codeberg.org. *)
val default_host : t -> string

(** Known-host bare URLs: gitlab.com and codeberg.org infer their
    forge; anything else (including github.com, owned by
    {!Strutil.canon_repo}) is [None]. *)
val infer_url : string -> (t * string) option

(** Explicit [gitlab:] / [codeberg:] / [gitea:] / [forgejo:] prefixes:
    fixed instances take bare paths, self-hosted instances the full
    URL. Anything else (including a bare self-hosted path with no
    host) is an [Error] naming the fix. *)
val of_prefix : string -> string -> (t * string, string) result

(** Parse an [upstream] pin into [(provider, repo)]: known-host bare
    URLs infer, [gitlab:] / [forgejo:] / [codeberg:] / [gitea:]
    prefixes route explicitly, anything else is GitHub truth. Never
    fails: unknown shapes fall back to GitHub. *)
val of_upstream : string -> t * string

(** [https://host/path]: the repo page for [repo] (a bare path or a
    full page URL). *)
val page_url : t -> string -> string

(** Fallback page suffix when a release has no installer asset:
    ["/releases/latest"] on GitHub-family forges, [""] (the repo
    page) on GitLab. *)
val release_suffix : t -> string

(** The releases API URL for [repo] (bare path or page URL). *)
val api_url : t -> string -> string

(** Parse one releases-API body into a {!Gh.release}. *)
val parse : t -> string -> (Gh.release, string) result

(** Latest release for [repo] on [p]. Empty tags are an [Error]. *)
val latest : Fetch.fetch -> t -> string -> (Gh.release, string) result

(** [tag_exists fetch p repo tag] is whether release [tag] still
    exists upstream (yank detection). A fetch error holding ["404"]
    answers [Ok false]; any other fetch error passes through as
    [Error]; a parseable body whose tag matches answers [Ok true], a
    mismatched tag [Ok false], an empty tag or unparseable body an
    [Error]. GitHub/Forgejo tags go raw (slash-tags are a known
    limitation); the GitLab tag is percent-encoded. *)
val tag_exists : Fetch.fetch -> t -> string -> string -> (bool, string) result
