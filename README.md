# devkit

Scan your PC for installed software across package managers, merge the
result with a `devkit.toml` manifest, and export a restore-ready snapshot.

## Build

System OCaml 5.5.0 + opam `system` switch. Deps:
`alcotest cmdliner yojson digestif decompress sexplib re ocamlformat`.

```sh
eval $(opam env --switch=system)
dune build
dune runtest          # 129 alcotests, must stay green
dune exec bin/main.exe -- --help
```

`dune-project` is the source of truth for packaging; `devkit.opam` is
generated (see AGENTS.md for the regen dance). Every `lib/*.ml` has a
`.mli`; format with `ocamlformat -i` before testing.

## Commands

```sh
devkit                    # dashboard: manifest status, or full scan
devkit import FILE        # preview a manifest against this machine
devkit add ID...          # append installed apps to devkit.toml
devkit append [--new]     # append newly-detected apps (or given ids)
devkit export [-o FILE]   # write devkit.toml + winget import-ready JSON
```

`export` writes two files: the TOML manifest (`devkit.toml`) and a
`winget import -i`-compatible JSON sibling (schema 2.0.0, installed
versions pinned; skipped when no winget apps are installed).

## Custom package managers (`tools.sexp`)

Built-ins: winget, npm, pipx, uv, cargo. Anything else goes in an
s-expression file — `tools.sexp` next to `devkit.toml` first, then
`~/.config/devkit/tools.sexp` (`%LOCALAPPDATA%\devkit\tools.sexp` on
Windows). First file wins per tool name; built-in names are reserved.

```scheme
;; ; comments allowed. Only name/prog/row_re are required.
(tools
 (tool
  (name mytool)
  (prog mytool)
  (list --list --short)              ; argv for the list command
  (skip_prefixes ("- " "WARN"))      ; ignore shim/warning lines
  (row_re "^(\\S+)\\s+v?([0-9][^ ]*)") ; group 1 = name, group 2 = version
  (strip_version_prefix v)           ; strip AFTER matching, so row_re
                                     ; must tolerate the raw prefix
  (install (mytool install {id}))    ; {id} is the only variable
  (upgrade (mytool upgrade {id}))))
```

A broken config fails loudly at startup (unknown field, bad regex,
missing key) — never silently. `row_re` is PCRE; a missing group 2
means unversioned. Custom tools scan after the built-ins in file order,
participate in dashboard/export, and install through their templates
(missing template or failed spawn = error outcome, never a skip).

## Docs

- `AGENTS.md` — toolchain, phased plan, conventions.
- `CHANGES.md` — changelog, newest first.
