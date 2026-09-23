(** Minimal ZIP reader: stored + deflated entries, central-directory driven.

    Naive extraction joins entry names onto the destination, a ZipSlip
    where entries like [../../evil.exe] or [/abs/path] escape it. This
    module sanitizes every entry name with {!safe_path} before any write,
    and refuses to proceed on the first unsafe entry.

    Only what the bootstrap needs is implemented: local headers are located
    through the central directory (so data-descriptor entries work), methods
    are stored (0) and deflated (8, via [decompress]), encryption and exotic
    methods are rejected. CRCs are not rechecked: inflate integrity plus
    sizes suffice for a downloader that hash-verifies the archive itself. *)

type entry =
  { name : string
  ; meth : int
  ; comp_size : int
  ; uncomp_size : int
  ; local_off : int
  }

let u16 (b : bytes) off =
  Char.code (Bytes.get b off) lor (Char.code (Bytes.get b (off + 1)) lsl 8)
;;

let u32 (b : bytes) off = u16 b off lor (u16 b (off + 2) lsl 16)
let sig_local = 0x04034B50
let sig_central = 0x02014B50
let sig_eocd = 0x06054B50

(** Locate the end-of-central-directory record by scanning backwards. *)
let find_eocd (b : bytes) : (int, string) result =
  let n = Bytes.length b in
  if n < 22
  then Error "too small for EOCD"
  else (
    let from = max 0 (n - 22 - 65536) in
    let hit = ref None in
    let i = ref (n - 22) in
    while !hit = None && !i >= from do
      if u32 b !i = sig_eocd then hit := Some !i;
      decr i
    done;
    match !hit with
    | None -> Error "EOCD signature not found"
    | Some off -> Ok off)
;;

(** Parse the central directory into entries. *)
let list_entries (b : bytes) : (entry list, string) result =
  match find_eocd b with
  | Error _ as e -> e
  | Ok eocd ->
    let count = u16 b (eocd + 10) in
    let cd_off = u32 b (eocd + 16) in
    let acc = ref [] in
    let off = ref cd_off in
    let ok = ref true in
    let err = ref "" in
    let n = Bytes.length b in
    for _ = 1 to count do
      if !ok
      then (
        let o = !off in
        if o + 46 > n || u32 b o <> sig_central
        then (
          ok := false;
          err := "bad central directory entry")
        else (
          let flags = u16 b (o + 8) in
          let meth = u16 b (o + 10) in
          let comp = u32 b (o + 20) in
          let uncomp = u32 b (o + 24) in
          let nl = u16 b (o + 28) in
          let xl = u16 b (o + 30) in
          let cl = u16 b (o + 32) in
          let lh = u32 b (o + 42) in
          if o + 46 + nl + xl + cl > n
          then (
            ok := false;
            err := "central directory entry overruns archive")
          else (
            let name = Bytes.sub_string b (o + 46) nl in
            if flags land 0x1 <> 0
            then (
              ok := false;
              err := "encrypted entry: " ^ name)
            else (
              acc
              := { name; meth; comp_size = comp; uncomp_size = uncomp; local_off = lh }
                 :: !acc;
              off := o + 46 + nl + xl + cl))))
    done;
    if !ok then Ok (List.rev !acc) else Error !err
;;

(** Raw DEFLATE (no zlib wrapper) via [decompress]. *)
let inflate_raw (data : bytes) : (bytes, string) result =
  let w = De.make_window ~bits:15 in
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let pos = ref 0 in
  let len = Bytes.length data in
  let refill buf =
    let n = min De.io_buffer_size (len - !pos) in
    for k = 0 to n - 1 do
      buf.{k} <- Bytes.get data (!pos + k)
    done;
    pos := !pos + n;
    n
  in
  let out = Buffer.create 4096 in
  let flush buf n =
    for k = 0 to n - 1 do
      Buffer.add_char out buf.{k}
    done
  in
  match De.Higher.uncompress ~w ~refill ~flush i o with
  | Ok () -> Ok (Buffer.to_bytes out)
  | Error (`Msg msg) -> Error ("inflate: " ^ msg)
;;

(** Entry payload from its local header. *)
let extract_entry (b : bytes) (e : entry) : (bytes, string) result =
  let n = Bytes.length b in
  let o = e.local_off in
  if o + 30 > n || u32 b o <> sig_local
  then Error "bad local header"
  else (
    let nl = u16 b (o + 26) in
    let xl = u16 b (o + 28) in
    let start = o + 30 + nl + xl in
    if start + e.comp_size > n
    then Error "entry overruns archive"
    else (
      let raw = Bytes.sub b start e.comp_size in
      match e.meth with
      | 0 -> Ok raw
      | 8 -> inflate_raw raw
      | m -> Error (Printf.sprintf "unsupported method %d: %s" m e.name)))
;;

(** Split a zip path on both separators (archives use [/]; hostile ones
    mix in [\\], which Windows treats as a separator too). *)
let split_seps (s : string) : string list =
  let s = String.map (fun c -> if c = '\\' then '/' else c) s in
  String.split_on_char '/' s
;;

(** [safe_path ~dest name] resolves [name] inside [dest], returning [None]
    for anything that escapes: absolute paths, drive letters ([C:…]), UNC
    ([//…]), or [..] climbing past the root. Trailing separators mark
    directories, reported via [is_dir]. *)
let safe_path ~dest (name : string) : (string * bool) option =
  if name = ""
  then None
  else (
    let is_dir =
      let n = String.length name in
      name.[n - 1] = '/' || name.[n - 1] = '\\'
    in
    let parts = split_seps name in
    let absolute =
      match parts with
      | "" :: _ -> true (* leading / or \\ *)
      | first :: _ when String.length first >= 2 && first.[1] = ':' -> true
      | _ -> false
    in
    if absolute
    then None
    else (
      let stack = ref [] in
      let escaped = ref false in
      List.iter
        (fun p ->
           if p = "" || p = "."
           then ()
           else if p = ".."
           then (
             match !stack with
             | [] -> escaped := true
             | _ :: rest -> stack := rest)
           else stack := p :: !stack)
        parts;
      if !escaped
      then None
      else (
        let rel = List.rev !stack |> String.concat Filename.dir_sep in
        if rel = ""
        then None (* name was only dots/slashes *)
        else Some (dest ^ Filename.dir_sep ^ rel, is_dir))))
;;

let rec mkdir_p (dir : string) : unit =
  if dir = "" || dir = "." || dir = Filename.dir_sep || Sys.file_exists dir
  then ()
  else (
    mkdir_p (Filename.dirname dir);
    try Unix.mkdir dir 0o755 with
    | Unix.Unix_error (Unix.EEXIST, _, _) -> ())
;;

(** Extract [zip_path] into [dest_dir]. Fails fast on the first unsafe
    entry. Entries before the hostile one are already written; Go wrote
    the whole archive, this stops at the first hostile name and reports it. *)
let extract ~(zip_path : string) ~(dest_dir : string) : (unit, string) result =
  (try
     let ic = open_in_bin zip_path in
     let n = in_channel_length ic in
     let b = Bytes.create n in
     really_input ic b 0 n;
     close_in ic;
     Ok b
   with
   | Sys_error msg -> Error msg)
  |> function
  | Error _ as e -> e
  | Ok b ->
    (match list_entries b with
     | Error _ as e -> e
     | Ok entries ->
       let rec go = function
         | [] -> Ok ()
         | e :: rest ->
           (match safe_path ~dest:dest_dir e.name with
            | None -> Error ("unsafe zip entry (ZipSlip refused): " ^ e.name)
            | Some (path, is_dir) ->
              if is_dir
              then (
                mkdir_p path;
                go rest)
              else
                (match extract_entry b e with
                 | Error _ as err -> err
                 | Ok payload ->
                   (try
                      mkdir_p (Filename.dirname path);
                      let oc = open_out_bin path in
                      output_bytes oc payload;
                      close_out oc;
                      Ok ()
                    with
                    | Sys_error msg -> Error msg))
                |> (function
                 | Error _ as err -> err
                 | Ok () -> go rest))
       in
       mkdir_p dest_dir;
       go entries)
;;
