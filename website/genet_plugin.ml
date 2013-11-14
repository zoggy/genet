(** *)

open Stog_types;;

module Smap = Stog_types.Str_map;;

let files = ref (Smap.empty : string Smap.t Smap.t) ;;

let get_ids s =
  let len = String.length s in
  let re_start = Str.regexp "^#id=\\([^\n]+\\)\n" in
  let re_end = Str.regexp_string "\n\n" in
  let rec f map p =
    let p1 =
      try Some (Str.search_forward re_start s p)
      with Not_found -> None
    in
    match p1 with
      None -> map
    | Some p1 ->
        let id = Str.matched_group 1 s in
        let p2 = p1 + String.length (Str.matched_string s) in
        try
          let p3 = Str.search_forward re_end s p2 in
          let code = String.sub s p2 (p3 - p2) in
          let map = Smap.add id code map in
          f map p3
        with Not_found ->
          let code = String.sub s p2 (len - p2) in
          Smap.add id code map
  in
  f Smap.empty 0
;;

let read_file stog elt file =
  let file = Filename.concat (Filename.dirname elt.elt_src) file in
  let stog = Stog_plug.add_dep stog elt (Stog_types.File file) in
  try (stog, Smap.find file !files)
  with Not_found ->
    let contents = Stog_misc.string_of_file file in
    let map = get_ids contents in
    files := Smap.add file map !files;
    (stog, map)
;;

let format_code ?prompt code =
  match prompt with
    None -> code
  | Some p ->
      let lines = Stog_misc.split_string code ['\n'] in
      let f line =
        if String.length line > 0 && line.[0] <> ' ' then
          p^" "^line
        else
          line
      in
      String.concat "\n" (List.map f lines)
;;

let fun_from_shell elt_id stog env args subs =
  let elt = Stog_types.elt stog elt_id in
  try
    let file =
      match Xtmpl.get_arg args ("","file") with
        None -> failwith "from_shell: missing file attribute"
      | Some file -> file
    in
    let (stog, map) = read_file stog elt file in
    let id =
      match Xtmpl.get_arg args ("","id") with
      | None -> failwith "from_shell: missing id"
      | Some id -> id
    in
    let code =
      try Smap.find id map
      with Not_found ->
          failwith (Printf.sprintf "from_shell: id %S not found in file %S" id file)
    in
    let prompt = Xtmpl.get_arg args ("","prompt") in
    (stog, [ Xtmpl.E (("","command-line"), [], [ Xtmpl.D (format_code ?prompt code) ]) ])
  with
  Failure msg ->
      Stog_msg.error msg;
      (stog, [])
;;

let module_name = "genet";;

let base_rules stog elt_id =
  [ ("", "from-shell"), fun_from_shell elt_id ];;

let fun_level_base =
  Stog_engine.fun_apply_stog_elt_rules base_rules
;;

let level_funs =
  [
    "base", fun_level_base ;
  ]
;;

let default_levels =
  List.fold_left
    (fun map (name, levels) -> Stog_types.Str_map.add name levels map)
    Stog_types.Str_map.empty
    [
      "base", [ 5 ] ;
    ]

let make_engine ?levels () =
  let levels = Stog_html.mk_levels module_name level_funs default_levels ?levels () in
  let module M =
  struct
    type data = unit
    let modul = {
        Stog_engine.mod_name = module_name ;
        mod_levels = levels ;
        mod_data = ()
       }

    type cache_data = ()

    let cache_load stog data elt xml = data
    let cache_store stog data elt = ()
  end
  in
  (module M : Stog_engine.Module)
;;

let f stog =
  let levels =
    try Some (Stog_types.Str_map.find module_name stog.Stog_types.stog_levels)
    with Not_found -> None
  in
  make_engine ?levels ()
;;

let () = Stog_engine.register_module module_name f;

