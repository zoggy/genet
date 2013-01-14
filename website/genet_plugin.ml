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

let read_file elt file =
  let file = Filename.concat (Filename.dirname elt.elt_src) file in
  Stog_plug.add_dep elt (Stog_plug.File file);
  try Smap.find file !files
  with Not_found ->
    let contents = Stog_misc.string_of_file file in
    let map = get_ids contents in
    files := Smap.add file map !files;
    map
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

let fun_from_shell _ _ elt env args subs =
  try
    let file =
      match Xtmpl.get_arg args ("","file") with
        None -> failwith "from_shell: missing file attribute"
      | Some file -> file
    in
    let map = read_file elt file in
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
    [ Xtmpl.E (("","command-line"), [], [ Xtmpl.D (format_code ?prompt code) ]) ]
  with
  Failure msg ->
      Stog_msg.error msg;
      []
;;


let rules stog elt_id elt = [ ("", "from-shell"), fun_from_shell stog elt_id elt];;

let () = Stog_plug.register_level_fun 5 (Stog_html.compute_elt rules);;
