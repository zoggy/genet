(** *)

open Grdf_types;;
open Rdf_sparql;;

let dbg = Misc.create_log_fun
  ~prefix: "Grdf_ftype"
    "GENET_GRDF_FTYPES_DEBUG_LEVEL"
;;

let filetypes wld =
  dbg ~level: 1 (fun () -> "Grdf_ftype.filetypes");
  let query =
    { select_proj = ["uri"] ;
      select_distinct = None ;
      select_where = (
       [ (`V "uri",
          [ `I Grdfs.rdf_type, [ `I Grdfs.genet_filetype ] ;
          ]
         )
       ], None)
    }
  in
  let f acc qr =
    let uri = Rdf_query_results.get_binding_value_by_name qr "uri" in
    match uri with
      None -> acc
    | Some uri ->
        match Rdf_node.get_uri uri with
          None -> acc
        | Some uri -> (Rdf_uri.as_string uri :: acc)
  in
  List.rev (Rdf_sparql.select_and_fold wld.wld_world wld.wld_model query f [])
;;

let name wld uri =
  dbg ~level: 1 (fun () -> "Grdf_ftype.name uri="^uri);
  let source = Rdf_node.new_from_uri_string wld.wld_world uri in
  Grdfs.name wld source
;;

let desc wld uri =
  dbg ~level: 1 (fun () -> "Grdf_ftype.desc uri="^uri);
  let source = Rdf_node.new_from_uri_string wld.wld_world uri in
  Grdfs.desc wld source
;;

let extension wld uri =
  dbg ~level: 1 (fun () -> "Grdf_ftype.extension uri="^uri);
  let source = Rdf_node.new_from_uri_string wld.wld_world uri in
  Misc.string_of_opt (Grdfs.target_literal wld source Grdfs.genet_file_ext)
;;

let filetype_exists wld uri =
  dbg ~level: 1 (fun () -> "Grdf_ftype.filetype_exists uri="^uri);
  let query =
    { select_proj = ["name"] ;
      select_distinct = None ;
      select_where = (
       [ (`I uri,
          [ `I Grdfs.rdf_type, [ `I Grdfs.genet_filetype ] ;
            `I Grdfs.genet_name, [ `V "name" ]
          ]
         )
       ], None)
    }
  in
  let f acc qr =
    let name = Rdf_query_results.get_binding_value_by_name qr "name" in
    match name with
      None -> acc
    | Some name ->
        match Rdf_node.get_literal_value name with
          None -> acc
        | Some name -> name :: acc
  in
  match List.rev (Rdf_sparql.select_and_fold wld.wld_world wld.wld_model query f []) with
    name :: _ -> Some name
  | [] -> None
;;

let do_add wld uri ~name ~desc ~extension =
  dbg ~level: 1 (fun () -> "Grdf_ftype.do_add uri="^uri^" name="^name);
  let sub = Rdf_node.new_from_uri_string wld.wld_world uri in
  let cl = Rdf_node.new_from_uri_string wld.wld_world Grdfs.genet_filetype in
  Grdfs.add_type wld.wld_world wld.wld_model ~sub ~obj: cl;
  Grdfs.add_name wld sub name;
  Grdfs.add_desc wld sub desc;
  let pred = Rdf_node.new_from_uri_string wld.wld_world Grdfs.genet_file_ext in
  let obj = Rdf_node.new_from_literal wld.wld_world extension in
  Grdfs.add_stmt wld.wld_world wld.wld_model ~sub ~pred ~obj
;;

let add wld ~name ~desc ~extension =
  dbg ~level: 1 (fun () -> "Grdf_ftype.add name="^name);
  let uri = Grdfs.uri_filetype name in
  match filetype_exists wld uri with
    Some name-> Grdf_types.error (Grdf_types.Filetype_exists name)
  | None -> do_add wld uri ~name ~desc ~extension; uri
;;

let string_of_filetype wld uri =
  let name = name wld uri in
  let ext = extension wld uri in
  Printf.sprintf "%s (.%s)" name ext