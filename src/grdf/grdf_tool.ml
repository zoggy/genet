(** *)

open Grdf_types;;
open Rdf_sparql;;


let dbg = Misc.create_log_fun
  ~prefix: "Grdf_tool"
    "GENET_GRDF_TOOL_DEBUG_LEVEL"
;;

let tools wld =
  let query =
    { select_proj = ["uri"] ;
      select_distinct = None ;
      select_where = (
       [ (`V "uri",
          [ `I Grdfs.rdf_type, [ `I Grdfs.genet_tool ] ]
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
        | Some uri -> (Rdf_uri.as_string uri) :: acc
  in
  Rdf_sparql.select_and_fold wld.wld_world wld.wld_model query f
;;

let name wld uri =
  dbg ~level: 1 (fun () -> "Grdf_tool.name uri="^uri);
  let source = Rdf_node.new_from_uri_string wld.wld_world uri in
  Grdfs.name wld source
;;

let tool_exists wld uri =
  dbg ~level: 1 (fun () -> "Grdf_tool.tool_exists uri="^uri);
  let query =
    { select_proj = ["name"] ;
      select_distinct = None ;
      select_where = (
       [ (`I uri,
          [ `I Grdfs.rdf_type, [ `I Grdfs.genet_tool ] ;
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
  match Rdf_sparql.select_and_fold wld.wld_world wld.wld_model query f with
    name :: _ -> Some name
  | [] -> None
;;

let add_tool wld pref name =
  let uri = Grdfs.uri_tool ~pref ~tool: name in
  match tool_exists wld uri with
    Some name2 -> Grdf_types.error (Grdf_types.Tool_exists name2)
  | None ->
      let sub = Rdf_node.new_from_uri_string wld.wld_world uri in
      let cl = Rdf_node.new_from_uri_string wld.wld_world Grdfs.genet_tool in
      Grdfs.add_type wld.wld_world wld.wld_model ~sub ~obj: cl;
      Grdfs.add_name wld sub name;
      uri
;;

let branches wld uri = Grdf_branch.subs wld uri;;
