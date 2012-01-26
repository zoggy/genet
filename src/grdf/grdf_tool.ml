(** *)

open Grdf_types;;
open Rdf_sparql;;

type t = { tool_name : string ; tool_uri : string ; }

let tools wld =
  let query =
    { select_proj = ["name" ; "uri"] ;
      select_distinct = None ;
      select_where = (
       [ (`V "uri",
          [ `I Grdfs.rdf_type, [ `I Grdfs.genet_tool ] ;
            `I Grdfs.genet_name, [ `V "name" ]
          ]
         )
       ], None)
    }
  in

(*
  let query = Printf.sprintf
    "SELECT ?name ?uri WHERE { ?uri <%s> <%s> . ?uri <%s> ?name }"
    Grdfs.rdf_type Grdfs.genet_tool Grdfs.genet_name
  in
*)
  let f acc qr =
    let name = Rdf_query_results.get_binding_value_by_name qr "name" in
    let uri = Rdf_query_results.get_binding_value_by_name qr "uri" in
    match name, uri with
      None, _ | _, None -> acc
    | Some name, Some uri ->
        match Rdf_node.get_literal_value name, Rdf_node.get_uri uri with
          None, _ | _, None -> acc
        | Some name, Some uri ->
            { tool_name = name ; tool_uri = Rdf_uri.as_string uri} :: acc
  in
  Rdf_sparql.select_and_fold wld.wld_world wld.wld_model query f
;;

let get_tool wld uri =
  let source = Rdf_node.new_from_uri_string wld.wld_world uri in
  let arc = Rdf_node.new_from_uri_string wld.wld_world Grdfs.genet_name in
  let iterator = Rdf_model.get_targets wld.wld_model ~source ~arc in
  let name =
    if Rdf_iterator.is_at_end iterator then
      None
    else
      match Rdf_iterator.get_object iterator Rdf_node.copy_node with
        None -> None
      | Some node -> Rdf_node.get_literal_value node
  in
  match name with
    None -> None
  | Some name -> Some { tool_name = name ; tool_uri = uri }
;;

let add_tool wld pref name =
  let uri = Grdfs.uri_tool ~pref ~tool: name in
  match get_tool wld uri with
    Some t -> Grdf_types.error (Grdf_types.Tool_exists t.tool_name)
  | None ->
      let sub = Rdf_node.new_from_uri_string wld.wld_world uri in
      let cl = Rdf_node.new_from_uri_string wld.wld_world Grdfs.genet_tool in
      Grdfs.add_type wld.wld_world wld.wld_model ~sub ~obj: cl;
      Grdfs.add_name wld sub name;
      get_tool wld uri
;;

let branches wld uri = Grdf_branch.subs wld uri;;
