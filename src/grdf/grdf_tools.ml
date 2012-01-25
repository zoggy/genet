(** *)

open Grdf_types;;

type t = { tool_name : string ; tool_uri : string ; }

let tools wld =
  let query = Printf.sprintf
    "SELECT ?name ?uri WHERE { ?uri <%s> <%s> . ?uri <%s> ?name }"
    Grdfs.rdf_type Grdfs.genet_tool Grdfs.genet_name
  in
  let q = Rdf_query.new_query ~name: "sparql" wld.wld_world ~query in
  try
    let qr = Rdf_model.query_execute wld.wld_model q in
    let rec iter acc =
      if Rdf_query_results.finished qr then
        acc
      else
        (
         let name = Rdf_query_results.get_binding_value_by_name qr "name" in
         let uri = Rdf_query_results.get_binding_value_by_name qr "uri" in
         match name, uri with
           None, _ | _, None -> iter acc
         | Some name, Some uri ->
             match Rdf_node.get_literal_value name, Rdf_node.get_uri uri with
               None, _ | _, None -> iter acc
             | Some name, Some uri ->
                 ignore(Rdf_query_results.next qr);
                 iter ({ tool_name = name ; tool_uri = Rdf_uri.as_string uri} :: acc)
        )
    in
    iter []
  with
    Rdf_query_results.Query_results_creation_failed _ ->
      failwith ("Query failed: "^query)
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

exception Tool_exists of string;;

let add_tool wld pref name =
  let uri = Grdfs.uri_tool ~pref ~tool: name in
  match get_tool wld uri with
    Some t -> raise (Tool_exists t.tool_name)
  | None ->
      let sub = Rdf_node.new_from_uri_string wld.wld_world uri in
      let cl = Rdf_node.new_from_uri_string wld.wld_world Grdfs.genet_tool in
      Grdfs.add_type wld.wld_world wld.wld_model ~sub ~obj: cl;
      let pred = Rdf_node.new_from_uri_string wld.wld_world Grdfs.genet_name in
      (* FIXME convert to UTF 8*)
      let obj = Rdf_node.new_from_literal wld.wld_world name in
      Grdfs.add_stmt wld.wld_world wld.wld_model ~sub ~pred ~obj;
      get_tool wld uri
;;