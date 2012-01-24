(** *)

type t = { tool_name : string ; tool_uri : string ; }

let get_tool world model uri =
  let source = Rdf_node.new_from_uri_string world uri in
  let arc = Rdf_node.new_from_uri_string world Grdfs.genet_name in
  let iterator = Rdf_model.get_targets model ~source ~arc in
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

let add_tool world model pref name =
  let uri = Grdfs.uri_tool ~pref ~tool: name in
  match get_tool world model uri with
    Some t -> raise (Tool_exists t.tool_name)
  | None ->
      let sub = Rdf_node.new_from_uri_string world uri in
      let cl = Rdf_node.new_from_uri_string world Grdfs.genet_tool in
      Grdfs.add_type world model ~sub ~obj: cl;
      let pred = Rdf_node.new_from_uri_string world Grdfs.genet_name in
      (* FIXME convert to UTF 8*)
      let obj = Rdf_node.new_from_literal world name in
      Grdfs.add_stmt world model ~sub ~pred ~obj;
      get_tool world model uri
;;