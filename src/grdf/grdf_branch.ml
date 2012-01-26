(** *)

open Grdf_types;;
open Rdf_sparql;;

type t = { bch_name : string ; bch_uri : string ; }

let branches wld =
  let query =
    { select_proj = ["name" ; "uri"] ;
      select_distinct = None ;
      select_where = (
       [ (`V "uri",
          [ `I Grdfs.rdf_type, [ `I Grdfs.genet_branch ] ;
            `I Grdfs.genet_name, [ `V "name" ]
          ]
         )
       ], None)
    }
  in
  let f acc qr =
    let name = Rdf_query_results.get_binding_value_by_name qr "name" in
    let uri = Rdf_query_results.get_binding_value_by_name qr "uri" in
    match name, uri with
      None, _ | _, None -> acc
    | Some name, Some uri ->
        match Rdf_node.get_literal_value name, Rdf_node.get_uri uri with
          None, _ | _, None -> acc
        | Some name, Some uri ->
            { bch_name = name ; bch_uri = Rdf_uri.as_string uri} :: acc
  in
  Rdf_sparql.select_and_fold wld.wld_world wld.wld_model query f
;;

let name wld uri =
  let source = Rdf_node.new_from_uri_string wld.wld_world uri in
  let arc = Rdf_node.new_from_uri_string wld.wld_world Grdfs.genet_name in
  let iterator = Rdf_model.get_targets wld.wld_model ~source ~arc in
  if Rdf_iterator.is_at_end iterator then
    ""
  else
    match Rdf_iterator.get_object iterator Rdf_node.copy_node with
      None -> ""
    | Some node -> Misc.string_of_opt (Rdf_node.get_literal_value node)
;;

let parent wld uri =
  let target = Rdf_node.new_from_uri_string wld.wld_world uri in
  let arc = Rdf_node.new_from_uri_string wld.wld_world Grdfs.genet_hasbranch in
  let iterator = Rdf_model.get_sources wld.wld_model ~target ~arc in
  if Rdf_iterator.is_at_end iterator then
    None
  else
    match Rdf_iterator.get_object iterator Rdf_node.copy_node with
      None -> None
    | Some node ->
        match Rdf_node.get_uri node with
          None -> None
        | Some uri ->
            let s_uri = Rdf_uri.as_string uri in
            let tool = Rdf_node.new_from_uri_string wld.wld_world Grdfs.genet_tool in
            Some (s_uri, Grdfs.is_a wld.wld_world wld.wld_model ~sub: node ~obj: tool)
;;

let subs wld uri =
  let source = Rdf_node.new_from_uri_string wld.wld_world uri in
  let arc = Rdf_node.new_from_uri_string wld.wld_world Grdfs.genet_hasbranch in
  let iterator = Rdf_model.get_targets wld.wld_model ~source ~arc in
  let f acc node =
    match Rdf_node.get_uri node with
      None -> acc
    | Some uri -> (Rdf_uri.as_string uri) :: acc
  in
  Rdf_iterator.fold_objects iterator Rdf_node.copy_node f
;;

let branch_exists wld uri =
  let query =
    { select_proj = ["name"] ;
      select_distinct = None ;
      select_where = (
       [ (`I uri,
          [ `I Grdfs.rdf_type, [ `I Grdfs.genet_branch ] ;
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

let do_add wld uri name =
  let sub = Rdf_node.new_from_uri_string wld.wld_world uri in
  let cl = Rdf_node.new_from_uri_string wld.wld_world Grdfs.genet_branch in
  Grdfs.add_type wld.wld_world wld.wld_model ~sub ~obj: cl;
  Grdfs.add_name wld sub name
;;

let add wld parent name =
  let node_parent = Rdf_node.new_from_uri_string wld.wld_world parent in
  let parent_is_tool = Grdfs.is_a_tool wld node_parent in
  let parent_is_branch = Grdfs.is_a_branch wld node_parent in
  if not (parent_is_tool || parent_is_branch) then
    Grdf_types.error Grdf_types.Parent_is_not_tool_or_branch;

  let uri =
    (if parent_is_tool
     then Grdfs.uri_branch_from_parent_tool
     else Grdfs.uri_branch_from_parent_branch)
    parent name
  in
  match branch_exists wld uri with
    Some name -> Grdf_types.error (Grdf_types.Branch_exists name)
  | None -> do_add wld uri name
;;