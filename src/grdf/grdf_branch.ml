(** *)

open Grdf_types;;
open Rdf_sparql;;

let dbg = Misc.create_log_fun
  ~prefix: "Grdf_branch"
    "GENET_GRDF_BRANCH_DEBUG_LEVEL"
;;

type t = { bch_name : string ; bch_uri : string ; }

let branches wld =
  dbg ~level: 1 (fun () -> "Grdf_branch.branches");
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
  List.rev (Rdf_sparql.select_and_fold wld.wld_world wld.wld_model query f [])
;;

let name wld uri =
  dbg ~level: 1 (fun () -> "Grdf_branch.name uri="^uri);
  let source = Rdf_node.new_from_uri_string wld.wld_world uri in
  Grdfs.name wld source
;;

let parent wld uri =
  dbg ~level: 1 (fun () -> "Grdf_branch.parent uri="^uri);
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
            Some (s_uri, Grdfs.is_a_tool wld node)
;;

let subs wld uri =
  dbg ~level: 1 (fun () -> "Grdf_branch.subs uri="^uri);
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
  dbg ~level: 1 (fun () -> "Grdf_branch.branch_exists uri="^uri);
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
  match List.rev (Rdf_sparql.select_and_fold wld.wld_world wld.wld_model query f []) with
    name :: _ -> Some name
  | [] -> None
;;

let do_add wld uri name =
  dbg ~level: 1 (fun () -> "Grdf_branch.do_add uri="^uri^" name="^name);
  let sub = Rdf_node.new_from_uri_string wld.wld_world uri in
  let cl = Rdf_node.new_from_uri_string wld.wld_world Grdfs.genet_branch in
  Grdfs.add_type wld.wld_world wld.wld_model ~sub ~obj: cl;
  Grdfs.add_name wld sub name
;;

let add wld ~parent name =
  dbg ~level: 1 (fun () -> "Grdf_branch.add parent="^parent^" name="^name);
  let node_parent = Rdf_node.new_from_uri_string wld.wld_world parent in
  let parent_is_tool = Grdfs.is_a_tool wld node_parent in
  let parent_is_branch = Grdfs.is_a_branch wld node_parent in
  if not (parent_is_tool || parent_is_branch) then
    Grdf_types.error (Grdf_types.Not_tool_or_branch parent);

  dbg ~level:2 (fun () -> "parent is ok");
  let uri =
    (if parent_is_tool
     then Grdfs.uri_branch_from_parent_tool
     else Grdfs.uri_branch_from_parent_branch)
    parent name
  in
  match branch_exists wld uri with
    Some name -> Grdf_types.error (Grdf_types.Branch_exists name)
  | None ->
      do_add wld uri name;
      Grdfs.add_stmt wld.wld_world wld.wld_model
      ~sub: node_parent
      ~pred: (Rdf_node.new_from_uri_string wld.wld_world Grdfs.genet_hasbranch)
      ~obj:  (Rdf_node.new_from_uri_string wld.wld_world uri);
      uri
;;

let rec tool wld uri =
  dbg ~level: 1 (fun () -> "Grdf_branch.tool uri="^uri);
  if Grdfs.is_a_tool wld (Rdf_node.new_from_uri_string wld.wld_world uri)
  then uri
  else
    match parent wld uri with
      None -> uri
    | Some (parent, is_tool) -> if is_tool then parent else tool wld parent
;;

