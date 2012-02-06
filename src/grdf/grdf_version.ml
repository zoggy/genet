(** *)

open Grdf_types;;
open Rdf_sparql;;

let dbg = Misc.create_log_fun
  ~prefix: "Grdf_version"
    "GENET_GRDF_VERSION_DEBUG_LEVEL"
;;

let versions wld =
  dbg ~level: 1 (fun () -> "Grdf_version.versions");
  let query =
    { select_proj = ["uri"] ;
      select_distinct = None ;
      select_where = (
       [ (`V "uri",
          [ `I Grdfs.rdf_type, [ `I Grdfs.genet_version ] ;
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
  Rdf_sparql.select_and_fold wld.wld_world wld.wld_model query f
;;

let name wld uri =
  dbg ~level: 1 (fun () -> "Grdf_version.name uri="^uri);
  let source = Rdf_node.new_from_uri_string wld.wld_world uri in
  Grdfs.name wld source
;;

let parent wld uri =
  dbg ~level: 1 (fun () -> "Grdf_version.parent uri="^uri);
  let target = Rdf_node.new_from_uri_string wld.wld_world uri in
  Grdfs.source_uri wld Grdfs.genet_hasversion target
;;

let version_exists wld uri =
  dbg ~level: 1 (fun () -> "Grdf_version.version_exists uri="^uri);
  let query =
    { select_proj = ["name"] ;
      select_distinct = None ;
      select_where = (
       [ (`I uri,
          [ `I Grdfs.rdf_type, [ `I Grdfs.genet_version ] ;
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
  dbg ~level: 1 (fun () -> "Grdf_version.do_add uri="^uri^" name="^name);
  let sub = Rdf_node.new_from_uri_string wld.wld_world uri in
  let cl = Rdf_node.new_from_uri_string wld.wld_world Grdfs.genet_version in
  Grdfs.add_type wld.wld_world wld.wld_model ~sub ~obj: cl;
  Grdfs.add_name wld sub name
;;

let add wld ~tool ?(parent=tool) name =
  dbg ~level: 1 (fun () -> "Grdf_version.add parent="^parent^" name="^name);
  let node_tool = Rdf_node.new_from_uri_string wld.wld_world tool in
  let node_parent = Rdf_node.new_from_uri_string wld.wld_world parent in

  let tool_is_tool = Grdfs.is_a_tool wld node_tool in
  let parent_is_tool = Grdfs.is_a_tool wld node_parent in
  let parent_is_branch = Grdfs.is_a_branch wld node_parent in

  if not tool_is_tool then
    Grdf_types.error (Grdf_types.Not_a_tool tool);

  if not (parent_is_tool || parent_is_branch) then
    Grdf_types.error (Grdf_types.Not_tool_or_branch parent);

  let root_tool = Grdf_branch.tool wld parent in
  if root_tool <> tool then
    Grdf_types.error (Grdf_types.Tool_of_branch_differs (parent, root_tool, tool));

  let uri = Grdfs.uri_version tool name in

  match version_exists wld uri with
    Some name -> Grdf_types.error (Grdf_types.Version_exists name)
  | None ->
      do_add wld uri name;
      Grdfs.add_stmt wld.wld_world wld.wld_model
      ~sub: node_parent
      ~pred: (Rdf_node.new_from_uri_string wld.wld_world Grdfs.genet_hasversion)
      ~obj:  (Rdf_node.new_from_uri_string wld.wld_world uri);
      uri
;;

let versions_of wld ?(recur=false) uri =
  dbg ~level: 1 (fun () -> "Grdf_branch.versions_of uri="^uri);
  if recur then
    begin
      let add set uri = Sset.add uri set in
      let rec iter set uri =
        let source = Rdf_node.new_from_uri_string wld.wld_world uri in
        let versions = Grdfs.target_uris wld source Grdfs.genet_hasversion in
        let set = List.fold_left add set versions in
        let subs = Grdf_branch.subs wld uri in
        List.fold_left iter set subs
      in
      let set = iter Sset.empty uri in
      Sset.elements set
    end
  else
    (
     let source = Rdf_node.new_from_uri_string wld.wld_world uri in
     Grdfs.target_uris wld source Grdfs.genet_hasversion
    )
;;
