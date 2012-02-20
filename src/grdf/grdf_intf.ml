open Grdf_types;;
open Rdf_sparql;;

let dbg = Misc.create_log_fun
  ~prefix: "Grdf_intf"
    "GENET_GRDF_INTF_DEBUG_LEVEL"
;;

let intfs wld =
  dbg ~level: 1 (fun () -> "Grdf_intf.intfs");
  let query =
    { select_proj = ["uri"] ;
      select_distinct = None ;
      select_where = (
       [ (`V "uri",
          [ `I Grdfs.rdf_type, [ `I Grdfs.genet_intf ] ;
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
  dbg ~level: 1 (fun () -> "Grdf_intf.name uri="^uri);
  let source = Rdf_node.new_from_uri_string wld.wld_world uri in
  Grdfs.name wld source
;;

let intf_exists wld uri =
  dbg ~level: 1 (fun () -> "Grdf_intf.intf_exists uri="^uri);
  let query =
    { select_proj = ["name"] ;
      select_distinct = None ;
      select_where = (
       [ (`I uri,
          [ `I Grdfs.rdf_type, [ `I Grdfs.genet_intf ] ;
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
  dbg ~level: 1 (fun () -> "Grdf_intf.do_add uri="^uri^" name="^name);
  let sub = Rdf_node.new_from_uri_string wld.wld_world uri in
  let cl = Rdf_node.new_from_uri_string wld.wld_world Grdfs.genet_intf in
  Grdfs.add_type wld.wld_world wld.wld_model ~sub ~obj: cl;
  Grdfs.add_name wld sub name
;;

let add wld ~parent name =
  dbg ~level: 1 (fun () -> "Grdf_intf.add parent="^parent^" name="^name);
  let node_parent = Rdf_node.new_from_uri_string wld.wld_world parent in
  let parent_is_tool = Grdfs.is_a_tool wld node_parent in
  let parent_is_branch = Grdfs.is_a_branch wld node_parent in
  if not (parent_is_tool || parent_is_branch) then
    Grdf_types.error (Grdf_types.Not_tool_or_branch parent);

  let tool = Grdf_branch.tool wld parent in
  let node_tool = Rdf_node.new_from_uri_string wld.wld_world tool in
  if not (Grdfs.is_a_tool wld node_tool) then
    Grdf_types.error (Grdf_types.Not_a_tool tool);
  let uri = Grdfs.uri_intf ~tool ~intf: name in
  begin
    match intf_exists wld uri with
      Some _ -> ()
    | None ->
        do_add wld uri name
  end;
  Grdfs.add_stmt wld.wld_world wld.wld_model
  ~sub: node_parent
  ~pred: (Rdf_node.new_from_uri_string wld.wld_world Grdfs.genet_hasintf)
  ~obj:  (Rdf_node.new_from_uri_string wld.wld_world uri);
  uri
;;

let intfs_of wld ?(recur=false) uri =
  dbg ~level: 1 (fun () -> "Grdf_intf.intfs uri="^uri);
  let get_of uri =
    let source = Rdf_node.new_from_uri_string wld.wld_world uri in
    Grdfs.target_uris wld source Grdfs.genet_hasintf
  in
  if recur then
    begin
      let add set uri = Sset.add uri set in
      let rec iter set uri =
        let uris = get_of uri in
        let set = List.fold_left add set uris in
        match Grdf_branch.parent wld uri with
          None -> set
        | Some (uri, _) -> iter set uri
      in
      let set = iter Sset.empty uri in
      Sset.elements set
    end
  else
    get_of uri
;;



