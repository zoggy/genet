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
        | Some uri -> Sset.add (Rdf_uri.as_string uri) acc
  in
  Rdf_sparql.select_and_fold wld.wld_world wld.wld_model query f Sset.empty
;;

let name wld uri =
  dbg ~level: 1 (fun () -> "Grdf_intf.name uri="^uri);
  let source = Rdf_types.node_of_uri_string wld.wld_world uri in
  Grdfs.name wld source
;;

let command_path wld uri =
  let source = Rdf_types.node_of_uri_string wld.wld_world uri in
  let pred = Grdfs.genet_haspath in
  Grdfs.target_literal wld source pred
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
  match List.rev (Rdf_sparql.select_and_fold wld.wld_world wld.wld_model query f []) with
    name :: _ -> Some name
  | [] -> None
;;

let do_add wld uri name =
  dbg ~level: 1 (fun () -> "Grdf_intf.do_add uri="^uri^" name="^name);
  let sub = Rdf_types.node_of_uri_string wld.wld_world uri in
  let cl = Rdf_types.node_of_uri_string wld.wld_world Grdfs.genet_intf in
  Grdfs.add_type wld.wld_world wld.wld_model ~sub ~obj: cl;
  Grdfs.add_name wld sub name
;;

let add wld ~parent name =
  dbg ~level: 1 (fun () -> "Grdf_intf.add parent="^parent^" name="^name);
  let node_parent = Rdf_types.node_of_uri_string wld.wld_world parent in
  let parent_is_tool = Grdfs.is_a_tool wld node_parent in
  let parent_is_branch = Grdfs.is_a_branch wld node_parent in
  if not (parent_is_tool || parent_is_branch) then
    Grdf_types.error (Grdf_types.Not_tool_or_branch parent);

  let tool = Grdf_branch.tool wld parent in
  let node_tool = Rdf_types.node_of_uri_string wld.wld_world tool in
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
  ~pred: (Rdf_types.node_of_uri_string wld.wld_world Grdfs.genet_hasintf)
  ~obj:  (Rdf_types.node_of_uri_string wld.wld_world uri);
  uri
;;

let sset_of_list = List.fold_left (fun set x -> Sset.add x set) Sset.empty ;;

let explicit_intfs_of wld uri =
  let source = Rdf_types.node_of_uri_string wld.wld_world uri in
  sset_of_list (Grdfs.target_uris wld source Grdfs.genet_hasintf)
;;

let explicit_no_intfs_of wld uri =
  let source = Rdf_types.node_of_uri_string wld.wld_world uri in
  sset_of_list (Grdfs.target_uris wld source Grdfs.genet_nointf)
;;

let intfs_of wld ?(recur=false) uri =
  dbg ~level: 1 (fun () -> "Grdf_intf.intfs uri="^uri);
  if recur then
    begin
      let rec iter set uri =
        let uris = explicit_intfs_of wld uri in
        let set = Sset.union set uris in
        match Grdf_branch.parent wld uri with
          None -> set
        | Some (uri, _) -> iter set uri
      in
      iter Sset.empty uri
    end
  else
    explicit_intfs_of wld uri
;;

let intfs_of_tool wld uri =
  let branches = Grdf_branch.subs wld ~recur: true uri in
  List.fold_left
    (fun acc b -> Sset.union acc (intfs_of wld b))
    (explicit_intfs_of wld uri) branches
;;

let compute_intfs_of wld uri =
  prerr_endline "Grdf_intf.compute_intfs_of: start";
  let rec inher = function
    None -> Sset.empty
  | Some (uri, _) ->
      let set = inher (Grdf_branch.parent wld uri) in
      let explicit = explicit_intfs_of wld uri in
      let explicit_no = explicit_no_intfs_of wld uri in
      Sset.union (Sset.diff set explicit_no) explicit
  in
  let node = Rdf_types.node_of_uri_string wld.wld_world uri in
  prerr_endline ("Grdf_intf.compute_intfs_of: node ok, uri="^uri);
  if Grdfs.is_a_tool wld node then
    (* show all interfaces *)
    (
     prerr_endline "Grdf_intf.compute_intfs_of: then";
     let ret = (intfs_of_tool wld uri, Sset.empty) in
     prerr_endline "Grdf_intf.compute_intfs_of: ok";
     ret
    )
  else
    begin
      prerr_endline "Grdf_intf.compute_intfs_of: else";
      let explicit = explicit_intfs_of wld uri in
      prerr_endline "Grdf_intf.compute_intfs_of: explicit ok";
      (* FIXME: when librdf_new_node_from_node will make a deep copy,
        remove the following line and use the previously defined node *)
      (*let node = Rdf_types.node_of_uri_string wld.wld_world uri in*)
      let parent =
        if Grdfs.is_a_version wld node then
          (
           prerr_endline "Grdf_intf.compute_intfs_of: is_a_version: true";
           match Grdf_version.parent wld uri with None -> None | Some uri -> Some (uri, false)
          )
        else
          (
           prerr_endline "Grdf_intf.compute_intfs_of: is_a_version: false";
           Grdf_branch.parent wld uri
          )
      in
      prerr_endline "Grdf_intf.compute_intfs_of: parent ok";
      let inherited = inher parent in
      prerr_endline "Grdf_intf.compute_intfs_of: inherited ok";
      (explicit, inherited)
    end
;;

let implementors wld uri =
  let world = wld.wld_world in
  let obj = Rdf_types.node_of_uri_string world uri in
  Grdfs.source_uris wld Grdfs.genet_hasintf obj
;;

let not_implementors wld uri =
  let world = wld.wld_world in
  let obj = Rdf_types.node_of_uri_string world uri in
  Grdfs.source_uris wld Grdfs.genet_nointf obj
;;

let tool_of_intf uri = Grdfs.uri_tool_of_intf uri

let string_of_intf wld ?(with_uri=true) uri =
  let ports_in = Grdf_port.ports wld uri Grdf_port.In in
  let ports_out = Grdf_port.ports wld uri Grdf_port.Out in
  Printf.sprintf "%s%s -> %s"
  (if with_uri then Printf.sprintf "%s : " uri else "")
  (Grdf_port.string_type_of_ports wld ~sep: " -> " ports_in)
  (Grdf_port.string_type_of_ports wld ~sep: " * " ports_out)
;;




