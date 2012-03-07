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
  List.rev (Rdf_sparql.select_and_fold wld.wld_world wld.wld_model query f [])
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
  match List.rev (Rdf_sparql.select_and_fold wld.wld_world wld.wld_model query f []) with
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

let intfs_of_tool wld uri =
  let branches = Grdf_branch.subs wld ~recur: true uri in
  let add set uri = Sset.add uri set in
  let set = List.fold_left
    (fun acc b -> List.fold_left add acc (intfs_of wld b))
    Sset.empty branches
  in
  Sset.elements set
;;

let implementors wld uri =
  let world = wld.wld_world in
  let obj = Rdf_node.new_from_uri_string world uri in
  Grdfs.source_uris wld Grdfs.genet_hasintf obj
;;

let not_implementors wld uri =
  let world = wld.wld_world in
  let obj = Rdf_node.new_from_uri_string world uri in
  Grdfs.source_uris wld Grdfs.genet_nointf obj
;;

let tool_of_intf uri = Grdfs.uri_tool_of_intf uri

type port = One of uri | List of uri;;
type dir = In | Out ;;

let pred_of_dir = function
| In -> Grdfs.genet_consumes
| Out -> Grdfs.genet_produces
;;

let ports wld dir uri =
  let pred = pred_of_dir dir in
  let f acc n node =
    match Grdfs.target_uri wld node Grdfs.genet_listof with
      None ->
        begin
          match Rdf_node.get_uri node with
            None -> acc
          | Some uri -> (n, One (Rdf_uri.as_string uri)) :: acc
        end
    | Some uri -> (n, List uri) :: acc
  in
  let l = Grdfs.fold_target_sequence f [] wld ~source: uri ~pred in
  List.sort (fun (n1, _) (n2, _) -> Pervasives.compare n1 n2) l
;;

let insert_port wld seq_node (n, port) =
  let world = wld.wld_world in
  let li = Rdf_node.new_from_uri_string world (Grdfs.li_ n) in
  match port with
    One uri ->
      let obj = Rdf_node.new_from_uri_string world uri in
      Grdfs.add_stmt world wld.wld_model
        ~sub: seq_node ~pred: li ~obj
  | List uri ->
      let obj = Rdf_node.new_from_blank_identifier world in
      Grdfs.add_stmt world wld.wld_model
        ~sub: seq_node ~pred: li ~obj;
      let ftype = Rdf_node.new_from_uri_string world uri in
      let pred = Rdf_node.new_from_uri_string world Grdfs.genet_listof in
      Grdfs.add_stmt world wld.wld_model
        ~sub: obj ~pred ~obj: ftype
;;

let delete_ports wld dir uri =
  let pred = pred_of_dir dir in
  let query =
    let triples1 = [
        (`I uri, [ `I pred, [`V "seq"] ] );
        (`V "seq", [ `V "seq_index", [`V "uri"] ])
      ]
    in
    let triples2 =
      [ (`V "uri", [ `I Grdfs.genet_listof, [ `V "uri2"] ]) ]
    in
    let where =
      (triples1, Some (`Optional (triples2, None)))
    in
    { Rdf_sparql.select_proj = [ "seq" ; "seq_index" ; "uri" ; "uri2" ] ;
      select_distinct = None ;
      select_where = where;
    }
  in
  let get = Rdf_query_results.get_binding_value in
  let sub = Rdf_node.new_from_uri_string wld.wld_world uri in
  let pred = Rdf_node.new_from_uri_string wld.wld_world pred in
  let pred_listof = Rdf_node.new_from_uri_string wld.wld_world Grdfs.genet_listof in
  let f () qr =
    match get qr 0, get qr 1, get qr 2 with
      None, _, _ | _, None, _ | _, _, None -> ()
    | Some seq, Some seq_index, Some uri ->
        Grdfs.remove_stmt wld.wld_world wld.wld_model
          ~sub ~pred ~obj: seq;
        Grdfs.remove_stmt wld.wld_world wld.wld_model
          ~sub: seq ~pred: seq_index ~obj: uri;
        match Rdf_node.kind uri with
        | Rdf_node.Blank _ ->
            begin
              match get qr 4 with
                None -> ()
              | Some obj ->
                  Grdfs.remove_stmt wld.wld_world wld.wld_model
                  ~sub: uri ~pred: pred_listof ~obj
            end
        | _ -> ()
  in
  Rdf_sparql.select_and_fold wld.wld_world wld.wld_model query f ()
;;
(*
let delete_ports wld dir uri =
  let pred = pred_of_dir dir in
  let query =
    let triples1 = [
        (`I uri, [ `I pred, [`V "seq"] ] );
        (`V "seq", [ `V "seq_index", [`V "uri"] ]) ;
      ]
    in
    let triples2 =
      [ (`V "uri", [ `I Grdfs.genet_listof, [ `V "uri2"]]) ]
    in
    { Rdf_sparql.delins_insert = None ;
      delins_delete = Some (Some (triples1 @ triples2), []) ;
      delins_where = (triples1, Some (`Optional (triples2, None))) ;
    }
  in
  let query = Rdf_sparql.Delete_insert query in
  ignore (Rdf_sparql.exec wld.wld_world wld.wld_model query)
;;
*)
let set_ports wld dir uri ports =
  let pred = pred_of_dir dir in
  delete_ports wld dir uri ;
  let source = Rdf_node.new_from_uri_string wld.wld_world uri in
  let obj = Rdf_node.new_from_blank_identifier wld.wld_world in
  let pred = Rdf_node.new_from_uri_string wld.wld_world pred in
  Grdfs.add_stmt wld.wld_world wld.wld_model
    ~sub: source ~pred ~obj;
  List.iter (insert_port wld obj) ports
;;

let add_port wld dir uri ?(pos=max_int) filetype =
  let ports = ports wld dir uri in
  let rec iter inserted m acc = function
    [] ->
      let acc =
        if not inserted then
         (m, filetype) :: acc
        else
          acc
      in
      List.rev acc
  | h :: q ->
      if pos <= m && not inserted then
        iter true (m+1) ((m, filetype)::acc) (h :: q)
      else
        iter inserted (m+1) (h :: acc) q
  in
  let new_ports = iter false 1 [] ports in
  set_ports wld dir uri new_ports
;;

let string_of_port wld = function
  One uri -> Grdf_ftype.extension wld uri
| List uri -> Printf.sprintf "%s list" (Grdf_ftype.extension wld uri)
;;

let string_of_intf wld ?(with_uri=true) uri =
  let ports_in = ports wld In uri in
  let ports_out = ports wld Out uri in
  let type_of sep = function
    [] -> "()"
  | l ->
      String.concat sep
      (List.map (fun (_, p) -> string_of_port wld p) l)
  in
  Printf.sprintf "%s%s -> %s"
  (if with_uri then Printf.sprintf "%s : " uri else "")
  (type_of " -> " ports_in) (type_of " * " ports_out)
;;




