(** *)

open Grdf_types;;
open Chn_types;;
open Chn_ast;;

let fchain_exists ctx orig_fullname uri_fchain =
  let uri_chain = Grdfs.uri_chain ~prefix: ctx.ctx_cfg.Config.rest_api
    ~modname: (Chn_types.string_of_chain_modname (Chn_types.chain_modname orig_fullname))
    (Chn_types.string_of_chain_basename (Chn_types.chain_basename orig_fullname))
  in
  let world = ctx.ctx_rdf.wld_world in
  let sub = Rdf_node.new_from_uri_string world uri_chain in
  let pred = Rdf_node.new_from_uri_string world Grdfs.genet_flattenedto in
  let obj = Rdf_node.new_from_uri_string world uri_fchain in
  let stmt = Rdf_statement.new_from_nodes world ~sub ~pred ~obj in
  Rdf_model.contains_statement ctx.ctx_rdf.wld_model stmt
;;

let rec import_flat_op ctx uri_src uri_dst path = ()
(*
  assert (path <> []);
  let uri_op = Grdfs.uri_fchain_op uri_dst path in
  (* copy ports of the src flat chain to our target operation *)
  let in_ports = Grdf_port.ports ctx.ctx_rdf uri_src Grdf_port.In in
  let out_ports = Grdf_port.ports ctx.ctx_rdf uri_src Grdf_port.Out in
  Grdf_port.set_ports ctx.ctx_rdf uri_op Grdf_port.In in_ports ;
  Grdf_port.set_ports ctx.ctx_rdf uri_op Grdf_port.Out out_ports ;

  (* copy sub operations *)
  let sub_ops = Grdfs.target_uris ctx.ctx_rdf uri_src Grdfs.genet_containsop in
  List.iter (import_flat_sub_op ctx uri_dst path) sub_ops;
  (* TODO: copy edges *)

and import_flat_sub_op ctx uri_dst path uri_sub_op =
  match List.rev (Misc.split_string uri_sub_op ['/']) with
    [] -> assert false
  | name :: _ ->
      let path = path @ [name] in
      import_flat_op ctx uri_sub_op

;;
*)
let add_intf ctx uri_op loc intf_spec =
  let uri_intf = Chn_types.uri_intf_of_interface_spec
    ~prefix: ctx.ctx_cfg.Config.rest_api intf_spec
  in
  Grdfs.add_stmt_uris ctx.ctx_rdf.wld_world ctx.ctx_rdf.wld_model
    ~sub: uri_op ~pred: Grdfs.genet_opfrom ~obj: uri_intf;
  Grdf_port.copy_ports ctx.ctx_rdf ~src: uri_intf ~dst: uri_op ;
;;

let create_ports_from_chn ctx uri chn =
  let mk_port dir (rank, map, ports) p =
    let ftype =
      let mk_uri = Grdfs.uri_filetype ~prefix: ctx.ctx_cfg.Config.rest_api in
      match p.p_ftype with
        Grdf_port.One uri -> Grdf_port.One (mk_uri uri)
      | Grdf_port.List uri -> Grdf_port.List (mk_uri uri)
    in
    let ports = (rank, ftype, Some p.p_name) :: ports in
    let map = Smap.add p.p_name rank map in
    (rank + 1, map, ports)
  in
  let set_ports dir t =
    let (_, map, ports) = Array.fold_left (mk_port dir) (1, Smap.empty, []) t in
    Grdf_port.set_ports  ctx.ctx_rdf uri dir ports;
    map
  in
  let map_in = set_ports Grdf_port.In chn.chn_inputs in
  let map_out = set_ports Grdf_port.Out chn.chn_outputs in
  (map_in, map_out)
;;

let mk_port_map ctx uri dir =
  let wld = ctx.ctx_rdf in
  let ports = Grdf_port.ports wld uri dir in
  List.fold_left
  (fun map uri ->
     let rank = Grdf_port.port_rank uri in
     let name = Grdf_port.port_name wld uri in
     Smap.add name rank map
  )
  Smap.empty ports
;;

let find_port map dir edge_part =
  let (dir, op_name) =
    match dir, edge_part.ep_op with
    | Grdf_port.In, None -> (Grdf_port.Out, "")
    | Grdf_port.Out, None -> (Grdf_port.In, "")
    | _, Some s -> (dir, s)
  in
  let (uri, map_in, map_out) =
    try Smap.find op_name map
    with Not_found ->
        Loc.raise_problem edge_part.ep_loc
        (Printf.sprintf "No operation %S" op_name)
  in
  let rank =
    match edge_part.ep_port with
      Pint n -> n
    | Pname name ->
        let map =
          match dir with
            Grdf_port.In -> map_in
          | Grdf_port.Out -> map_out
        in
        try Smap.find name map
        with Not_found ->
            let msg = Printf.sprintf "Unknown port %s%s"
              (match op_name with "" -> "" | s -> s^".")
                name
            in
            Loc.raise_problem edge_part.ep_loc msg
  in
  let f =
    match dir with
      Grdf_port.In -> Grdfs.uri_intf_in_port
    | Grdf_port.Out -> Grdfs.uri_intf_out_port
  in
  f uri rank
;;

let create_data_edge ctx uri map edge =
  let uri_src = find_port map Grdf_port.Out edge.edge_src in
  let uri_dst = find_port map Grdf_port.In edge.edge_dst in
  let type_src = Grdf_port.port_type ctx.ctx_rdf uri_src in
  let type_dst = Grdf_port.port_type ctx.ctx_rdf uri_dst in
  let compat =
    match type_src, type_dst with
      Grdf_port.One u1, Grdf_port.One u2
    | Grdf_port.One u1, Grdf_port.List u2
    | Grdf_port.List u1, Grdf_port.One u2
    | Grdf_port.List u1, Grdf_port.List u2 -> u1 = u2
  in
  if not compat then
    Loc.raise_problem edge.edge_src.ep_loc
      (Printf.sprintf "Incompatible types: %s <--> %s"
        (Grdf_port.string_of_port_type ctx.ctx_rdf type_src)
        (Grdf_port.string_of_port_type ctx.ctx_rdf type_dst));
  (* TODO: check that two edges don't have the same destination *)
  Grdfs.add_stmt_uris ctx.ctx_rdf.wld_world ctx.ctx_rdf.wld_model
    ~sub: uri_src ~pred: Grdfs.genet_produces ~obj: uri_dst
;;

let create_data_edges ctx uri map chn =
  List.iter (create_data_edge ctx uri map) chn.chn_edges
;;

let rec do_flatten ctx ?(path=[]) fullname =
  let modname = Chn_types.chain_modname fullname in
  let name = Chn_types.chain_basename fullname in
  let file = Chn_io.file_of_modname ctx.ctx_cfg modname in
  let id = Misc.get_git_id file in
  let uri_fchain = Grdfs.uri_fchain ~prefix: ctx.ctx_cfg.Config.rest_api
    ~modname: (Chn_types.string_of_chain_modname modname)
    ~name: (Chn_types.string_of_chain_basename name) ~id
  in
  if fchain_exists ctx fullname uri_fchain then
    uri_fchain
  else
    begin
      let chn_mod = Chn_io.chn_module_of_file file in
      let chn =
        match Chn_ast.get_chain chn_mod name with
          None -> failwith (Printf.sprintf "Unbound chain %s" (Chn_types.string_of_chain_name fullname))
        | Some chn -> chn
      in
      let op_map =
        match path with
          [] ->
            let (map_in, map_out) = create_ports_from_chn ctx uri_fchain chn in
            Smap.singleton "" (uri_fchain, map_in, map_out)
        | _ ->  Smap.empty
      in
      let op_map = List.fold_left (add_op ctx path uri_fchain) op_map chn.chn_ops in
      create_data_edges ctx uri_fchain op_map chn;
      uri_fchain
    end

and add_op ctx path uri_fchain map op =
  let uri_parent =
    match path with
      [] -> uri_fchain
    | _ -> Grdfs.uri_fchain_op uri_fchain path
  in
  let path = path @ [op.op_name] in
  let uri_op = Grdfs.uri_fchain_op uri_fchain path in
  let add = Grdfs.add_stmt_uris ctx.ctx_rdf.wld_world ctx.ctx_rdf.wld_model in
  add ~sub: uri_parent ~pred: Grdfs.genet_containsop ~obj: uri_op;
  let (map_in, map_out) =
    match op.op_from with
      Interface s ->
        add_intf ctx uri_op op.op_from_loc s;
        (mk_port_map ctx uri_op Grdf_port.In,
         mk_port_map ctx uri_op Grdf_port.Out)
    | Chain fullname ->
        let src = do_flatten ctx ~path fullname in
        add ~sub: uri_op ~pred: Grdfs.genet_opfrom ~obj: src;
        import_flat_op ctx src uri_fchain path;
        (Smap.empty, Smap.empty)
  in
  Smap.add op.op_name (uri_op, map_in, map_out) map
;;

let flatten ctx fullname =
  Rdf_model.transaction_start ctx.ctx_rdf.wld_model;
  try
    let x = do_flatten ctx fullname in
    Rdf_model.transaction_commit ctx.ctx_rdf.wld_model;
    x
  with
    e ->
      prerr_endline "flatten: ERROR!";
      let s =
        Misc.string_of_opt
          (Rdf_model.to_string ctx.ctx_rdf.wld_model
         ~name: "turtle")
      in
      prerr_endline s;
      Rdf_model.transaction_rollback ctx.ctx_rdf.wld_model;
      raise e
;;