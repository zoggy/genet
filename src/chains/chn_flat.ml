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
      let _op_map = List.fold_left (add_op ctx path uri_fchain) Smap.empty chn.chn_ops in
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
  begin
    match op.op_from with
      Interface s -> add_intf ctx uri_op op.op_from_loc s
    | Chain fullname ->
        let src = do_flatten ctx ~path fullname in
        add ~sub: uri_op ~pred: Grdfs.genet_opfrom ~obj: src;
        import_flat_op ctx src uri_fchain path;
        ()
  end;
  Smap.add op.op_name uri_op map
;;

let flatten ctx fullname =
  Rdf_model.transaction_start ctx.ctx_rdf.wld_model;
  try
    let x = do_flatten ctx fullname in
    Rdf_model.transaction_commit ctx.ctx_rdf.wld_model;
    x
  with
    e ->
      Rdf_model.transaction_rollback ctx.ctx_rdf.wld_model;
      raise e
;;