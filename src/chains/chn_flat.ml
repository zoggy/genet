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
      List.iter (add_op ctx path uri_fchain) chn.chn_ops;
      uri_fchain
    end

and add_op ctx path uri_fchain op =
    let path = path @ [op.op_name] in
    let uri_op = Grdfs.uri_fchain_op uri_fchain path in
    match op.op_from with
      Interface s -> ()
    | Chain fullname ->
        let src = do_flatten ctx ~path fullname in
        Grdfs.add_stmt_uris ctx.ctx_rdf.wld_world ctx.ctx_rdf.wld_model
          ~sub: uri_op ~pred: Grdfs.genet_opfrom ~obj: src;
        ()
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