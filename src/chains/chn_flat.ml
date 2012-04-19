(** *)

open Rdf_node;;
open Rdf_graph;;
open Grdf_types;;
open Chn_types;;
open Chn_ast;;

let dbg = Misc.create_log_fun
  ~prefix: "Chn_flat"
    "GENET_CHN_FLAT_DEBUG_LEVEL"
;;

let fchain_exists ctx orig_fullname uri_fchain =
  let uri_chain = Chn_types.uri_chain ctx.ctx_cfg.Config.rest_api orig_fullname in
  ctx.ctx_rdf.wld_graph.exists ~sub: (Uri uri_chain)
   ~pred: (Uri Grdfs.genet_flattenedto) ~obj: (Uri uri_fchain) ()
;;

let get_ops ctx uri =
  Grdfs.object_uris ctx.ctx_rdf ~sub: (Uri uri) ~pred: Grdfs.genet_containsop
;;

let get_op_name uri =
  match List.rev (Rdf_uri.path uri) with
    [] -> failwith "Not a flat chain operation uri: "^(Rdf_uri.string uri)
  | name :: _ -> name
;;

let port_consumers ctx uri =
  Grdfs.object_uris ctx.ctx_rdf ~sub: (Uri uri) ~pred: Grdfs.genet_produces
;;

let rec import_flat_op ctx uri_src uri_dst path =

  assert (path <> []);
  let uri_op = Grdfs.uri_fchain_op uri_dst path in
  (* copy ports of the src flat chain to our target operation *)
(*
  let in_ports = Grdf_port.ports ctx.ctx_rdf uri_src Grdf_port.In in
  let out_ports = Grdf_port.ports ctx.ctx_rdf uri_src Grdf_port.Out in
  Grdf_port.set_ports ctx.ctx_rdf uri_op Grdf_port.In in_ports ;
  Grdf_port.set_ports ctx.ctx_rdf uri_op Grdf_port.Out out_ports ;
*)
  Grdf_port.copy_ports ctx.ctx_rdf ~src: uri_src ~dst: uri_op;
  (* copy sub operations *)
  let sub_ops = get_ops ctx uri_src in
  List.iter (import_flat_sub_op ctx uri_dst path) sub_ops
  (* TODO: copy edges *)

and import_flat_sub_op ctx uri_dst path uri_sub_op =
  let name = get_op_name uri_sub_op in
  let path = path @ [name] in
  import_flat_op ctx uri_sub_op (Rdf_uri.concat uri_dst name) path
;;

let add_intf ctx uri_op loc intf_spec =
  let dbg = dbg ~loc: "add_intf" in
  dbg ~level: 2
    (fun () ->
      Printf.sprintf "uri_op=%s intf_spec=%s" (Rdf_uri.string uri_op) intf_spec
    );
  let uri_intf = Chn_types.uri_intf_of_interface_spec
    ~prefix: ctx.ctx_cfg.Config.rest_api intf_spec
  in
  dbg ~level: 3
    (fun () -> Printf.sprintf "uri_intf=%s" (Rdf_uri.string uri_intf));
  Grdfs.add_triple_uris ctx.ctx_rdf
    ~sub: uri_op ~pred: Grdfs.genet_opfrom ~obj: uri_intf;
  Grdf_port.copy_ports ctx.ctx_rdf ~src: uri_intf ~dst: uri_op
;;

let create_ports_from_chn ctx uri chn =
  let dbg = dbg ~loc: "create_port_from_chn" in
  dbg ~level: 2
    (fun () -> Printf.sprintf "uri=%s chn=%s"
      (Rdf_uri.string uri) (Chn_types.string_of_chain_basename chn.chn_name));
  let mk_port dir (rank, map, ports) p =
    dbg ~level: 3
      (fun () -> Printf.sprintf "mk_port rank=%d p=%s" rank p.p_name);
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
    let (n, map, ports) = Array.fold_left (mk_port dir) (1, Smap.empty, []) t in
    Grdf_port.set_ports ctx.ctx_rdf uri dir ports;
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
    | Grdf_port.List u1, Grdf_port.List u2 -> Rdf_uri.equal u1 u2
  in
  if not compat then
    Loc.raise_problem edge.edge_src.ep_loc
      (Printf.sprintf "Incompatible types: %s <--> %s"
        (Grdf_port.string_of_port_type ctx.ctx_rdf type_src)
        (Grdf_port.string_of_port_type ctx.ctx_rdf type_dst));
  (* TODO: check that two edges don't have the same destination *)
  Grdfs.add_triple_uris ctx.ctx_rdf
    ~sub: uri_src ~pred: Grdfs.genet_produces ~obj: uri_dst
;;

let create_data_edges ctx uri map chn =
  List.iter (create_data_edge ctx uri map) chn.chn_edges
;;

let rec do_flatten ctx ?(path=[]) fullname =
  let dbg = dbg ~loc: "do_flatten" in
  dbg ~level: 2
    (fun () -> Printf.sprintf "path=%s fullname=%s"
      (String.concat "/" path) (Chn_types.string_of_chain_name fullname));
  let modname = Chn_types.chain_modname fullname in
  let name = Chn_types.chain_basename fullname in
  let file = Chn_io.file_of_modname ctx.ctx_cfg modname in
  let id = Misc.get_git_id file in
  let uri_fchain = Grdfs.uri_fchain ~prefix: ctx.ctx_cfg.Config.rest_api
    ~modname: (Chn_types.string_of_chain_modname modname)
    ~name: (Chn_types.string_of_chain_basename name) ~id
  in
  dbg ~level:3
    (fun () -> Printf.sprintf "uri_fchain=%s" (Rdf_uri.string uri_fchain));
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
        let (map_in, map_out) = create_ports_from_chn ctx uri_fchain chn in
        Smap.singleton "" (uri_fchain, map_in, map_out)
      in
      let op_map = List.fold_left (add_op ctx path uri_fchain) op_map chn.chn_ops in
      create_data_edges ctx uri_fchain op_map chn;
      let sub = Chn_types.uri_chain ctx.ctx_cfg.Config.rest_api fullname in
      Grdfs.add_triple_uris ctx.ctx_rdf
        ~sub ~pred: Grdfs.genet_flattenedto ~obj: uri_fchain;
      uri_fchain
    end

and add_op ctx path uri_fchain map op =
  let dbg = dbg ~loc: "add_op" in
  dbg ~level: 2
  (fun () ->
    Printf.sprintf "path=%s uri_fchain=%s op=%s"
      (String.concat "/" path) (Rdf_uri.string uri_fchain) op.op_name
  );
  let uri_parent =
    match path with
      [] -> uri_fchain
    | _ -> Grdfs.uri_fchain_op uri_fchain path
  in
  dbg ~level: 3
     (fun () -> Printf.sprintf "parent=%s" (Rdf_uri.string uri_parent));
  let path = path @ [op.op_name] in
  let uri_op = Grdfs.uri_fchain_op uri_fchain path in
  dbg ~level: 3
     (fun () -> Printf.sprintf "uri_op=%s" (Rdf_uri.string uri_op));
  let add = Grdfs.add_triple_uris ctx.ctx_rdf in
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
  ctx.ctx_rdf.wld_graph.transaction_start ();
  try
    let x = do_flatten ctx fullname in
    ctx.ctx_rdf.wld_graph.transaction_commit();
    x
  with
    e ->
      ctx.ctx_rdf.wld_graph.transaction_rollback ();
      raise e
;;


class fchain_dot_printer =
  let dotp = new Chn_ast.chain_dot_printer in
  object(self)
    method id s = "n"^(Digest.to_hex (Digest.string s))
    method uri_id uri = self#id (Rdf_uri.string uri)

    method color_of_port_dir = function
    | Grdf_port.In -> dotp#color_in
    | Grdf_port.Out -> dotp#color_out

    method print_port_edges ctx b uri =
      let ports = port_consumers ctx uri in
      let src = self#uri_id uri in
      let f p =
        Printf.bprintf b "%s -> %s ;\n" src (self#uri_id p)
      in
      List.iter f ports

    method print_port ctx b uri =
      let dir = Grdf_port.port_dir uri in
      let ft_name ftype = Grdf_ftype.name ctx.ctx_rdf ftype in
      let (link, ft) =
        match Grdf_port.port_type ctx.ctx_rdf uri with
          Grdf_port.One ftype -> (ftype, ft_name ftype)
        | Grdf_port.List ftype -> (ftype, Printf.sprintf "%s list" (ft_name ftype))
      in
      let id = self#uri_id uri in
      let label =
        match Grdf_port.port_name ctx.ctx_rdf uri with
          "" -> string_of_int (Grdf_port.port_rank uri)
        | s -> s
      in
      Printf.bprintf b "%s [color=\"black\" fillcolor=\"%s\" style=\"filled\" shape=\"box\" href=\"%s\" label=\"%s:%s\"];\n"
        id (self#color_of_port_dir dir) (Rdf_uri.string link) label ft

    method print_op ctx ?(root=false) b acc uri =
      if not root then
        Printf.bprintf b "subgraph cluster_%s {\n  label=%S;\n"
           (self#uri_id uri) (get_op_name uri);

      let f acc dir =
        if root then
          Printf.bprintf b "subgraph cluster_%s {\n"
          (Grdf_port.string_of_dir dir);
        let ports = Grdf_port.ports ctx.ctx_rdf uri dir in
        List.iter (self#print_port ctx b) ports;
        if root then Buffer.add_string b "}\n";
        List.fold_right Uriset.add ports acc
      in
      let acc = List.fold_left f acc [ Grdf_port.In ; Grdf_port.Out ] in
      let acc = List.fold_left (self#print_op ctx b) acc (get_ops ctx uri) in
      if not root then Buffer.add_string b "}\n";
      acc

    method dot_of_fchain ctx uri =
      let b = Buffer.create 256 in
      Buffer.add_string b "digraph g {\nrankdir=TB;\nfontsize=10;\n";
      let ports = self#print_op ctx ~root: true b Uriset.empty uri in
      Uriset.iter (self#print_port_edges ctx b) ports;
      Buffer.add_string b "}\n";
      Buffer.contents b
  end