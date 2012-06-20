(** *)

open Rdf_graph;;
open Grdf_types;;
open Chn_types;;


let version_combinations ctx fchain =
  let intfs = Chn_flat.intfs_of_flat_chain ctx fchain in
  let intfs_by_tool = Uriset.fold
    (fun intf acc ->
       let tool = Grdf_intf.tool_of_intf intf in
       let set =
         try Urimap.find tool acc
         with Not_found -> Uriset.empty
       in
       let set = Uriset.add intf set in
       Urimap.add tool set acc
    )
    intfs Urimap.empty
  in
  let intf_tools = Urimap.fold (fun tool intfs acc -> (tool, intfs) :: acc)
    intfs_by_tool []
  in
  let rec f = function
    [] -> []
  | (tool, required_intfs) :: q ->
      prerr_endline (Printf.sprintf
              "required intfs for tool %s:" (Rdf_uri.string tool));
      Uriset.iter (fun uri -> prerr_endline (Rdf_uri.string uri)) required_intfs;
      let active_versions = Grdf_version.active_versions_of
        ctx.ctx_rdf ~recur: true tool
      in
      prerr_endline (Printf.sprintf "active_versions_of %s: %d"
        (Rdf_uri.string tool) (List.length active_versions));
      let combs = f q in
      let f_version acc version =
        (* keep only versions implementing all the required interfaces *)
        let implemented =
          let (explicit, inherited) = Grdf_intf.compute_intfs_of ctx.ctx_rdf version in
          Uriset.union explicit inherited
        in
        prerr_endline "*** implemented =";
        Uriset.iter
          (fun uri -> prerr_endline (Rdf_uri.string uri)) implemented;
        if Uriset.for_all
          (fun intf -> Uriset.exists (Rdf_uri.equal intf) implemented)
          required_intfs
        then
          match combs with
            [] ->
              (Urimap.singleton tool version) :: acc
          | _ ->
              (List.map (fun comb -> Urimap.add tool version comb) combs) @ acc
        else
          acc
      in
      List.fold_left f_version [] active_versions
  in
  f intf_tools
;;

let inst_input ctx uri_inst =
  let input = Grdfs.object_literal ctx.ctx_rdf
    ~sub: (Rdf_node.Uri uri_inst) ~pred: Grdfs.genet_useinput
  in
  let input_id = Grdfs.object_literal ctx.ctx_rdf
    ~sub: (Rdf_node.Uri uri_inst) ~pred: Grdfs.genet_useinputcommitid
  in
  match input, input_id with
    None, _ | _, None -> None
  | Some s, Some id -> Some (s, id)
;;

let equal_tool_versions = Urimap.equal Rdf_uri.equal;;

let instances ctx uri_fchain =
  let insts = Grdfs.subject_uris ctx.ctx_rdf
     ~pred: Grdfs.genet_instanciate ~obj: (Rdf_node.Uri uri_fchain)
  in
  let f acc uri_i =
    let versions = Grdfs.object_uris ctx.ctx_rdf
      ~sub: (Rdf_node.Uri uri_i) ~pred: Grdfs.genet_useversion
    in
    let versions =
      List.fold_left
      (fun acc v -> Urimap.add
         (Grdf_version.tool_of_version v)
         v
         acc)
      Urimap.empty
      versions
    in
    let input = inst_input ctx uri_i in
    (uri_i, versions, input) :: acc
  in
  List.fold_left f [] insts
;;

(** @todo[3] This could be rewritten when OCaml-RDF offers a
    Sparql implementation. *)
let inst_chain_exists ctx uri_fchain input comb =
  let insts = instances ctx uri_fchain in
  let input =
    (input.Ind_types.from_in_data,
     Misc.get_git_id input.Ind_types.dir)
  in
  let pred (_, versions, input_info) =
    equal_tool_versions comb versions &&
    (match input_info with None -> false | Some i -> i = input)
  in
  try
    let (uri_i, _, _) = List.find pred insts in
    Some uri_i
  with Not_found -> None
;;

module Graph = Chn_run.Graph;;

let copy_flat_port ctx ~inst ~parent ~flat_port =
  let inst_port = Chn_types.uri_inst_port_of_flat_port
    ctx ~inst ~flat: flat_port
  in
  let ptype = Grdf_port.port_type ctx.ctx_rdf flat_port in
  Grdf_port.set_port_type ctx.ctx_rdf inst_port ptype;
  Grdfs.add_triple_uris ctx.ctx_rdf
    ~sub: inst_port ~pred: Grdfs.genet_opfrom ~obj: flat_port;

  let pred = Grdf_port.pred_of_dir (Grdf_port.port_dir flat_port) in
  Grdfs.add_triple_uris ctx.ctx_rdf ~sub: parent ~pred ~obj: inst_port;
  inst_port
;;

let create_graph ctx ~inst ~fchain input =
  let g = Graph.create () in
  let file_sym =
    let cpt = ref 0 in
    fun () -> incr cpt; Printf.sprintf "file%d" !cpt
  in
  let new_file map ports =
    let file =
      let name = file_sym () in
      let ext =
        match ports with
          [] -> ""
        | p :: _ ->
            let typ = Grdf_port.port_type ctx.ctx_rdf p in
            match Grdf_port.port_file_type_uri ctx.ctx_cfg.Config.rest_api typ with
              None -> ""
            | Some uri -> (Grdf_ftype.extension ctx.ctx_rdf uri)
      in
      Printf.sprintf "%s.%s" name ext
    in
    List.fold_left
    (fun map uri_port -> Urimap.add uri_port file map)
    map ports
  in
  let f_consumer inst_uri_src flat_uri_src inst_p_src (g, set) p_dst =
    let flat_uri_dst = Grdfs.port_container p_dst in
    let inst_uri_dst =
      Chn_types.uri_inst_opn_of_flat_opn
      ~prefix: ctx.ctx_cfg.Config.rest_api ~inst ~flat: flat_uri_dst
    in
    let inst_p_dst = copy_flat_port ctx
      ~inst ~parent: inst_uri_dst ~flat_port: p_dst
    in

    if Rdf_uri.equal flat_uri_dst fchain then
      begin
        let g = Graph.add g (inst_uri_src, inst_uri_dst, (inst_p_src, inst_p_dst)) in
        (g, set)
      end
    else
      begin
        let g = Graph.add g
          (inst_uri_src, inst_uri_dst, (inst_p_src, inst_p_dst))
        in
        let set =
          let uri_from = Chn_flat.get_op_origin ctx flat_uri_dst in
          match Grdf_intf.intf_exists ctx.ctx_rdf uri_from with
            None -> set
          | Some _ -> Uriset.add flat_uri_dst set
        in
        (g, set)
      end
  in
  let f_producer inst_uri flat_uri (g, map, set) p =
    try
      ignore(Urimap.find p map);
      (* port already handled; do nothing more *)
      (g, map, set)
    with
      Not_found ->
        let inst_p = copy_flat_port ctx ~inst ~parent: inst_uri ~flat_port: p in
        let consumers = Chn_flat.port_consumers ctx p in
        let inst_consumers = List.map
          (fun flat -> Chn_types.uri_inst_port_of_flat_port ctx ~inst ~flat)
          consumers
        in
        let map = new_file map (inst_p :: inst_consumers) in
        let (g, set) = List.fold_left
          (f_consumer inst_uri flat_uri inst_p) (g, set) consumers
        in
        (g, map, set)
  in
  let rec fill flat_uri (g, map) =
    let dir =
      if Rdf_uri.equal flat_uri fchain
      then Grdf_port.In
      else Grdf_port.Out
    in
    let inst_uri =
      if Rdf_uri.equal flat_uri fchain then
        inst
      else
        (
         let sub = Chn_types.uri_inst_opn_of_flat_opn
           ~prefix: ctx.ctx_cfg.Config.rest_api ~inst ~flat: flat_uri
         in
         Grdfs.add_triple_uris ctx.ctx_rdf
         ~sub ~pred: Grdfs.genet_opfrom ~obj: flat_uri;

         Grdfs.add_type ctx.ctx_rdf
         ~sub: (Rdf_node.Uri sub) ~obj: (Rdf_node.Uri Grdfs.genet_instopn);

         Grdfs.add_triple_uris ctx.ctx_rdf
           ~sub: inst ~pred: Grdfs.genet_containsop ~obj: sub;

         sub
        )
    in
    let ports = Grdf_port.ports ctx.ctx_rdf flat_uri dir in
    let (g,map, set) = List.fold_left (f_producer inst_uri flat_uri)
      (g, map, Uriset.empty) ports
    in
    Uriset.fold fill set (g, map)
  in
  let (g, map) = fill fchain (g, Urimap.empty) in
  (g, map)
;;

let dot_of_graph ctx g port_to_file =
  let f_edge (p1, p2) =
    let type1 = Grdf_port.port_type ctx.ctx_rdf p1 in
    let type2 = Grdf_port.port_type ctx.ctx_rdf p2 in
    let f p = Grdf_port.string_of_port_type (fun x -> x) p in
    let label = Printf.sprintf "%s:%s\\n%s:%s" (f type1) (f type2)
       (Urimap.find p1 port_to_file) (Urimap.find p2 port_to_file)
    in
    (label, [])
  in
  let f_node uri =
    let label =
      try
        let uri_from = Chn_flat.get_op_origin ctx uri in
        match Grdf_intf.intf_exists ctx.ctx_rdf uri_from with
          None -> Filename.basename (Rdf_uri.string uri)
        | Some name -> name
      with
        _ ->
          Filename.basename (Rdf_uri.string uri)
    in
    let href = Rdf_uri.string uri in
    let id = "n"^(Digest.to_hex (Digest.string href)) in
    (id, label, [ "href", href ])
  in
  Graph.dot_of_graph ~f_edge ~f_node g
;;

let set_input_info ctx uri_inst input =
  let input_name = input.Ind_types.from_in_data in
  let input_id = Misc.get_git_id input.Ind_types.dir in

  let sub = Rdf_node.Uri uri_inst in

  let pred = Rdf_node.Uri Grdfs.genet_useinput in
  let obj = Rdf_node.node_of_literal_string input_name in
  Grdfs.add_triple ctx.ctx_rdf ~sub ~pred ~obj;

  let pred = Rdf_node.Uri Grdfs.genet_useinputcommitid in
  let obj = Rdf_node.node_of_literal_string input_id in
  Grdfs.add_triple ctx.ctx_rdf ~sub ~pred ~obj;
;;

let do_instanciate ctx reporter uri_fchain input comb =
  let prefix = ctx.ctx_cfg.Config.rest_api in
  match Chn_types.is_uri_fchain prefix uri_fchain with
    None -> assert false
  | Some fchain_name ->
      let id = Misc.unique_id () in
      let inst_name = Chn_types.mk_ichain_name
        (Chn_types.fchain_chainname fchain_name) id
      in
      let uri_inst = Chn_types.uri_ichain prefix inst_name in
      prerr_endline (Printf.sprintf "do_instanciate: uri_inst = %S" (Rdf_uri.string uri_inst));
      Grdfs.add_triple_uris ctx.ctx_rdf
        ~sub: uri_inst ~pred: Grdfs.genet_instanciate ~obj: uri_fchain;

      (* associate tool versions *)
      Urimap.iter
        (fun _ version ->
          Grdfs.add_triple_uris ctx.ctx_rdf
            ~sub: uri_inst ~pred: Grdfs.genet_useversion ~obj: version
        )
        comb;

      let obj = Rdf_node.node_of_literal_string input.Ind_types.from_in_data in
      let pred = Rdf_node.Uri Grdfs.genet_useinput in
      Grdfs.add_triple ctx.ctx_rdf ~sub: (Rdf_node.Uri uri_inst) ~pred ~obj;

      (* associate input files (with commit ids); they're supposed to be ordered by rank *)
      set_input_info ctx uri_inst input;

      Grdfs.set_creation_date_uri ctx.ctx_rdf uri_inst ();
      let (g, port_to_file) = create_graph ctx
        ~inst: uri_inst ~fchain: uri_fchain input
      in

      prerr_endline "generating dot";
      Misc.file_of_string ~file: "/tmp/inst.dot" (dot_of_graph ctx g port_to_file);

      ignore(Chn_run.run_graph ctx reporter uri_inst comb input g port_to_file);
      uri_inst
;;

let instanciate ctx reporter uri_fchain input comb =
  match inst_chain_exists ctx uri_fchain input comb with
    Some uri -> uri
  | None ->
      ctx.ctx_rdf.wld_graph.transaction_start ();
      try
        let uri = do_instanciate ctx reporter uri_fchain input comb in
        ctx.ctx_rdf.wld_graph.transaction_commit();
        uri
      with
        e ->
          ctx.ctx_rdf.wld_graph.transaction_rollback ();
          raise e
;;


