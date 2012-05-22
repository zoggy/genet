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
  | (tool, intfs) :: q ->
      let active_versions = Grdf_version.active_versions_of
        ctx.ctx_rdf ~recur: true tool
      in
      let combs = f q in
      let f_version acc version =
        (* keep only versions implementing all the required interfaces *)
        let implemented =
          let (explicit, inherited) = Grdf_intf.compute_intfs_of ctx.ctx_rdf version in
          Uriset.union explicit inherited
        in
        if Uriset.for_all
          (fun intf -> Uriset.exists (Rdf_uri.equal intf) implemented)
          intfs
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

let equal_tool_versions = Urimap.equal Rdf_uri.equal;;

let instances ctx uri_fchain =
  let insts = Grdfs.subject_uris ctx.ctx_rdf
     ~pred: Grdfs.genet_instanciate ~obj: (Rdf_node.Uri uri_fchain)
  in
  let f acc uri_i =
    let versions = Grdfs.object_uris ctx.ctx_rdf
      ~sub: (Rdf_node.Uri uri_i) ~pred: Grdfs.genet_hasversion
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
    (uri_i, versions) :: acc
  in
  List.fold_left f [] insts
;;

(** @todo[3] This could be rewritten when OCaml-RDF offers a
    Sparql implementation. *)
let inst_chain_exists ctx uri_fchain comb =
  let insts = instances ctx uri_fchain in
  let pred (_, versions) = equal_tool_versions comb versions in
  try Some (fst (List.find pred insts))
  with Not_found -> None
;;

module Graph = Graph.Make_with_map
  (struct
     type t = Rdf_uri.uri
     let compare = Rdf_uri.compare
   end
  )
  (struct
     type t = Rdf_uri.uri * Rdf_uri.uri
     let compare (p1, p2) (p3, p4) =
       match Rdf_uri.compare p1 p3 with
         0 -> Rdf_uri.compare p2 p4
       | n -> n
   end)
;;

let create_graph ctx uri_fchain =
  let g = Graph.create () in
  let file_sym =
    let cpt = ref 0 in
    fun () -> incr cpt; Printf.sprintf "file%d" !cpt
  in
  let new_file map ports =
    let file = file_sym () in
    List.fold_left 
    (fun map uri_port -> Urimap.add uri_port file map)
    map ports
  in
  let f_consumer uri_src p_src (g, set) p_dst =
    let uri_dst = Grdfs.port_container p_dst in
    let g = Graph.add g (uri_src, uri_dst, (p_src, p_dst)) in
    let set = 
      let uri_from = Chn_flat.get_op_origin ctx uri_dst in
      match Grdf_intf.intf_exists ctx.ctx_rdf uri_from with
        None -> set
      | Some _ -> Uriset.add uri_dst set
    in
    (g, set)
  in
  let f_producer uri (g, map, set) p =
    try
      ignore(Urimap.find p map);
      (* port already handled; do nothing more *)
      (g, map, set)
    with
      Not_found ->
        let consumers = Chn_flat.port_consumers ctx p in
        let map = new_file map (p :: consumers) in
        let (g, set) = List.fold_left (f_consumer uri p) (g, set) consumers in
        (g, map, set)
  in
  let rec fill uri (g, map) =
    let dir = if Rdf_uri.equal uri uri_fchain 
      then Grdf_port.In
      else Grdf_port.Out
    in
    let ports = Grdf_port.ports ctx.ctx_rdf uri dir in
    let (g,map, set) = List.fold_left (f_producer uri)
      (g, map, Uriset.empty) ports
    in
    Uriset.fold fill set (g, map)
  in
  let (g, map) = fill uri_fchain (g, Urimap.empty) in
  (g, map)
;;


let do_instanciate ctx uri_fchain comb =
  let prefix = ctx.ctx_cfg.Config.rest_api in
  match Chn_types.is_uri_fchain prefix uri_fchain with
    None -> assert false
  | Some fchain_name ->
      let id = Misc.unique_id () in
      let inst_name = Chn_types.mk_ichain_name
        (Chn_types.fchain_chainname fchain_name) id
      in
      let uri_inst = Chn_types.uri_ichain prefix inst_name in
      Grdfs.add_triple_uris ctx.ctx_rdf
        ~sub: uri_inst ~pred: Grdfs.genet_instanciate ~obj: uri_fchain;
      Urimap.iter
        (fun _ version ->
          Grdfs.add_triple_uris ctx.ctx_rdf
            ~sub: uri_inst ~pred: Grdfs.genet_hasversion ~obj: version
        )
        comb;
      Grdfs.set_creation_date_uri ctx.ctx_rdf uri_inst ();
      let (g, port_to_file) = create_graph ctx uri_fchain in
      failwith "instanciate: not implemented!"
;;

let instanciate ctx uri_fchain comb =
  match inst_chain_exists ctx uri_fchain comb with
    Some uri -> uri
  | None ->
      ctx.ctx_rdf.wld_graph.transaction_start ();
      try
        let uri = do_instanciate ctx uri_fchain comb in
        ctx.ctx_rdf.wld_graph.transaction_commit();
        uri
      with
        e ->
          ctx.ctx_rdf.wld_graph.transaction_rollback ();
          raise e
;;

