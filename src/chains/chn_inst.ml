(** *)

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

(** @todo[3] This could be rewritten when a OCaml-RDF offers a
    Sparql implementation *)
let inst_chain_exists ctx uri_fchain comb =
  let insts = instances ctx uri_fchain in
  let pred (_, versions) = equal_tool_versions comb versions in
  try
    Some (fst (List.find pred insts))
  with
    Not_found ->
      None
;;

let instanciate ctx uri_fchain combs =
  match inst_chain_exists ctx uri_fchain combs with
    Some uri -> uri
  | None -> failwith "instanciate: not implemented!"
;;


