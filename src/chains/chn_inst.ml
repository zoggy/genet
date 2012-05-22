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
              [tool, version] :: acc
          | _ ->
              (List.map (fun comb -> (tool, version) :: comb) combs) @ acc
        else
          acc
      in
      List.fold_left f_version [] active_versions
  in
  f intf_tools
;;