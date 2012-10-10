(*********************************************************************************)
(*                Genet                                                          *)
(*                                                                               *)
(*    Copyright (C) 2012 Institut National de Recherche en Informatique          *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License version 3             *)
(*    or later as published by the Free Software Foundation.                     *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software Foundation,    *)
(*    Inc., 59 Temple Place, Suite 330, Boston, MA                               *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*                                                                               *)
(*********************************************************************************)

(** *)

open Rdf_graph;;
open Grdf_types;;
open Chn_types;;

let dbg = Misc.create_log_fun
  ~prefix: "Chn_inst"
    "GENET_CHN_INST_DEBUG_LEVEL"
;;
let instance_source ctx inst_uri =
  Grdfs.object_uri ctx.ctx_rdf
    ~sub: (Rdf_node.Uri inst_uri)
    ~pred: Grdfs.genet_instanciate
;;

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
      dbg ~level: 2 (fun () ->
         Printf.sprintf "required intfs for tool %s:\n%s"
          (Rdf_uri.string tool)
          (String.concat "\n"
            (Uriset.fold (fun uri acc -> (Rdf_uri.string uri)::acc) required_intfs []))
      );
       let active_versions = Grdf_version.active_versions_of
        ctx.ctx_rdf ~recur: true tool
      in
      dbg ~level: 2 (fun () -> Printf.sprintf "active_versions_of %s: %d"
         (Rdf_uri.string tool) (List.length active_versions));
      let combs = f q in
      let f_version acc version =
        (* keep only versions implementing all the required interfaces *)
        let implemented =
          let (explicit, inherited) = Grdf_intf.compute_intfs_of ctx.ctx_rdf version in
          Uriset.union explicit inherited
        in
        dbg ~level:2
          (fun () -> Printf.sprintf "Implemented:\n%s"
            (String.concat "\n"
              (Uriset.fold (fun uri acc -> (Rdf_uri.string uri) :: acc) implemented []))
        );
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

let inst_versions ctx uri_inst =
  let versions = Grdfs.object_uris ctx.ctx_rdf
    ~sub: (Rdf_node.Uri uri_inst) ~pred: Grdfs.genet_useversion
  in
  List.fold_left
  (fun acc v -> Urimap.add
     (Grdf_version.tool_of_version v)
     v
     acc)
    Urimap.empty
  versions
;;

let instances ctx uri_fchain =
  let insts = Grdfs.subject_uris ctx.ctx_rdf
     ~pred: Grdfs.genet_instanciate ~obj: (Rdf_node.Uri uri_fchain)
  in
  let f acc uri_i =
    let versions = inst_versions ctx uri_i in
    let input = inst_input ctx uri_i in
    (uri_i, versions, input) :: acc
  in
  List.fold_left f [] insts
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

let create_flat_graph ctx uri_fchain =
  let g = Graph.create () in
  let f_consumer uri_src p_src (g, set) p_dst =
    let uri_dst = Grdfs.port_container p_dst in
    if Rdf_uri.equal uri_dst uri_fchain then
      begin
        let g = Graph.add g
         (Chn_run.Flat uri_src, Chn_run.Flat uri_dst,
          (Chn_run.Flat p_src, Chn_run.Flat p_dst)
         ) in
        (g, set)
      end
    else
      begin
        let g = Graph.add g
           (Chn_run.Flat uri_src, Chn_run.Flat uri_dst,
            (Chn_run.Flat p_src, Chn_run.Flat p_dst)
           ) in
        let set =
          let uri_from = Chn_flat.get_op_origin ctx uri_dst in
          match uri_from with
           | _ when Rdf_uri.equal uri_from Grdfs.genet_explode ->
             Uriset.add uri_dst set
           | _ when Rdf_uri.equal uri_from Grdfs.genet_implode ->
             Uriset.add uri_dst set
           | _ ->
             match Grdf_intf.intf_exists ctx.ctx_rdf uri_from with
               None -> set
             | Some _ -> Uriset.add uri_dst set
        in
        (g, set)
      end
  in
  let f_producer uri (g, port_set, set) p =
    if Uriset.mem p port_set then
      (
       dbg ~level: 1 (fun () -> Printf.sprintf "port %s already seen, stop" (Rdf_uri.string p));
       (* port already handled; do nothing more *)
       (g, port_set, set)
      )
    else
       (
        let consumers = Chn_flat.port_consumers ctx p in
        dbg ~level: 1
         (fun () -> Printf.sprintf "%d consumers for port %s"
            (List.length consumers) (Rdf_uri.string p));
        let port_set = Uriset.add p port_set in
        let (g, set) = List.fold_left
          (f_consumer uri p) (g, set) consumers
        in
        (g, port_set, set)
       )
  in
  let rec fill uri (g, port_set) =
    dbg ~level: 1 (fun () -> Printf.sprintf "fill uri=%s" (Rdf_uri.string uri));
    let dir =
      if Rdf_uri.equal uri uri_fchain
      then Grdf_port.In
      else Grdf_port.Out
    in
    let ports = Grdf_port.ports ctx.ctx_rdf uri dir in
    dbg ~level: 1 (fun () -> Printf.sprintf "%d %s port(s)" (List.length ports) (Grdf_port.string_of_dir dir));
    let (g, port_set, set) = List.fold_left (f_producer uri)
      (g, port_set, Uriset.empty) ports
    in
    Uriset.fold fill set (g, port_set)
  in
  let (g, _) = fill uri_fchain (g, Uriset.empty) in
  g
;;



let do_instanciate ctx reporter uri_fchain input comb =
   let prefix = ctx.ctx_cfg.Config.rest_api in
  match Chn_types.is_uri_fchain ctx uri_fchain with
    None -> assert false
  | Some fchain_name ->
      let id = Misc.unique_id () in
      let inst_name = Chn_types.mk_ichain_name
        (Chn_types.fchain_chainname fchain_name) id
      in
      let uri_inst = Chn_types.uri_ichain prefix inst_name in
      Grdfs.add_type ctx.ctx_rdf
        ~sub: (Rdf_node.Uri uri_inst)
        ~obj: (Rdf_node.Uri Grdfs.genet_instchain);
      Grdfs.add_triple_uris ctx.ctx_rdf
        ~sub: uri_inst ~pred: Grdfs.genet_instanciate ~obj: uri_fchain;
      (* associate tool versions *)
      Urimap.iter
        (fun _ version ->
          Grdfs.add_triple_uris ctx.ctx_rdf
            ~sub: uri_inst ~pred: Grdfs.genet_useversion ~obj: version
        )
        comb;

      (* associate input files *)
      set_input_info ctx uri_inst input;
      Grdfs.set_creation_date_uri ctx.ctx_rdf uri_inst ();

      let g = create_flat_graph ctx uri_fchain in
      let dot = Chn_run.dot_of_graph ctx g in
      Misc.file_of_string ~file: "/tmp/inst.dot" dot;

      Chn_run.run ctx reporter ~inst: uri_inst ~fchain: uri_fchain input comb g ;
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
      |  e ->
          ctx.ctx_rdf.wld_graph.transaction_rollback ();
          raise e
;;


