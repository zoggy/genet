(*********************************************************************************)
(*                Genet                                                          *)
(*                                                                               *)
(*    Copyright (C) 2012-2013 Institut National de Recherche en Informatique     *)
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

(** Looking up for instanciated chains. *)

module Irimap = Rdf_iri.Irimap

type ichain_info = {
    ichain : Grdf_types.iri ;
    fchain : Grdf_types.iri ;
    input : [`Relative] Fname.filename * string ; (* name * id *)
    tools : Grdf_types.iri Irimap.t ; (* version (iri) of each used tool *)
  }

type dist = {
    dist : int ;
    on_input : bool ;
    on_tools : (Grdf_types.iri option * Grdf_types.iri option) Irimap.t ;
      (* v1 and v2 or each difference of version regarding a tool *)

    on_fchain : bool ;
    on_chain : bool ;
  }

let dist_zero =
  { dist = 0 ;
    on_input = false ; on_tools = Irimap.empty ;
    on_fchain = false ; on_chain = false ;
  }

let tool_dist ctx dist tools1 tools2 =
  let f iri_tool v1 v2 =
    match v1, v2 with
      Some iri1, Some iri2 when Rdf_iri.equal iri1 iri2 ->
        None
    | _ -> Some (v1, v2)
  in
  let map = Irimap.merge f tools1 tools2 in
  { dist with
    dist = dist.dist + Irimap.cardinal map ;
    on_tools = map ;
  }
;;

let inst_chain_dist ctx ii1 ii2 =
  match Rdf_iri.equal ii1.ichain ii2.ichain with
  | true -> dist_zero
  | false ->
      let dist = dist_zero in
      let dist =
        match Rdf_iri.equal ii1.fchain ii2.fchain with
          true -> dist
        | false ->
            let dist = { dist with dist = dist.dist + 1 ; on_fchain = true } in
            match Chn_types.is_iri_fchain ctx ii1.fchain,
              Chn_types.is_iri_fchain ctx ii2.fchain
            with
              None, _
            | _, None -> dist
            | Some fname1, Some fname2 ->
                let cn1 = Chn_types.fchain_chainname fname1 in
                let cn2 = Chn_types.fchain_chainname fname2 in
                match Chn_types.compare_chain_name cn1 cn2 with
                  0 -> dist
                | _ -> { dist with dist = dist.dist + 1 ; on_chain = true }
      in
      let dist = tool_dist ctx dist ii1.tools ii2.tools in
      let dist =
        match ii1.input = ii2.input with
          true -> dist
        | false ->
            { dist with dist = dist.dist + 1 ; on_input = true }
      in
      dist
;;

let ichain_info ctx iri_inst =
  let input =
    match Chn_inst.inst_input ctx iri_inst with
      None -> assert false
    | Some x -> x
  in
  let fchain =
    match Chn_inst.instance_source ctx iri_inst with
      None -> assert false
    | Some iri -> iri
  in
  { ichain = iri_inst ;
    fchain ;
    input ;
    tools = Chn_inst.inst_versions ctx iri_inst ;
  }
;;

let get_dist_instances ctx iri_inst =
  match Chn_inst.instance_source ctx iri_inst with
    None -> assert false
  | Some iri_fchain ->
      (*let instances = Grdfs.subject_iris ctx.Chn_types.ctx_rdf
        ~pred: Grdfs.genet_instanciate ~obj: (Rdf_node.Iri iri_fchain)
      in*)
      let instances =
        let g = ctx.Chn_types.ctx_rdf.Grdf_types.wld_graph in
        let l = g.Rdf_graph.find ~pred: Grdfs.genet_instanciate () in
        let f acc = function
          (Rdf_term.Iri iri, _, _) -> iri :: acc
        | _ -> acc
        in
        List.fold_left f [] l
      in
      let infos = List.map (ichain_info ctx) instances in
      let info_inst = ichain_info ctx iri_inst in
      List.map
        (fun info -> (info.ichain, inst_chain_dist ctx info_inst info))
        infos
;;

let make_graph ctx iri_inst =
  let dists = get_dist_instances ctx iri_inst in
  let edge_atts dist =
    let for_tools =
      if Irimap.is_empty dist.on_tools then
        ""
      else
        (
         let f_v = function
           None -> "-"
         | Some iri -> Grdf_version.name ctx.Chn_types.ctx_rdf iri
         in
         let f tool (v1, v2) acc =
           Printf.sprintf "%s %s:%s/%s" acc
             (Grdf_tool.name ctx.Chn_types.ctx_rdf tool)
             (f_v v1) (f_v v2)
         in
         Irimap.fold f dist.on_tools ""
        )
     in
     Printf.sprintf "label=%S"
      (Printf.sprintf "%d%s" dist.dist for_tools)
  in
  let node_atts iri =
    match Chn_types.is_iri_ichain ctx.Chn_types.ctx_cfg.Config.rest_api iri with
      None -> assert false
    | Some name ->
        Printf.sprintf "%slabel=%S, href=%S"
          (if Rdf_iri.equal iri iri_inst then "root=true, " else "")
          (Chn_types.string_of_ichain_name name)
          (Rdf_iri.string iri)
  in
  let id iri = Grdf_dot.md5 (Rdf_iri.string iri) in
  let b = Buffer.create 256 in
  let print_node (iri,d) =
    Printf.bprintf b "N%s [%s];\n" (id iri) (node_atts iri);
    if d <> dist_zero then
      begin
        Printf.bprintf b "N%s -> N%s [%s];\n"
        (id iri_inst) (id iri) (edge_atts d)
      end
  in
  Buffer.add_string b "digraph g {\nranksep=\"5,4\";\n";
  List.iter print_node dists;
  Buffer.add_string b "}\n";
  Buffer.contents b
;;



