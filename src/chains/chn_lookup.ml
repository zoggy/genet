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

(** Looking up for instanciated chains. *)

type ichain_info = {
    ichain : Grdf_types.uri ;
    fchain : Grdf_types.uri ;
    input : string * string ; (* name * id *)
    tools : Grdf_types.uri Urimap.t ; (* version (uri) of each used tool *)
  }

type dist = {
    dist : int ;
    on_input : bool ;
    on_tools : (Grdf_types.uri option * Grdf_types.uri option) Urimap.t ;
      (* v1 and v2 or each difference of version regarding a tool *)

    on_fchain : bool ;
    on_chain : bool ;
  }

let dist_zero =
  { dist = 0 ;
    on_input = false ; on_tools = Urimap.empty ;
    on_fchain = false ; on_chain = false ;
  }

let tool_dist ctx dist tools1 tools2 =
  let f uri_tool v1 v2 =
    match v1, v2 with
      Some uri1, Some uri2 when Rdf_uri.equal uri1 uri2 ->
        None
    | _ -> Some (v1, v2)
  in
  let map = Urimap.merge f tools1 tools2 in
  { dist with
    dist = dist.dist + Urimap.cardinal map ;
    on_tools = map ;
  }
;;

let inst_chain_dist ctx ii1 ii2 =
  match Rdf_uri.equal ii1.ichain ii2.ichain with
  | true -> dist_zero
  | false ->
      let dist = dist_zero in
      let dist =
        match Rdf_uri.equal ii1.fchain ii2.fchain with
          true -> dist
        | false ->
            let dist = { dist with dist = dist.dist + 1 ; on_fchain = true } in
            match Chn_types.is_uri_fchain ctx ii1.fchain,
              Chn_types.is_uri_fchain ctx ii2.fchain
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

let ichain_info ctx uri_inst =
  let input =
    match Chn_inst.inst_input ctx uri_inst with
      None -> assert false
    | Some x -> x
  in
  let fchain =
    match Chn_inst.instance_source ctx uri_inst with
      None -> assert false
    | Some uri -> uri
  in
  { ichain = uri_inst ;
    fchain ;
    input ;
    tools = Chn_inst.inst_versions ctx uri_inst ;
  }
;;

let get_dist_instances ctx uri_inst =
  match Chn_inst.instance_source ctx uri_inst with
    None -> assert false
  | Some uri_fchain ->
      let instances = Grdfs.subject_uris ctx.Chn_types.ctx_rdf
        ~pred: Grdfs.genet_instanciate ~obj: (Rdf_node.Uri uri_fchain)
      in
      let infos = List.map (ichain_info ctx) instances in
      let info_inst = ichain_info ctx uri_inst in
      List.map
        (fun info -> (info.ichain, inst_chain_dist ctx info_inst info))
        infos
;;

let make_graph ctx uri_inst =
  let dists = get_dist_instances ctx uri_inst in
  let node_atts uri =
    match Chn_types.is_uri_ichain ctx.Chn_types.ctx_cfg.Config.rest_api uri with
      None -> assert false
    | Some name ->
        Printf.sprintf "label=%S, href=%S"
          (Chn_types.string_of_ichain_name name)
          (Rdf_uri.string uri)
  in
  let id uri = Grdf_dot.md5 (Rdf_uri.string uri) in
  let b = Buffer.create 256 in
  let print_node (uri,d) =
    Printf.bprintf b "N%s [%s];\n" (id uri) (node_atts uri);
    if d <> dist_zero then
      begin
        Printf.bprintf b "N%s -- N%s [label=\"%d\"];\n"
        (id uri_inst) (id uri) d.dist
      end
  in
  Buffer.add_string b "graph g {\n";
  List.iter print_node dists;
  Buffer.add_string b "}\n";
  Buffer.contents b
;;



