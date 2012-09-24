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

(** Querying for instanciated chains. *)

open Chn_types;;

let uriset_add set x = Uriset.add x set;;
let uriset_of_list ?(acc=Uriset.empty) list =
  List.fold_left uriset_add acc list
;;

let instances_of_input ctx ?acc ?id input_path =
  let input_name = String.concat "/" input_path in
  let list = Grdfs.subject_uris ctx.ctx_rdf
    ~pred: Grdfs.genet_useinput
    ~obj: (Rdf_node.node_of_literal_string input_name)
  in
  let list =
    match id with
      None -> list
    | Some id ->
        let list_by_id = Grdfs.subject_uris ctx.ctx_rdf
          ~pred: Grdfs.genet_useinputcommitid
          ~obj: (Rdf_node.node_of_literal_string id)
        in
        let by_id = List.fold_right
          Uriset.add list_by_id Uriset.empty
        in
        List.filter (fun uri -> Uriset.mem uri by_id) list
  in
  uriset_of_list ?acc list
;;

let instances_of_fchain ctx ?acc uri_fchain =
  let list = Grdfs.subject_uris ctx.ctx_rdf
    ~pred: Grdfs.genet_instanciate
    ~obj: (Rdf_node.Uri uri_fchain)
  in
  uriset_of_list ?acc list
;;

let instances_of_chain ctx ?(acc=Uriset.empty) uri =
  let prefix = ctx.ctx_cfg.Config.rest_api in
  match Chn_types.is_uri_chain prefix uri with
    None ->
      begin
        match Chn_types.is_uri_fchain ctx uri with
          None -> acc
        | Some _ -> instances_of_fchain ctx ~acc uri
      end
  | Some chain_name ->
      (* return instanciated chains for all chains flattened from chain_name *)
      let f acc uri_fchain = instances_of_fchain ctx ~acc uri_fchain in
      let fchains = Chn_flat.flat_chains_of_chain ctx chain_name in
      List.fold_left f acc fchains
;;

let instances_of_tools ctx ?base tools =
  let versions = Urimap.fold (fun _ version acc -> version :: acc) tools [] in
  match versions with
    [] -> base
  | _ ->
      let (start, q) =
        match base with
          Some set -> (set, versions)
        | None ->
            match versions with
              [] -> assert false
            | v :: q ->
                let list = Grdfs.subject_uris ctx.ctx_rdf
                  ~pred: Grdfs.genet_useversion
                  ~obj: (Rdf_node.Uri v)
                in
                (uriset_of_list list, q)
      in
      let f set version =
        (* small opt: do nothing if base set empty *)
        match Uriset.is_empty set with
          true -> set
        | false ->
            let list = Grdfs.subject_uris ctx.ctx_rdf
              ~pred: Grdfs.genet_useversion
              ~obj: (Rdf_node.Uri version)
            in
            let set2 = uriset_of_list list in
            Uriset.inter set set2
      in
      Some (List.fold_left f start q)
;;

(* TODO: part of this function and functions above
  should be rewritten when sparql will be implemented in ocaml-rdf *)
let query_instances ctx ?input ?chain ~tools =
  match input, chain, Urimap.is_empty tools with
    None, None, true -> []
  | _ ->
      let set =
        match input with
          None -> None
        | Some (input, id) -> Some (instances_of_input ctx ?id input)
      in
      let set =
        match chain with
          None -> set
        | Some uri ->
            let set2 = instances_of_chain ctx uri in
            match set with
              None -> Some set2
            | Some set -> Some (Uriset.inter set set2)
      in
      let set =
        match Urimap.is_empty tools with
          true -> set
        | false -> instances_of_tools ctx ?base: set tools
      in
      match set with
        None -> []
      | Some set -> Uriset.fold (fun x acc -> x :: acc) set []
;;
