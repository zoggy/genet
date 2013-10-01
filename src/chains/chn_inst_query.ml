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

(** Querying for instanciated chains. *)

open Chn_types;;

module Iriset = Rdf_iri.Iriset
module Irimap = Rdf_iri.Irimap

let iriset_add set x = Iriset.add x set;;
let iriset_of_list ?(acc=Iriset.empty) list =
  List.fold_left iriset_add acc list
;;

let instances_of_input ctx ?acc ?id input_path =
  let input_name = String.concat "/" input_path in
  let list = Grdfs.subject_iris ctx.ctx_rdf
    ~pred: Grdfs.genet_useinput
    ~obj: (Rdf_term.term_of_literal_string input_name)
  in
  let list =
    match id with
      None -> list
    | Some id ->
        let list_by_id = Grdfs.subject_iris ctx.ctx_rdf
          ~pred: Grdfs.genet_useinputcommitid
          ~obj: (Rdf_term.term_of_literal_string id)
        in
        let by_id = List.fold_right
          Iriset.add list_by_id Iriset.empty
        in
        List.filter (fun iri -> Iriset.mem iri by_id) list
  in
  iriset_of_list ?acc list
;;

let instances_of_fchain ctx ?acc iri_fchain =
  let list = Grdfs.subject_iris ctx.ctx_rdf
    ~pred: Grdfs.genet_instanciate
    ~obj: (Rdf_term.Iri iri_fchain)
  in
  iriset_of_list ?acc list
;;

let instances_of_chain ctx ?(acc=Iriset.empty) iri =
  let prefix = ctx.ctx_cfg.Config.rest_api in
  match Chn_types.is_iri_chain prefix iri with
    None ->
      begin
        match Chn_types.is_iri_fchain ctx iri with
          None -> acc
        | Some _ -> instances_of_fchain ctx ~acc iri
      end
  | Some chain_name ->
      (* return instanciated chains for all chains flattened from chain_name *)
      let f acc iri_fchain = instances_of_fchain ctx ~acc iri_fchain in
      let fchains = Chn_flat.flat_chains_of_chain ctx chain_name in
      List.fold_left f acc fchains
;;

let instances_of_tools ctx ?base tools =
  let versions = Irimap.fold (fun _ version acc -> version :: acc) tools [] in
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
                let list = Grdfs.subject_iris ctx.ctx_rdf
                  ~pred: Grdfs.genet_usetoolversion
                  ~obj: (Rdf_term.Iri v)
                in
                (iriset_of_list list, q)
      in
      let f set version =
        (* small opt: do nothing if base set empty *)
        match Iriset.is_empty set with
          true -> set
        | false ->
            let list = Grdfs.subject_iris ctx.ctx_rdf
              ~pred: Grdfs.genet_usetoolversion
              ~obj: (Rdf_term.Iri version)
            in
            let set2 = iriset_of_list list in
            Iriset.inter set set2
      in
      Some (List.fold_left f start q)
;;

(* TODO: part of this function and functions above
  should be rewritten when sparql will be implemented in ocaml-rdf *)
let query_instances ctx ?input ?chain ~tools =
  match input, chain, Irimap.is_empty tools with
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
        | Some iri ->
            let set2 = instances_of_chain ctx iri in
            match set with
              None -> Some set2
            | Some set -> Some (Iriset.inter set set2)
      in
      let set =
        match Irimap.is_empty tools with
          true -> set
        | false -> instances_of_tools ctx ?base: set tools
      in
      match set with
        None -> []
      | Some set -> Iriset.fold (fun x acc -> x :: acc) set []
;;
