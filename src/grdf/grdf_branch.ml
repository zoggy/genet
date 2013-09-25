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

(** *)

open Rdf_term;;
open Grdf_types;;

let dbg = Misc.create_log_fun
  ~prefix: "Grdf_branch"
    "GENET_GRDF_BRANCH_DEBUG_LEVEL"
;;

type t = { bch_name : string ; bch_iri : Rdf_iri.iri ; }

let branches wld =
  dbg ~level: 1 (fun () -> "Grdf_branch.branches");
  let iris = Grdfs.subject_iris wld
    ~pred: Grdfs.rdf_type
    ~obj: (Iri Grdfs.genet_branch)
  in
  let f acc iri =
    let name = Grdfs.name wld (Iri iri) in
    { bch_name = name ; bch_iri = iri} :: acc
  in
  List.fold_left f [] iris
;;

let name wld iri = Grdfs.name wld (Iri iri);;

let parent wld iri =
  dbg ~level: 1 (fun () -> "Grdf_branch.parent iri="^(Rdf_iri.string iri));
  let obj = Iri iri in
  let pred = Grdfs.genet_hasbranch in
  match Grdfs.subject_iri wld ~pred ~obj with
    None -> None
  | Some parent ->
    Some (parent, Grdfs.is_a_tool wld parent)
;;

let subs_aux wld iri =
  dbg ~level: 1 (fun () -> "Grdf_branch.subs iri="^(Rdf_iri.string iri));
  Grdfs.object_iris wld
    ~sub: (Iri iri) ~pred: Grdfs.genet_hasbranch
;;

let subs wld ?(recur=false) iri =
  if recur then
    begin
      let add set iri = Rdf_iri.Iriset.add iri set in
      let rec f acc iri =
        let l = subs_aux wld iri in
        let acc = List.fold_left add acc l in
        List.fold_left f acc l
      in
      Rdf_iri.Iriset.elements (f Rdf_iri.Iriset.empty iri)
    end
  else
    subs_aux wld iri
;;


let branch_exists wld iri =
  dbg ~level: 1 (fun () -> "Grdf_branch.branch_exists iri="^(Rdf_iri.string iri));
  if Grdfs.is_a_branch wld iri then
    Some (Grdfs.name wld (Iri iri))
  else
    None
;;

let do_add wld iri name =
  dbg ~level: 1 (fun () -> "Grdf_branch.do_add iri="^(Rdf_iri.string iri)^" name="^name);
  let sub = Iri iri in
  let obj = Iri Grdfs.genet_branch in
  Grdfs.add_type wld ~sub ~obj;
  Grdfs.add_name wld sub name
;;

let add wld ~parent name =
  dbg ~level: 1 (fun () -> "Grdf_branch.add parent="^(Rdf_iri.string parent)^" name="^name);
  let parent_is_tool = Grdfs.is_a_tool wld parent in
  let parent_is_branch = Grdfs.is_a_branch wld parent in
  if not (parent_is_tool || parent_is_branch) then
    Grdf_types.error (Grdf_types.Not_tool_or_branch parent);

  dbg ~level:2 (fun () -> "parent is ok");
  let iri =
    (if parent_is_tool
     then Grdfs.iri_branch_from_parent_tool
     else Grdfs.iri_branch_from_parent_branch)
    parent name
  in
  match branch_exists wld iri with
    Some name -> Grdf_types.error (Grdf_types.Branch_exists name)
  | None ->
      do_add wld iri name;
      Grdfs.add_triple wld
      ~sub: (Iri parent)
      ~pred: Grdfs.genet_hasbranch
      ~obj:  (Iri iri);
      iri
;;

let rec tool wld iri =
  dbg ~level: 1 (fun () -> "Grdf_branch.tool iri="^(Rdf_iri.string iri));
  if Grdfs.is_a_tool wld iri
  then iri
  else
    match parent wld iri with
      None -> iri
    | Some (parent, is_tool) -> if is_tool then parent else tool wld parent
;;

