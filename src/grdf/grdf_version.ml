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
  ~prefix: "Grdf_version"
    "GENET_GRDF_VERSION_DEBUG_LEVEL"
;;

let versions wld =
  dbg ~level: 1 (fun () -> "Grdf_version.versions");
  Grdfs.subject_iris wld
    ~pred: Grdfs.rdf_type ~obj: (Iri Grdfs.genet_version)
;;

let name wld iri = Grdfs.name wld (Iri iri);;

let parent wld iri =
  dbg ~level: 1 (fun () -> "Grdf_version.parent iri="^(Rdf_iri.string iri));
  let obj = Iri iri in
  Grdfs.subject_iri wld ~pred: Grdfs.genet_hasversion ~obj
;;

let version_exists wld iri =
  dbg ~level: 1 (fun () -> "Grdf_version.version_exists iri="^(Rdf_iri.string iri));
  if Grdfs.is_a_version wld iri then
    Some (name wld iri)
  else
    None
;;

let do_add wld iri name =
  dbg ~level: 1 (fun () -> "Grdf_version.do_add iri="^(Rdf_iri.string iri)^" name="^name);
  let sub = Iri iri in
  let obj = Grdfs.genet_version in
  Grdfs.add_type wld ~sub: iri ~obj;
  Grdfs.add_name wld sub name
;;

let add wld ~parent name =
  dbg ~level: 1 (fun () -> "Grdf_version.add parent="^(Rdf_iri.string parent)^" name="^name);
  let node_parent = Iri parent in

  let parent_is_tool = Grdfs.is_a_tool wld parent in
  let parent_is_branch = Grdfs.is_a_branch wld parent in

  if not (parent_is_tool || parent_is_branch) then
    Grdf_types.error (Grdf_types.Not_tool_or_branch parent);

  let tool =
    if parent_is_tool then parent else Grdf_branch.tool wld parent
  in
  let iri = Grdfs.iri_version tool name in

  match version_exists wld iri with
    Some name -> Grdf_types.error (Grdf_types.Version_exists name)
  | None ->
      do_add wld iri name;
      Grdfs.add_triple wld
      ~sub: node_parent
      ~pred: Grdfs.genet_hasversion
      ~obj:  (Iri iri);
      Grdfs.set_is_active_iri wld iri true;
      Grdfs.set_creation_date_iri wld iri ();
      iri
;;

let versions_of wld ?(recur=false) iri =
  dbg ~level: 1 (fun () -> "Grdf_branch.versions_of iri="^(Rdf_iri.string iri));
  if recur then
    begin
      let add set iri = Rdf_iri.Iriset.add iri set in
      let rec iter set iri =
        let versions = Grdfs.object_iris wld
          ~sub: (Iri iri) ~pred: Grdfs.genet_hasversion
        in
        let set = List.fold_left add set versions in
        let subs = Grdf_branch.subs wld iri in
        List.fold_left iter set subs
      in
      let set = iter Rdf_iri.Iriset.empty iri in
      Rdf_iri.Iriset.elements set
    end
  else
    (
     Grdfs.object_iris wld ~sub: (Iri iri) ~pred: Grdfs.genet_hasversion
    )
;;

let active_versions_of wld ?recur iri =
  let versions = versions_of wld ?recur iri in
  let pred = Grdfs.is_active_iri wld in
  List.filter pred versions
;;

let tool_of_version iri = Grdfs.iri_tool_of_version iri;;
