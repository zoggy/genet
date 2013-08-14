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

open Rdf_node;;
open Grdf_types;;

let dbg = Misc.create_log_fun
  ~prefix: "Grdf_branch"
    "GENET_GRDF_BRANCH_DEBUG_LEVEL"
;;

type t = { bch_name : string ; bch_uri : uri ; }

let branches wld =
  dbg ~level: 1 (fun () -> "Grdf_branch.branches");
  let uris = Grdfs.subject_uris wld
    ~pred: Grdfs.rdf_type
    ~obj: (Uri Grdfs.genet_branch)
  in
  let f acc uri =
    let name = Grdfs.name wld (Uri uri) in
    { bch_name = name ; bch_uri = uri} :: acc
  in
  List.fold_left f [] uris
;;

let name wld uri = Grdfs.name wld (Uri uri);;

let parent wld uri =
  dbg ~level: 1 (fun () -> "Grdf_branch.parent uri="^(Rdf_uri.string uri));
  let obj = Uri uri in
  let pred = Grdfs.genet_hasbranch in
  match Grdfs.subject_uri wld ~pred ~obj with
    None -> None
  | Some parent ->
    Some (parent, Grdfs.is_a_tool wld parent)
;;

let subs_aux wld uri =
  dbg ~level: 1 (fun () -> "Grdf_branch.subs uri="^(Rdf_uri.string uri));
  Grdfs.object_uris wld
    ~sub: (Uri uri) ~pred: Grdfs.genet_hasbranch
;;

let subs wld ?(recur=false) uri =
  if recur then
    begin
      let add set uri = Uriset.add uri set in
      let rec f acc uri =
        let l = subs_aux wld uri in
        let acc = List.fold_left add acc l in
        List.fold_left f acc l
      in
      Uriset.elements (f Uriset.empty uri)
    end
  else
    subs_aux wld uri
;;


let branch_exists wld uri =
  dbg ~level: 1 (fun () -> "Grdf_branch.branch_exists uri="^(Rdf_uri.string uri));
  if Grdfs.is_a_branch wld uri then
    Some (Grdfs.name wld (Uri uri))
  else
    None
;;

let do_add wld uri name =
  dbg ~level: 1 (fun () -> "Grdf_branch.do_add uri="^(Rdf_uri.string uri)^" name="^name);
  let sub = Uri uri in
  let obj = Uri Grdfs.genet_branch in
  Grdfs.add_type wld ~sub ~obj;
  Grdfs.add_name wld sub name
;;

let add wld ~parent name =
  dbg ~level: 1 (fun () -> "Grdf_branch.add parent="^(Rdf_uri.string parent)^" name="^name);
  let parent_is_tool = Grdfs.is_a_tool wld parent in
  let parent_is_branch = Grdfs.is_a_branch wld parent in
  if not (parent_is_tool || parent_is_branch) then
    Grdf_types.error (Grdf_types.Not_tool_or_branch parent);

  dbg ~level:2 (fun () -> "parent is ok");
  let uri =
    (if parent_is_tool
     then Grdfs.uri_branch_from_parent_tool
     else Grdfs.uri_branch_from_parent_branch)
    parent name
  in
  match branch_exists wld uri with
    Some name -> Grdf_types.error (Grdf_types.Branch_exists name)
  | None ->
      do_add wld uri name;
      Grdfs.add_triple wld
      ~sub: (Uri parent)
      ~pred: (Uri Grdfs.genet_hasbranch)
      ~obj:  (Uri uri);
      uri
;;

let rec tool wld uri =
  dbg ~level: 1 (fun () -> "Grdf_branch.tool uri="^(Rdf_uri.string uri));
  if Grdfs.is_a_tool wld uri
  then uri
  else
    match parent wld uri with
      None -> uri
    | Some (parent, is_tool) -> if is_tool then parent else tool wld parent
;;

