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
  ~prefix: "Grdf_tool"
    "GENET_GRDF_TOOL_DEBUG_LEVEL"
;;

let tools wld =
  Grdfs.subject_iris wld
    ~pred: Grdfs.rdf_type
    ~obj: (Iri Grdfs.genet_tool)
;;

let name wld iri = Grdfs.name wld (Iri iri)
;;

let tool_exists wld iri =
  dbg ~level: 1 (fun () -> "Grdf_tool.tool_exists iri="^(Rdf_iri.string iri));
  if Grdfs.is_a_tool wld iri then
    Some (name wld iri)
  else
    None
;;

let add_tool wld name =
  let iri = Grdfs.iri_tool ~prefix: wld.wld_prefix ~tool: name in
  match tool_exists wld iri with
    Some name2 -> Grdf_types.error (Grdf_types.Tool_exists name2)
  | None ->
      let sub = Iri iri in
      Grdfs.add_type wld ~sub ~obj: (Iri Grdfs.genet_tool);
      Grdfs.add_name wld sub name;
      iri
;;

let branches wld iri = Grdf_branch.subs wld iri;;
