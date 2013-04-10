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
  ~prefix: "Grdf_diff"
    "GENET_GRDF_DIFF_DEBUG_LEVEL"
;;

let diff_commands wld =
  dbg ~level: 1 (fun () -> "Grdf_diff.diff_commands");
  let l = Grdfs.subject_uris wld ~pred: Grdfs.rdf_type ~obj: (Uri Grdfs.genet_diffcommand) in
  Grdfs.uriset_of_list l
;;

let name wld uri = Grdfs.name wld (Uri uri);;

let command_path wld uri =
  let pred = Grdfs.genet_haspath in
  Grdfs.object_literal wld ~sub: (Uri uri) ~pred
;;

let set_command_path wld uri path =
  let pred = Rdf_node.Uri Grdfs.genet_haspath in
  Grdfs.add_triple wld ~sub: (Rdf_node.Uri uri) ~pred
    ~obj: (Rdf_node.node_of_literal_string path)
;;

let diffcommand_exists wld uri =
  dbg ~level: 1 (fun () -> "Grdf_diff.diffcommand_exists uri="^(Rdf_uri.string uri));
  if Grdfs.is_a_diffcommand wld uri then
    Some (name wld uri)
  else
    None
;;

let do_add wld uri name =
  dbg ~level: 1 (fun () -> "Grdf_diff.do_add uri="^(Rdf_uri.string uri)^" name="^name);
  let sub = Uri uri in
  Grdfs.add_type wld ~sub ~obj: (Uri Grdfs.genet_diffcommand);
  Grdfs.add_name wld sub name
;;

let add prefix wld ?(force=false) ~name ~path =
  dbg ~level: 1 (fun () -> "Grdf_diff.add name="^name);
  let uri = Grdfs.uri_diffcommand ~prefix ~name in
  begin
    match diffcommand_exists wld uri with
      Some _ ->
        begin
          match force with
            false -> failwith ("Diff command already exists: "^(Rdf_uri.string uri))
          | true -> set_command_path wld uri path
        end
    | None ->
        do_add wld uri name;
        set_command_path wld uri path
  end;
  uri
;;