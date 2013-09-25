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

open Rdf_term;;
open Grdf_types;;

let dbg = Misc.create_log_fun
  ~prefix: "Grdf_intf"
    "GENET_GRDF_INTF_DEBUG_LEVEL"
;;

let intfs wld =
  dbg ~level: 1 (fun () -> "Grdf_intf.intfs");
  let l = Grdfs.subject_iris wld ~pred: Grdfs.rdf_type ~obj: (Iri Grdfs.genet_intf) in
  Grdfs.iriset_of_list l
;;

let name wld iri = Grdfs.name wld (Iri iri);;

let command_path wld iri =
  let pred = Grdfs.genet_haspath in
  Grdfs.object_literal wld ~sub: (Iri iri) ~pred
;;

let set_command_path wld iri path =
  let pred = Grdfs.genet_haspath in
  Grdfs.add_triple wld ~sub: (Rdf_term.Iri iri) ~pred
    ~obj: (Rdf_term.term_of_literal_string path)
;;

let intf_exists wld iri =
  dbg ~level: 1 (fun () -> "Grdf_intf.intf_exists iri="^(Rdf_iri.string iri));
  if Grdfs.is_a_intf wld iri then
    Some (name wld iri)
  else
    None
;;

let do_add wld iri name =
  dbg ~level: 1 (fun () -> "Grdf_intf.do_add iri="^(Rdf_iri.string iri)^" name="^name);
  let sub = Iri iri in
  Grdfs.add_type wld ~sub ~obj: (Iri Grdfs.genet_intf);
  Grdfs.add_name wld sub name
;;

let add wld ~parent name =
  dbg ~level: 1 (fun () -> "Grdf_intf.add parent="^(Rdf_iri.string parent)^" name="^name);
  let node_parent = Iri parent in
  let parent_is_tool = Grdfs.is_a_tool wld parent in
  let parent_is_branch = Grdfs.is_a_branch wld parent in
  if not (parent_is_tool || parent_is_branch) then
    Grdf_types.error (Grdf_types.Not_tool_or_branch parent);

  let tool = Grdf_branch.tool wld parent in
  if not (Grdfs.is_a_tool wld tool) then
    Grdf_types.error (Grdf_types.Not_a_tool tool);
  let iri = Grdfs.iri_intf ~tool ~intf: name in
  begin
    match intf_exists wld iri with
      Some _ -> ()
    | None -> do_add wld iri name
  end;
  (* eventually remove information about not implementing this interface *)
  Grdfs.rem_triple wld
    ~sub: node_parent
    ~pred: Grdfs.genet_nointf
    ~obj:  (Iri iri);

  Grdfs.add_triple wld
    ~sub: node_parent
    ~pred: Grdfs.genet_hasintf
    ~obj:  (Iri iri);

  iri
;;

let add_no_intf wld ~parent iri =
  dbg ~level: 1 (fun () -> "Grdf_intf.add_no_intf parent="^(Rdf_iri.string parent)^" iri="^(Rdf_iri.string iri));
  match intf_exists wld iri with
    None -> failwith (Printf.sprintf "No such interface: %S" (Rdf_iri.string iri))
  | Some _ ->
      let parent_is_branch = Grdfs.is_a_branch wld parent in
      let parent_is_version = Grdfs.is_a_version wld parent in
      if not (parent_is_branch || parent_is_version) then
        Grdf_types.error (Grdf_types.Not_branch_or_version parent);
      let node_parent = Iri parent in
      (* eventually remove information about implementing this interface *)
      Grdfs.rem_triple wld
        ~sub: node_parent
        ~pred: Grdfs.genet_hasintf
        ~obj:  (Iri iri);

      Grdfs.add_triple wld
        ~sub: node_parent
        ~pred: Grdfs.genet_nointf
        ~obj:  (Iri iri)
;;

let explicit_intfs_of wld iri =
  Grdfs.iriset_of_list (Grdfs.object_iris wld ~sub: (Iri iri) ~pred: Grdfs.genet_hasintf)
;;

let explicit_no_intfs_of wld iri =
  Grdfs.iriset_of_list (Grdfs.object_iris wld ~sub: (Iri iri) ~pred: Grdfs.genet_nointf)
;;

let intfs_of wld ?(recur=false) iri =
  dbg ~level: 1 (fun () -> "Grdf_intf.intfs iri="^(Rdf_iri.string iri));
  if recur then
    begin
      let rec iter set iri =
        let iris = explicit_intfs_of wld iri in
        let set = Rdf_iri.Iriset.union set iris in
        match Grdf_branch.parent wld iri with
          None -> set
        | Some (iri, _) -> iter set iri
      in
      iter Rdf_iri.Iriset.empty iri
    end
  else
    explicit_intfs_of wld iri
;;

let intfs_of_tool wld iri =
  let branches = Grdf_branch.subs wld ~recur: true iri in
  List.fold_left
    (fun acc b -> Rdf_iri.Iriset.union acc (intfs_of wld b))
    (explicit_intfs_of wld iri) branches
;;

let compute_intfs_of wld iri =
  (*prerr_endline "Grdf_intf.compute_intfs_of: start";*)
  let rec inher = function
    None -> Rdf_iri.Iriset.empty
  | Some (iri, _) ->
      let set = inher (Grdf_branch.parent wld iri) in
      let explicit = explicit_intfs_of wld iri in
      let explicit_no = explicit_no_intfs_of wld iri in
      Rdf_iri.Iriset.union (Rdf_iri.Iriset.diff set explicit_no) explicit
  in
  (*prerr_endline ("Grdf_intf.compute_intfs_of: node ok, iri="^(Rdf_iri.string iri));*)
  if Grdfs.is_a_tool wld iri then
    (* show all interfaces *)
    (
     (*prerr_endline "Grdf_intf.compute_intfs_of: then";*)
     let ret = (intfs_of_tool wld iri, Rdf_iri.Iriset.empty, Rdf_iri.Iriset.empty) in
     (*prerr_endline "Grdf_intf.compute_intfs_of: ok";*)
     ret
    )
  else
    begin
      (*prerr_endline "Grdf_intf.compute_intfs_of: else";*)
      let explicit = explicit_intfs_of wld iri in
      (*prerr_endline "Grdf_intf.compute_intfs_of: explicit ok";*)
      let parent =
        if Grdfs.is_a_version wld iri then
          (
           (*prerr_endline "Grdf_intf.compute_intfs_of: is_a_version: true";*)
           match Grdf_version.parent wld iri with None -> None | Some iri -> Some (iri, false)
          )
        else
          (
           (*prerr_endline "Grdf_intf.compute_intfs_of: is_a_version: false";*)
           Grdf_branch.parent wld iri
          )
      in
      (*prerr_endline "Grdf_intf.compute_intfs_of: parent ok";*)
      let inherited = inher parent in
      let explicit_no = explicit_no_intfs_of wld iri in
      (*prerr_endline "Grdf_intf.compute_intfs_of: inherited ok";*)
      (explicit, explicit_no, inherited)
    end
;;

let implementors wld iri =
  Grdfs.subject_iris wld ~pred: Grdfs.genet_hasintf ~obj: (Iri iri)
;;

let not_implementors wld iri =
  Grdfs.subject_iris wld ~pred: Grdfs.genet_nointf ~obj: (Iri iri)
;;

let tool_of_intf iri = Grdfs.iri_tool_of_intf iri

let tools_of_intfs set =
  let f iri_intf acc =
    Rdf_iri.Iriset.add (tool_of_intf iri_intf) acc
  in
  Rdf_iri.Iriset.fold f set Rdf_iri.Iriset.empty
;;

let string_of_intf wld ?(with_iri=true) iri =
  let ports_in = Grdf_port.ports wld iri Grdf_port.In in
  let ports_out = Grdf_port.ports wld iri Grdf_port.Out in
  let f = Grdf_port.string_of_port_type (fun x -> x) in
  Printf.sprintf "%s%s -> %s"
  (if with_iri then Printf.sprintf "%s : " (Rdf_iri.string iri) else "")
  (Grdf_port.string_type_of_ports wld f ~sep: " -> " ports_in)
  (Grdf_port.string_type_of_ports wld f ~sep: " * " ports_out)
;;

let get_port wld iri ?(pos=max_int) dir =
  let n = List.length (Grdf_port.ports wld iri dir) in
  let p = max 1 (min n pos) in
  let f = Grdf_port.iri_intf_port_of_dir dir in
  f iri p
;;

let additional_tools_used wld iri =
  Grdfs.object_iris wld ~sub: (Rdf_term.Iri iri) ~pred: Grdfs.genet_usetool
;;



