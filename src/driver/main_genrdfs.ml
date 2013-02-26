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

(** Main module of the program generating the RDFschema of genet. *)

open Rdf_node;;

let add_triple = Grdfs.add_triple;;

let add_isdefined_by_genet wld sub =
  let pred = Uri (Grdfs.rdfs_"isDefinedBy") in
  let obj = Uri Grdfs.genet in
  add_triple wld ~sub ~pred ~obj
;;

let add_label wld sub label =
  let pred = Uri (Grdfs.rdfs_"label") in
  let obj = Rdf_node.node_of_literal_string label in
  add_triple wld ~sub ~pred ~obj
;;

let add_comment wld sub label =
  let pred = Uri (Grdfs.rdfs_"comment") in
  let obj = Rdf_node.node_of_literal_string label in
  add_triple wld ~sub ~pred ~obj
;;

let add_type = Grdfs.add_type;;

let add_istype_class wld sub =
  let obj = Uri (Grdfs.rdfs_"Class") in
  add_type wld ~sub ~obj
;;

let add_istype_property wld sub =
  let obj = Uri (Grdfs.rdfs_"Property") in
  add_type wld ~sub ~obj
;;

let add_property wld
  ~label ~comment ~definedby ?domain ?range sub =
  add_istype_property wld sub;
  add_label wld sub label;
  add_comment wld sub comment;

  let pred = Uri (Grdfs.rdfs_"isDefinedBy") in
  add_triple wld ~sub ~pred ~obj: definedby;

  (match domain with
     None -> ()
   | Some obj ->
       let pred = Uri (Grdfs.rdfs_"domain") in
       add_triple wld ~sub ~pred ~obj
  );

  (match range with
     None -> ()
   | Some obj ->
       let pred = Uri (Grdfs.rdfs_"range") in
       add_triple wld ~sub ~pred ~obj
  )
;;

let add_tool_class wld =
  let uri_tool = Uri Grdfs.genet_tool in
  add_istype_class wld uri_tool;
  add_isdefined_by_genet wld uri_tool;
  add_label wld uri_tool "Tool"
;;

let add_branch_class wld =
  let uri_branch = Uri Grdfs.genet_branch in
  add_istype_class wld uri_branch;
  add_isdefined_by_genet wld uri_branch;
  add_label wld uri_branch "Branch"
;;

let add_intf_class wld =
  let uri_intf = Uri Grdfs.genet_intf in
  add_istype_class wld uri_intf;
  add_isdefined_by_genet wld uri_intf;
  add_label wld uri_intf "Interface"
;;

let add_version_class wld =
  let uri_version = Uri Grdfs.genet_version in
  add_istype_class wld uri_version;
  add_isdefined_by_genet wld uri_version;
  add_label wld uri_version "Version"
;;

let add_filetype_class wld =
  let uri_ft = Uri Grdfs.genet_filetype in
  add_istype_class wld uri_ft;
  add_isdefined_by_genet wld uri_ft;
  add_label wld uri_ft "Filetype"
;;

let add_vocabulary wld =
  add_tool_class wld;
  add_branch_class wld;
  add_intf_class wld;
  add_version_class wld;
  add_filetype_class wld;

  let node_literal = Uri (Grdfs.rdfs_"Literal") in
  let node_branch = Uri Grdfs.genet_branch in
  let node_intf = Uri Grdfs.genet_intf in
  let node_version = Uri Grdfs.genet_version in
  let node_filetype = Uri Grdfs.genet_filetype in
  let node_seq = Uri (Grdfs.rdf_"Seq") in
  let props = [
      (Grdfs.genet_name, "has name", "Name", None, Some node_literal) ;
      (Grdfs.genet_hasbranch, "has branch", "Has a branch", None, Some node_branch) ;
      (Grdfs.genet_nointf, "has no interface", "Does not implement the interface", None, Some node_intf) ;
      (Grdfs.genet_hasintf, "has interface", "Implement the interface", None, Some node_intf) ;
      (Grdfs.genet_haspath, "has path", "The path of the command", Some node_intf, Some node_literal) ;
      (Grdfs.genet_hasdiffcom, "has diff command", "The path of the command to compute diffs",
       Some node_filetype, Some node_literal) ;
      (Grdfs.genet_hasversion, "has version", "Has version", None, Some node_version) ;
      (Grdfs.genet_consumes, "consumes", "Filetypes consumed, ordered", Some node_intf, Some node_seq);
      (Grdfs.genet_produces, "produces", "Filetypes produced, ordered", Some node_intf, Some node_seq);
    ]
  in
  let definedby = Uri Grdfs.genet in
  List.iter
  (fun (uri, label, comment, domain, range) ->
     add_property wld
     ~label ~comment ~definedby ?domain ?range
     (Uri uri)
  )
  props
;;

let output_format = ref "rdfxml";;

let options =
  Options.option_version "Genet-genrdfs"::
  ("--ntriples", Cmdline.Unit (fun () -> output_format := "ntriples"), " output in ntriples format") ::
  ("--turtle", Cmdline.Unit (fun () -> output_format := "turtle"), " output in turtle format") ::
  []
;;

let main () =
  let _options = Options.parse options in
  failwith "not implemented yet"
(*
  let storage = Rdf_storage.new_storage world ~factory: "memory" ~name: "test" in
  let model = Rdf_model.new_model world storage in
  add_vocabulary wld;
  match Rdf_model.to_string model ~name: !output_format with
    None -> failwith "Failed to serialize model"
  | Some string -> print_string string
  *)
;;

let _ = Misc.safe_main main;;