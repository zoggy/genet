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

open Rdf_term;;

let add_vocabulary g =
  let cl = Rdf_rdfs.class_ in
  let lit = Rdf_rdfs.rdfs_Literal in

  cl g ~label: "Element" Grdfs.genet_element ;

  List.iter
    (fun (label, iri) ->
       cl g ~label ~subof: Grdfs.genet_element iri)
    [
      "Interface", Grdfs.genet_intf ;
      "Port", Grdfs.genet_port ;
      "Version", Grdfs.genet_version ;
      "Filetype", Grdfs.genet_filetype ;
      "Chain", Grdfs.genet_chain ;
      "Flat chain", Grdfs.genet_flatchain ;
      "Instanciated chain", Grdfs.genet_instchain ;
      "Instanciated operation", Grdfs.genet_instopn ;
      "Flattened operation", Grdfs.genet_flatopn ;
      "Diff command", Grdfs.genet_diffcommand ;
    ] ;

  cl g ~label: "Branch" ~subof: Grdfs.genet_version Grdfs.genet_branch ;
  cl g ~label: "Tool" ~subof: Grdfs.genet_branch Grdfs.genet_tool ;
  cl g ~label: "Explode operation" ~subof: Grdfs.genet_flatopn Grdfs.genet_explode ;
  cl g ~label: "Implode operation" ~subof: Grdfs.genet_flatopn Grdfs.genet_implode ;

  List.iter
    (fun (iri, label, domain, range) ->
       Rdf_rdfs.property g ~label ~domains: [domain] ~ranges: [range] iri
    )
    [
      Grdfs.genet_name, "Name", Grdfs.genet_element, lit ;
      Grdfs.genet_desc, "Description", Grdfs.genet_element, lit ;
      Grdfs.genet_file_ext, "Filename extension", Grdfs.genet_filetype, lit ;
      Grdfs.genet_hasbranch, "Has branch", Grdfs.genet_branch, Grdfs.genet_branch ;
      Grdfs.genet_nointf, "Does not implement interface", Grdfs.genet_branch, Grdfs.genet_intf ;
      Grdfs.genet_haspath, "Has command path", Grdfs.genet_intf, lit ;
      Grdfs.genet_usetool, "Depend on additional tool", Grdfs.genet_intf, Grdfs.genet_tool ;
      Grdfs.genet_hasversion, "Has version", Grdfs.genet_branch, Grdfs.genet_version ;
      Grdfs.genet_consumes, "Use input port", Grdfs.genet_intf, Grdfs.genet_port ;
      Grdfs.genet_produces, "Output on port", Grdfs.genet_intf, Grdfs.genet_port ;
      Grdfs.genet_hasdiffcom, "Has diff command", Grdfs.genet_filetype, lit ;
      Grdfs.genet_hasintf, "Implement interface", Grdfs.genet_version, Grdfs.genet_intf ;
      Grdfs.genet_hastype, "Has type", Grdfs.genet_port, lit ;
      Grdfs.genet_flattenedto, "Flattened to", Grdfs.genet_chain, Grdfs.genet_flatchain ;
      Grdfs.genet_opfrom, "Operation comes from", Grdfs.genet_instopn, Grdfs.genet_flatopn ;
      Grdfs.genet_portfrom, "Port comes from", Grdfs.genet_port, Grdfs.genet_port ;
      Grdfs.genet_containsop, "Contains operation", Grdfs.genet_flatchain, Grdfs.genet_flatopn ;
      Grdfs.genet_createdon, "Created on", Grdfs.genet_element, lit ;
      Grdfs.genet_startedon, "Started on", Grdfs.genet_element, lit ;
      Grdfs.genet_stoppedon, "Stopped on", Grdfs.genet_element, lit ;
      Grdfs.genet_isactive, "Is active", Grdfs.genet_version, lit ;
      Grdfs.genet_instanciate, "Instanciate", Grdfs.genet_instchain, Grdfs.genet_flatchain ;
      Grdfs.genet_useinput, "Use input", Grdfs.genet_instchain, lit ;
      Grdfs.genet_useinputcommitid, "Use input commit id", Grdfs.genet_instchain, lit ;
      Grdfs.genet_useversion, "Use version", Grdfs.genet_flatchain, lit ;
      Grdfs.genet_usetoolversion, "Use tool version", Grdfs.genet_instchain, Grdfs.genet_version ;
      Grdfs.genet_filemd5, "File md5checksum", Grdfs.genet_element, lit ;
      Grdfs.genet_hasimplode, "Corresponding implode operation", Grdfs.genet_explode, Grdfs.genet_implode ;
      Grdfs.genet_returncode, "Return code", Grdfs.genet_instopn, lit ;
      Grdfs.genet_commandoutput, "Command output", Grdfs.genet_instopn, lit ;
      Grdfs.genet_failedcommand, "Failed command", Grdfs.genet_instchain, Grdfs.genet_instopn ;
      Grdfs.genet_refinstfor, "Reference instanciation for", Grdfs.genet_instchain, Rdf_rdfs.rdfs_Resource ;
    ]
;;

let to_string = ref Rdf_xml.to_string;;

let options =
  Options.option_version "Genet-genrdfs"::
    ("--turtle", Cmdline.Unit (fun () -> to_string := Rdf_ttl.to_string), " output in turtle format") ::
    []
;;

let main () =
  let _options = Options.parse options in
  let g = Rdf_graph.open_graph (Rdf_iri.iri "http://foo.net") in
  add_vocabulary g;
  Rdf_rdfs.add_namespaces g;
  g.Rdf_graph.add_namespace (Grdfs.genet_"") "genet";
  g.Rdf_graph.add_namespace Rdf_rdf.dc "dc";
  print_endline (!to_string g)
;;

let _ = Misc.safe_main main;;