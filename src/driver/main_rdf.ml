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

(** RDF import/export for the command line tool. *)

open Cmdline;;

let verbose opts ?(level=1) msg =
  if opts.Options.verb_level >= level then
    prerr_endline msg
;;

let export_rdf config rdf_wld opts =
  let s = Rdf_xml.to_string
    ~namespaces: ((config.Config.rest_api, "project") :: Grdfs.namespaces)
      rdf_wld.Grdf_types.wld_graph
  in
  print_string s
;;

let com_export_rdf = {
    com_options = [] ;
    com_usage = "" ;
    com_compl = [] ;
    com_kind = Main_cmd.mk_final_fun export_rdf;
  }
;;

let import_rdf config rdf_wld opts =
  let f_import file =
    verbose opts (Printf.sprintf "Import file %S..." file);
    let base =
      let iri = Printf.sprintf "file://%s"
        (Filename.concat (Sys.getcwd()) file)
      in
      Rdf_iri.iri iri
    in
    Rdf_xml.from_file rdf_wld.Grdf_types.wld_graph ~base file;
    verbose opts " ok"
  in
  List.iter f_import opts.Options.args
;;

let com_import_rdf = {
    com_options = [] ;
    com_usage = "<rdf files>" ;
    com_compl = [ Cmdline.Compfun Main_cmd.compl_file ] ;
    com_kind = Main_cmd.mk_final_fun import_rdf;
  }
;;

Main_cmd.register_subcommand "export-rdf" com_export_rdf "export RDF graph";;
Main_cmd.register_subcommand "import-rdf" com_import_rdf "import RDF graph";;
