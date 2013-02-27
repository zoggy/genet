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

(** Main module of the genet-query program.*)

open Grdf_tool;;

let pruri_endline uri = print_endline (Rdf_uri.string uri);;

let list_tools _ wld _ =
  let tools = Grdf_tool.tools wld in
  List.iter pruri_endline tools
;;
let com_tools = {
  Cmdline.com_options = [] ;
  com_usage = "" ;
  com_compl = [] ;
  com_kind = Main_cmd.mk_final_fun list_tools ;
  }
;;
let com_tools = ("tools", com_tools, "print tools") ;;

let list_branches _ wld options =
  let branches =
    match options.Options.args with
      [] -> List.map (fun b -> b.Grdf_branch.bch_uri) (Grdf_branch.branches wld)
    | l ->
        let add set elt = Uriset.add elt set in
        let f set s_uri =
          List.fold_left add set (Grdf_branch.subs wld (Rdf_uri.uri s_uri))
        in
        let set = List.fold_left f Uriset.empty l in
        Uriset.elements set
  in
  List.iter pruri_endline branches
;;
let com_branches = {
  Cmdline.com_options = [] ;
  com_usage = "[anscestor uris]" ;
  com_compl = [Cmdline.Complist Main_cmd.compl_tool_or_branch] ;
  com_kind = Main_cmd.mk_final_fun list_branches ;
  }
;;
let com_branches = ("branches", com_branches, "list branches");;

let list_versions _ wld options =
  let versions =
    match options.Options.args with
      [] -> Grdf_version.versions wld
    | l ->
        let add set elt = Uriset.add elt set in
        let f set s_uri =
          List.fold_left add set
          (Grdf_version.versions_of ~recur: true wld (Rdf_uri.uri s_uri))
        in
        let set = List.fold_left f Uriset.empty l in
        Uriset.elements set
  in
  List.iter pruri_endline versions
;;
let com_versions =
  { Cmdline.com_options = [] ;
    com_usage = "[<uri1>] [<uri2> ...]" ;
    com_compl = [ Cmdline.Complist Main_cmd.compl_tool_or_branch ] ;
    com_kind = Main_cmd.mk_final_fun list_versions ;
  }
;;
let com_versions = ("versions", com_versions, "list versions");;

let list_interfaces _ wld options =
  let intfs =
    match options.Options.args with
      [] -> Grdf_intf.intfs wld
    | l ->
        let f set s_uri =
          let (explicit, inherited) =
            Grdf_intf.compute_intfs_of wld (Rdf_uri.uri s_uri)
          in
          Uriset.union set (Uriset.union explicit inherited)
        in
        List.fold_left f Uriset.empty l
  in
  Uriset.iter (fun uri -> print_endline (Grdf_intf.string_of_intf wld uri)) intfs
;;
let com_intfs =
  { Cmdline.com_options = [] ;
    com_usage = "[<uri1>] [<uri2> ...]" ;
    com_compl = [ Cmdline.Complist Main_cmd.compl_intf_provider ] ;
    com_kind = Main_cmd.mk_final_fun list_interfaces ;
  }
;;
let com_intfs = ("interfaces", com_intfs, "list interfaces");;


let list_filetypes _ wld _ =
  let l = Grdf_ftype.filetypes wld in
  List.iter (fun ft -> print_endline (Grdf_ftype.string_of_filetype wld ft)) l
;;
let com_ftypes =
  { Cmdline.com_options = [] ;
    com_usage = "" ;
    com_compl = [] ;
    com_kind = Main_cmd.mk_final_fun list_filetypes ;
  }
;;
let com_ftypes = ("filetypes", com_ftypes, "list filetypes");;

let list_ichains _ wld _ =
  let l = Chn_inst.inst_chains wld in
  List.iter pruri_endline l
;;
let com_ichains =
  { Cmdline.com_options = [] ;
    com_usage = "" ;
    com_compl = [] ;
    com_kind = Main_cmd.mk_final_fun list_ichains ;
  }
;;
let com_ichains = ("ichains", com_ichains, "list inst chains");;

let dot _ wld _ = print_endline (Grdf_dot.dot wld);;
let com_dot =
  { Cmdline.com_options = [] ;
    com_usage = "" ;
    com_compl = [] ;
    com_kind = Main_cmd.mk_final_fun dot ;
  }
;;
let com_dot = ("dot", com_dot, "output graph in graphviz format");;

let print_filename_of_url config wld opts =
  match opts.Options.args with
  | [] | _ :: _ :: _ ->
      failwith "Please give only one uri"
  | [s] ->
      let prefix = config.Config.rest_api in
      let out_pref = Grdfs.uri_outfile_path prefix [] in
      let out_pref_raw = Grdfs.uri_outfile_path ~raw: true prefix [] in
      let in_pref = Grdfs.uri_input_path prefix [] in
      let in_pref_raw = Grdfs.uri_input_path ~raw: true prefix  [] in
      let out_dir = Config.out_dir config in
      let in_dir = Config.data_dir config in
      let prefixes = (* order matters as raw prefixes are prefix of "not raw" *)
        [ out_pref_raw, out_dir ; out_pref, out_dir ;
          in_pref_raw, in_dir ; in_pref, in_dir ;
        ]
      in
      let rec f = function
        [] -> raise Not_found
      | (prefix, dir) :: q ->
          try
            let path = Misc.path_under ~parent: (Rdf_uri.string prefix) s in
            Filename.concat dir path
          with _ -> f q
      in
      try print_endline (f prefixes)
      with
        Not_found ->
          failwith (Printf.sprintf "%S does not correspond to an input or output file url" s)
;;
let com_filename =
  { Cmdline.com_options = [] ;
    com_usage = "<uri>" ;
    com_compl = [ Cmdline.Compfun Main_cmd.compl_file_uri ] ;
    com_kind = Main_cmd.mk_final_fun print_filename_of_url ;
  }
;;
let com_filename =
  ("filename", com_filename, "print the filename corresponding to the given file uri")
;;

let ref_inst_of_inst ctx opts =
  match opts.Options.args with
  | [] | _ :: _ :: _ ->
    failwith "Please give one and only one inst chain uri"
  | [s] ->
      let uri = Rdf_uri.uri s in
      match Chn_inst.reference_inst_of_inst ctx uri with
        None -> ()
      | Some uri -> print_endline (Rdf_uri.string uri)
;;
let com_ref_inst_of_inst =
  { Cmdline.com_options = [] ;
    com_usage = "<uri>" ;
    com_compl = [ Cmdline.Compfun Main_cmd.compl_ichain ] ;
    com_kind = Main_cmd.mk_final_fun (Main_cmd.mk_ctx_fun ref_inst_of_inst) ;
  }
;;
let com_ref_inst_of_inst =
  ("ref-inst-of-inst", com_ref_inst_of_inst,
   "print reference inst chain, if any, from given inst chain")
;;

let ref_inst ctx opts =
  match opts.Options.args with
  | [input ; chain_name] ->
      let chain_name = Chn_types.chain_name_of_string chain_name in
      let chain = Chn_types.uri_chain ctx.Chn_types.ctx_cfg.Config.rest_api chain_name in
      let l = Chn_inst.reference_insts ctx ~input ~chain in
      List.iter (fun uri -> print_endline (Rdf_uri.string uri)) l
  | _ ->
      failwith "Please give input name and chain fullname, no less, no more"
;;

let com_ref_inst =
  { Cmdline.com_options = [] ;
    com_usage = "<input name> <chain name>" ;
    com_compl = [
      Cmdline.Compfun Main_cmd.compl_input_name ;
      Cmdline.Compfun Main_cmd.compl_chain_name ;
    ] ;
    com_kind = Main_cmd.mk_final_fun (Main_cmd.mk_ctx_fun ref_inst) ;
  }
;;
let com_ref_inst =
  ("ref-inst", com_ref_inst,
   "print reference inst chain, if any, from given input name and chain fullname")
;;

let command =
  { Cmdline.com_options = [] ;
    com_usage = "" ;
    com_compl = [] ;
    com_kind = Cmdline.Commands
      [
        com_tools ;
        com_branches ;
        com_versions ;
        com_intfs ;
        com_ftypes ;
        com_ichains ;
        com_dot ;
        com_filename ;
        com_ref_inst ;
        com_ref_inst_of_inst ;
      ];
  }
;;

Main_cmd.register_subcommand "query" command "query information";;

(*

let options =
    ("--inst", Arg.String (fun s -> mode := Some (Inst s)),
     "<url> look for other executions 'almost' like the given one") ;

let lookup_insts ctx s =
  let _uri_inst = Rdf_uri.uri s in
  []
;;

let lookup_and_print_insts ctx s =
  let insts = lookup_insts ctx s in
  List.iter
    (fun uri -> print_endline (Rdf_uri.string uri))
    insts;
  let dot = Chn_lookup.make_graph ctx (Rdf_uri.uri s) in
  let svg = Grdf_dot.dot_to_svg ~program: "circo" dot in
  print_endline (Xtmpl.string_of_xmls svg)
;;

*)