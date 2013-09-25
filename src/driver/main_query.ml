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

module Iriset = Rdf_iri.Iriset

let priri_endline iri = print_endline (Rdf_iri.string iri);;

let list_tools _ wld _ =
  let tools = Grdf_tool.tools wld in
  List.iter priri_endline tools
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
      [] -> List.map (fun b -> b.Grdf_branch.bch_iri) (Grdf_branch.branches wld)
    | l ->
        let add set elt = Iriset.add elt set in
        let f set s_iri =
          List.fold_left add set (Grdf_branch.subs wld (Rdf_iri.iri s_iri))
        in
        let set = List.fold_left f Iriset.empty l in
        Iriset.elements set
  in
  List.iter priri_endline branches
;;
let com_branches = {
  Cmdline.com_options = [] ;
  com_usage = "[anscestor iris]" ;
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
        let add set elt = Iriset.add elt set in
        let f set s_iri =
          List.fold_left add set
          (Grdf_version.versions_of ~recur: true wld (Rdf_iri.iri s_iri))
        in
        let set = List.fold_left f Iriset.empty l in
        Iriset.elements set
  in
  List.iter priri_endline versions
;;
let com_versions =
  { Cmdline.com_options = [] ;
    com_usage = "[<iri1>] [<iri2> ...]" ;
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
        let f set s_iri =
          let (explicit, explicit_no, inherited) =
            Grdf_intf.compute_intfs_of wld (Rdf_iri.iri s_iri)
          in
          Iriset.diff (Iriset.union set (Iriset.union explicit inherited)) explicit_no
        in
        List.fold_left f Iriset.empty l
  in
  Iriset.iter (fun iri -> print_endline (Grdf_intf.string_of_intf wld iri)) intfs
;;
let com_intfs =
  { Cmdline.com_options = [] ;
    com_usage = "[<iri1>] [<iri2> ...]" ;
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

let list_chains config wld _ =
  let (cmods,_) = Chn_io.load_chain_files config in
  let f_mod modname acc chn =
    let n = Chn_types.mk_chain_name modname chn.Chn_ast.chn_name in
    (Chn_types.string_of_chain_name n) :: acc
  in
  let f acc cmod =
     List.fold_left (f_mod cmod.Chn_ast.cmod_name)
      acc cmod.Chn_ast.cmod_chains
  in
  List.iter print_endline (List.fold_left f [] cmods)
;;
let com_chains =
  { Cmdline.com_options = [] ;
    com_usage = "" ;
    com_compl = [] ;
    com_kind = Main_cmd.mk_final_fun list_chains ;
  }
;;
let com_chains = ("chains", com_chains, "list chains");;

let list_inputs config _ _ =
  List.iter (fun i -> print_endline (Fname.rel_string i)) (Ind_io.list_inputs config)
;;
let com_inputs =
  { Cmdline.com_options = [] ;
    com_usage = "" ;
    com_compl = [] ;
    com_kind = Main_cmd.mk_final_fun list_inputs ;
  }
;;
let com_inputs = ("inputs", com_inputs, "list inputs");;

let list_fchains _ wld _ =
  let l = Chn_flat.flat_chains wld in
  List.iter priri_endline l
;;
let com_fchains =
  { Cmdline.com_options = [] ;
    com_usage = "" ;
    com_compl = [] ;
    com_kind = Main_cmd.mk_final_fun list_fchains ;
  }
;;
let com_fchains = ("fchains", com_fchains, "list flat chains");;

let list_ichains _ wld _ =
  let l = Chn_inst.inst_chains wld in
  List.iter priri_endline l
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
      failwith "Please give only one iri"
  | [s] ->
      let prefix = config.Config.rest_api in
      let out_pref = Grdfs.iri_outfile_path prefix [] in
      let out_pref_raw = Grdfs.iri_outfile_path ~raw: true prefix [] in
      let in_pref = Grdfs.iri_input_path prefix [] in
      let in_pref_raw = Grdfs.iri_input_path ~raw: true prefix  [] in
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
            let path = Misc.path_under ~parent: (Rdf_iri.string prefix) s in
            Fname.concat_s dir path
          with _ -> f q
      in
      try print_endline (Fname.abs_string (f prefixes))
      with
        Not_found ->
          failwith (Printf.sprintf "%S does not correspond to an input or output file url" s)
;;
let com_filename =
  { Cmdline.com_options = [] ;
    com_usage = "<iri>" ;
    com_compl = [ Cmdline.Compfun Main_cmd.compl_file_iri ] ;
    com_kind = Main_cmd.mk_final_fun print_filename_of_url ;
  }
;;
let com_filename =
  ("filename", com_filename, "print the filename corresponding to the given file iri")
;;

let ref_inst_of_inst ctx opts =
  match opts.Options.args with
  | [] | _ :: _ :: _ ->
    failwith "Please give one and only one inst chain iri"
  | [s] ->
      let iri = Rdf_iri.iri s in
      match Chn_inst.reference_inst_of_inst ctx iri with
        None -> ()
      | Some iri -> print_endline (Rdf_iri.string iri)
;;
let com_ref_inst_of_inst =
  { Cmdline.com_options = [] ;
    com_usage = "<iri>" ;
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
      let input = Fname.relative input in
      let chain_name = Chn_types.chain_name_of_string chain_name in
      let chain = Chn_types.iri_chain ctx.Chn_types.ctx_cfg.Config.rest_api chain_name in
      let l = Chn_inst.reference_insts ctx ~input ~chain in
      List.iter (fun iri -> print_endline (Rdf_iri.string iri)) l
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

let list_diffcommands _ wld _ =
  Iriset.iter
    (fun iri ->
       let path = Misc.string_of_opt (Grdf_diff.command_path wld iri) in
       print_endline (Printf.sprintf "%s : %s" (Rdf_iri.string iri) path)
    )
    (Grdf_diff.diffcommands wld)
;;
let com_diffcommands =
  { Cmdline.com_options = [] ;
    com_usage = "" ;
    com_compl = [] ;
    com_kind = Main_cmd.mk_final_fun list_diffcommands ;
  }
;;
let com_diffcommands = ("diff-commands", com_diffcommands, "list predefined diff commands");;

let command =
  { Cmdline.com_options = [] ;
    com_usage = "" ;
    com_compl = [] ;
    com_kind = Cmdline.Commands
      [
        com_tools ;
        com_branches ;
        com_chains ;
        com_versions ;
        com_intfs ;
        com_ftypes ;
        com_fchains ;
        com_ichains ;
        com_inputs ;
        com_dot ;
        com_filename ;
        com_ref_inst ;
        com_ref_inst_of_inst ;
        com_diffcommands ;
      ];
  }
;;

Main_cmd.register_subcommand "query" command "query information";;

(*

let options =
    ("--inst", Arg.String (fun s -> mode := Some (Inst s)),
     "<url> look for other executions 'almost' like the given one") ;

let lookup_insts ctx s =
  let _iri_inst = Rdf_iri.iri s in
  []
;;

let lookup_and_print_insts ctx s =
  let insts = lookup_insts ctx s in
  List.iter
    (fun iri -> print_endline (Rdf_iri.string iri))
    insts;
  let dot = Chn_lookup.make_graph ctx (Rdf_iri.iri s) in
  let svg = Grdf_dot.dot_to_svg ~program: "circo" dot in
  print_endline (Xtmpl.string_of_xmls svg)
;;

*)