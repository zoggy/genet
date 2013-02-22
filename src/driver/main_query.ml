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
          Uriset.union set
          (Grdf_intf.intfs_of ~recur: true wld (Rdf_uri.uri s_uri))
        in
        List.fold_left f Uriset.empty l
  in
  Uriset.iter (fun uri -> print_endline (Grdf_intf.string_of_intf wld uri)) intfs
;;
let com_intfs =
  { Cmdline.com_options = [] ;
    com_usage = "[<uri1>] [<uri2> ...]" ;
    com_kind = Main_cmd.mk_final_fun list_interfaces ;
  }
;;
let com_intfs = ("interfaces", com_intfs, "list interfaces");;


let list_filetypes _ wld _ =
  let l = Grdf_ftype.filetypes wld in
  List.iter (fun ft -> prerr_endline (Grdf_ftype.string_of_filetype wld ft)) l
;;
let com_ftypes =
  { Cmdline.com_options = [] ;
    com_usage = "" ;
    com_kind = Main_cmd.mk_final_fun list_filetypes ;
  }
;;
let com_ftypes = ("filetypes", com_ftypes, "list filetypes");;

let dot _ wld _ = print_endline (Grdf_dot.dot wld);;
let com_dot =
  { Cmdline.com_options = [] ;
    com_usage = "" ;
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
    com_usage = "<input name> <chain uri>" ;
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
    com_kind = Cmdline.Commands
      [
        com_tools ;
        com_branches ;
        com_versions ;
        com_intfs ;
        com_ftypes ;
        com_dot ;
        com_filename ;
        com_ref_inst ;
        com_ref_inst_of_inst ;
      ];
  }
;;

Main_cmd.register_subcommand "query" command "query information";;

(*

type mode =
  | Tools
  | Branches
  | Versions
  | Interfaces
  | Filetypes
  | Dot
  | File of string
  | Inst of string
  | Ref_inst_of_inst of string
  | Ref_inst of string * string (* input * chain name *)

let mode = ref None;;

let set_ref_inst s =
  match Misc.split_string s [','] with
    [s1 ; s2 ] -> mode := Some (Ref_inst (s1, s2))
  | _ ->
    let msg = "option --ref-inst needs an argument of form <input name>,<chain fullname>" in
    failwith msg
;;

let options =
  [
    ("--tools", Arg.Unit (fun () -> mode := Some Tools), " print tools") ;

    ("--branches", Arg.Unit (fun () -> mode := Some Branches),
     " print branches (all or only the ones of the given parents)") ;

    ("--versions", Arg.Unit (fun () -> mode := Some Versions),
     " print versions (all or only the ones of the given tools or branches)") ;

    ("--interfaces", Arg.Unit (fun () -> mode := Some Interfaces),
     " print interfaces (all or only the ones of the given tools or branches)") ;

    ("--filetypes", Arg.Unit (fun () -> mode := Some Filetypes),
     " print filetypes") ;

    ("--file", Arg.String (fun s -> mode := Some (File s)),
     "<url> print the filename corresponding to the given file url") ;

    ("--inst", Arg.String (fun s -> mode := Some (Inst s)),
     "<url> look for other executions 'almost' like the given one") ;

    ("--dot", Arg.Unit (fun () -> mode := Some Dot),
     " print graph in graphviz format") ;

    ("--ref-inst-of-inst", Arg.String (fun s -> mode := Some (Ref_inst_of_inst s)),
     "<url> print reference inst chain, if any, from given inst chain" ) ;

    ("--ref-inst", Arg.String set_ref_inst,
     "<url> print reference inst chain, if any, from given inst chain" ) ;
  ]
;;







let l

l



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






let query config rdf_wld opts =
  let ctx = { Chn_types.ctx_rdf = rdf_wld ; ctx_cfg = config ; ctx_user = None} in
  begin
    match !mode with
      None -> ()
    | Some Tools -> list_tools rdf_wld
    | Some Branches -> list_branches rdf_wld opts
    | Some Versions -> list_versions rdf_wld opts
    | Some Interfaces -> list_interfaces rdf_wld opts
    | Some Filetypes -> list_filetypes rdf_wld
    | Some Dot -> dot rdf_wld
    | Some (File s) -> print_filename_of_url config s
    | Some (Inst s) -> lookup_and_print_insts ctx s
    | Some (Ref_inst_of_inst s) -> ref_inst_of_inst ctx s
    | Some (Ref_inst (input, chain_name)) -> ref_inst ctx input chain_name
  end
;;
*)