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

(** Operation on chains in [genet] tool. *)

open Chn_types;;
open Chn_ast;;
open Cmdline;;

let gendot = ref (None : string option);;
let option_dot =
  ("--dot", Cmdline.String (None, fun file -> gendot := Some file), "<prefix> generate dot file(s) using <prefix>");;

let gen_chain_dot file chain =
  let dot = (new Chn_ast.chain_dot_printer)#dot_of_chain chain in
  Misc.file_of_string ~file: file dot
;;

let gen_fchain_dot ctx file fchain =
  let dot = (new Chn_flat.fchain_dot_printer)#dot_of_fchain ctx fchain in
  Misc.file_of_string ~file: file dot
;;

let test_file file =
  try
    let cmod = Chn_io.chn_module_of_file file in
    Chn_io.print_chn_module stdout cmod;
    begin
      match !gendot with
        None -> ()
      | Some prefix ->
          let f chn =
            let outfile = Printf.sprintf "%s%s.%s.dot"
              prefix
              (String.capitalize (Filename.basename file))
              (Chn_types.string_of_chain_basename chn.chn_name)
            in
            gen_chain_dot outfile chn
          in
          List.iter f cmod.cmod_chains
    end
  with
    Loc.Problem pb ->
      failwith (Loc.string_of_problem pb)
;;

let com_test _ _ opts =
  List.iter test_file opts.Options.args;;
let com_test = {
    com_options = [ option_dot ] ;
    com_usage = "<chain file(s)>" ;
    com_kind = Main_cmd.mk_final_fun com_test ;
  };;
let com_test = ("test", com_test, "test the given chain files");;

let string_of_comb ctx comb =
  let f_version tool version acc =
    let s = Printf.sprintf "%s %s"
      (Grdf_tool.name ctx.Chn_types.ctx_rdf tool)
      (Grdf_version.name ctx.Chn_types.ctx_rdf version)
    in
    s :: acc
  in
  String.concat ", " (Urimap.fold f_version comb [])
;;

let show_combinations ctx opts =
  match opts.Options.args with
    [s] ->
      let uri = Rdf_uri.uri s in
      let combs = Chn_inst.version_combinations ctx uri in
      print_endline (Printf.sprintf "combinations for %s:" (Rdf_uri.string uri));
      let f_comb comb = print_endline (string_of_comb ctx comb) in
      List.iter f_comb combs
  | _ ->
      failwith "Please give one and only one flat chain uri"
;;
let com_showcombs = {
    com_options = [] ;
    com_usage = "<flat chain uri>" ;
    com_kind = Main_cmd.mk_final_fun (Main_cmd.mk_ctx_fun show_combinations) ;
  };;
let com_showcombs =
  ("tool-combs", com_showcombs,
   "show tool combinations usable to instanciate a flat chain")
;;


let command_chain = {
  com_options = [] ;
  com_usage = "" ;
  com_kind = Cmdline.Commands
      [
        com_test ;
        com_showcombs ;
      ]
};;

let flatten ctx opts =
  match opts.Options.args with
    [s] ->
      begin
        try let fullname = Chn_types.chain_name_of_string s in
          let uri = Chn_flat.flatten ctx fullname in
          print_endline (Printf.sprintf "%s => %s" s (Rdf_uri.string uri));
          begin
            match !gendot with
              None -> ()
            | Some file -> gen_fchain_dot ctx file uri
          end
        with
          e ->
            let msg =
              match e with
                Failure s | Sys_error s ->  s
              | Loc.Problem pb -> Loc.string_of_problem pb
              | e -> Printexc.to_string e
            in
            let msg = Printf.sprintf "flatten %s: %s" s msg in
            failwith msg
      end
  | _ ->
      failwith "Please give one and only one chain fullname"
;;

let com_flatten = {
    com_options = [ option_dot ] ;
    com_usage = "<Mod.chain>" ;
    com_kind = Main_cmd.mk_final_fun (Main_cmd.mk_ctx_fun flatten) ;
  };;

let flatten_all ctx opts =
  let (chain_mods, _) = Chn_io.load_chain_files ctx.ctx_cfg in
  let f_chain mod_name acc chain =
    let fullname = Chn_types.mk_chain_name mod_name chain.chn_name in
    try
      let uri = Chn_flat.flatten ctx fullname in
      print_endline (Printf.sprintf "%s => %s"
       (Chn_types.string_of_chain_name fullname) (Rdf_uri.string uri));
      acc
    with
      e ->
        let msg =
          match e with
            Failure s | Sys_error s ->  s
          | Loc.Problem pb -> Loc.string_of_problem pb
          | e -> Printexc.to_string e
        in
        let msg = Printf.sprintf "flatten %s: %s"
          (Chn_types.string_of_chain_name fullname) msg
        in
        msg :: acc
  in
  let f_mod acc cmod =
    List.fold_left (f_chain cmod.cmod_name) acc cmod.cmod_chains
  in
  match List.fold_left f_mod [] chain_mods with
    [] -> ()
  | errors -> failwith (String.concat "\n" errors)
;;

let com_flatten_all = {
    com_options = [ ] ;
    com_usage = "" ;
    com_kind = Main_cmd.mk_final_fun (Main_cmd.mk_ctx_fun flatten_all) ;
  };;

Main_cmd.register_subcommand "chain" command_chain "operations on chains";;
Main_cmd.register_subcommand "flatten" com_flatten "flatten a chain";;
Main_cmd.register_subcommand "flatten-all" com_flatten_all "flatten all chains";;


