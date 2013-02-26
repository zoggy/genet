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

(** Execution of chains from command line tool. *)

open Chn_types;;
open Cmdline;;

let verbose opts ?(level=1) msg =
  if opts.Options.verb_level >= level then
    prerr_endline msg
;;

let exec_one opts reporter input =
  try
    let config = Config.read_config opts.Options.config_file in
    let rdf_wld = Grdf_init.open_graph config in
    let ctx = { Chn_types.ctx_rdf = rdf_wld ; ctx_cfg = config ; ctx_user = None } in
    let spec_dir = Filename.concat (Config.data_dir config) input in
    let spec = Ind_io.load config spec_dir in
    Chn_exec.exec ctx reporter spec
  with
    Assert_failure _ | Not_found as exc -> raise exc
  | exc ->
      let msg =
        match exc with
          Ind_io.Error e ->
            Printf.sprintf "Input %s: %s" input
            (Ind_io.string_of_error e)
        | Failure s -> s
        | e -> Printexc.to_string e
      in
      reporter#error msg;
      reporter#incr_errors
;;

let all = ref false;;
let option_all = ("--all", Cmdline.Set all, " Execute all inputs");;

let com_exec ctx opts =
  let reporter = new Reporter.reporter opts.Options.verb_level in
  let inputs =
    if !all then
      Ind_io.list_inputs ctx.ctx_cfg
    else
      match opts.Options.args with
        [] -> failwith "Please give the name of at least one input or --all"
      | l -> l
  in
  List.iter (exec_one opts reporter) inputs;
 let errors = reporter#total_errors in
  print_endline (Reporter.string_of_msg_list reporter#messages);
  if errors > 0 then
    prerr_endline
    (Printf.sprintf "%d error%s" errors (if errors > 1 then "s" else ""));
  exit errors
;;

let com_exec = {
    com_options = [ option_all ] ;
    com_usage = "[<input name1> [<input name2> ...]]" ;
    com_compl = [ Cmdline.Complist Main_cmd.compl_input_name ] ;
    com_kind = Main_cmd.mk_final_fun (Main_cmd.mk_ctx_fun com_exec) ;
  }
;;

Main_cmd.register_subcommand "exec" com_exec "execute inputs";;

