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

(** Main module of the [genet-chain] tool. *)

open Chn_ast;;

let gen_chain_dot file chain =
  let dot = (new Chn_ast.chain_dot_printer)#dot_of_chain chain in
  Misc.file_of_string ~file: file dot
;;

let gen_fchain_dot ctx file fchain =
  let dot = (new Chn_flat.fchain_dot_printer)#dot_of_fchain ctx fchain in
  Misc.file_of_string ~file: file dot
;;



let test_file ?dot file =
  try
    let cmod = Chn_io.chn_module_of_file file in
    Chn_io.print_chn_module stdout cmod;
    begin
      match dot with
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
    end;
    true
  with
    Loc.Problem pb ->
      prerr_endline (Loc.string_of_problem pb);
      false
;;

let flatten opts ?dot acc s =
  try
    let config = Config.read_config opts.Options.config_file in
    let rdf_wld = Grdf_init.open_graph config in
    let ctx = { Chn_types.ctx_rdf = rdf_wld ; ctx_cfg = config ; ctx_user = None } in
    let fullname = Chn_types.chain_name_of_string s in
    let uri = Chn_flat.flatten ctx fullname in
    print_endline (Printf.sprintf "%s => %s" s (Rdf_uri.string uri));
    begin match dot with None -> () | Some file -> gen_fchain_dot ctx file uri end;
    acc
  with
    e ->
      let msg =
        match e with
          Failure s | Sys_error s ->  s
        | Loc.Problem pb -> Loc.string_of_problem pb
        | e -> Printexc.to_string e
      in
      let msg = Printf.sprintf "flatten %s: %s" s msg in
      prerr_endline msg;
      acc + 1
;;

let flatten_all opts acc =
  try
    let config = Config.read_config opts.Options.config_file in
    let (chain_mods, _) = Chn_io.load_chain_files config in
    let rdf_wld = Grdf_init.open_graph config in
    let ctx = { Chn_types.ctx_rdf = rdf_wld ; ctx_cfg = config ; ctx_user = None } in
    let f_chain mod_name  acc chain =
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
            (Chn_types.string_of_chain_name fullname) msg in
          prerr_endline msg;
          acc + 1
    in
    let f_mod acc cmod =
      List.fold_left (f_chain cmod.cmod_name) acc cmod.cmod_chains
    in
    List.fold_left f_mod acc chain_mods
  with
    e ->
      let msg =
        match e with
          Failure s | Sys_error s ->  s
        | Loc.Problem pb -> Loc.string_of_problem pb
        | e -> Printexc.to_string e
      in
      let msg = Printf.sprintf "flatten-all: %s" msg in
      prerr_endline msg;
      acc + 1
;;

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

let show_combinations opts acc s =
  let uri = Rdf_uri.uri s in
  let config = Config.read_config opts.Options.config_file in
  let rdf_wld = Grdf_init.open_graph config in
  let ctx = { Chn_types.ctx_rdf = rdf_wld ; ctx_cfg = config ; ctx_user = None } in
  let combs = Chn_inst.version_combinations ctx uri in
  print_endline (Printf.sprintf "combinations for %s:" (Rdf_uri.string uri));
  let f_comb comb = print_endline (string_of_comb ctx comb) in
  List.iter f_comb combs;
  acc
;;

let dot = ref None;;
type action =
  | Test of string
  | Flatten of string
  | Flatten_all
  | Show_combs of string
;;
let actions = ref [];;

let do_action opts acc = function
  Test file -> if test_file ?dot: !dot file then acc else acc + 1
| Flatten s -> flatten opts ?dot: !dot acc s
| Flatten_all -> flatten_all opts acc
| Show_combs s -> show_combinations opts acc s
;;

let options =
  Options.option_config ::
  [
    "--dot", Arg.String (fun file -> dot := Some file), " generate dot files" ;
    "-t", Arg.String (fun s -> actions := Test s :: !actions), "<file> test the given file" ;
    "-f", Arg.String (fun s -> actions := Flatten s :: !actions), "<Mod.chain> flatten the given chain" ;
    "--flatten-all", Arg.Unit (fun s -> actions := Flatten_all :: !actions), "flatten all the chains" ;

    "--show-combs", Arg.String (fun s -> actions := Show_combs s :: !actions),
    "<flat chain uri> show tool combinations usable to instanciate this chain";
  ]
;;


let main () =
  let opts = Options.parse options in
  let n = List.fold_left (do_action opts) 0 (List.rev !actions) in
  flush stdout;
  if n > 0 then
    prerr_endline (Printf.sprintf "%d problem(s) encountered" n);
  exit n
;;

let () = Misc.safe_main main;;
