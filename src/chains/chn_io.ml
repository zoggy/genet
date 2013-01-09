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

(** Reading and writing chain ASTs. *)

let modname_of_file f =
  Chn_types.chain_modname_of_string
  (String.capitalize (Filename.basename (Filename.chop_extension f)))
;;

let file_of_modname config name =
  let dir = Config.chains_dir config in
  let name = String.uncapitalize (Chn_types.string_of_chain_modname name) in
  Filename.concat dir (Printf.sprintf "%s.gnt" name)
;;

let input_chn_module name lexbuf =
  try
    let l = Chn_parser.ast Lexer.main lexbuf in
    { Chn_ast.cmod_name = name ;
      Chn_ast.cmod_chains = l ;
    }
  with
    Chn_parser.Error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      let loc = { Loc.loc_start = pos ; Loc.loc_end = pos } in
      Loc.raise_problem loc "Syntax error"
;;

let chn_module_of_file file =
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  let lexbuf =
    { lexbuf with
      Lexing.lex_curr_p = { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = file }
    }
  in
  let modname = modname_of_file file in
  Misc.try_finalize (input_chn_module modname) lexbuf close_in ic
;;

let print_chn_module oc m =
  output_string oc ((Chn_ast.printer ())#string_of_chn_module m)
;;

let chain_files config =
  let dir = Config.chains_dir config in
  Find.find_list Find.Stderr [dir]
  [ Find.Maxdepth 1 ; Find.Type Unix.S_REG ;
    Find.Regexp (Str.regexp ".*\\.gnt$") ]
;;

let load_chain_files config =
  let f (cmods, errors) file =
    try
      let cmod = chn_module_of_file file in
      (cmod :: cmods, errors)
    with
      e ->
        let msg =
          match e with
            Sys_error s | Failure s -> s
          | Loc.Problem pb -> Loc.string_of_problem pb
          | _ -> raise e
        in
        (cmods, msg :: errors)
  in
  List.fold_left f ([], []) (chain_files config)
;;

module File_set = Set.Make
  (struct
     type t = string * Misc.git_status
     let compare (f1,_) (f2,_) = Pervasives.compare f1 f2
   end)
;;
type chain_file_dep =
  {
   dep_chains : Chn_ast.Cset.t ; (** all chains it (recursively) depends on *)
   dep_files : File_set.t ; (** all files it (recursively) depends on *)
  }

type chain_file_deps = chain_file_dep Chn_ast.Cmap.t;;

let get_chain_deps_files ctx chain_name =
  (* we assume that there is no cycle in deps *)
  let config = ctx.Chn_types.ctx_cfg in
  let (cmods, errors) = load_chain_files config in
  let deps = Chn_ast.compute_deps ctx.Chn_types.ctx_rdf config cmods in
  let g = Chn_ast.compute_dep_graph deps in

  let gather_deps map (chains,files) (j,_) =
    let d = Chn_ast.Cmap.find j map in
    (Chn_ast.Cset.union d.dep_chains chains, File_set.union d.dep_files files)
  in
  let rec f map chn_name =
    try
      ignore(Chn_ast.Cmap.find chn_name map);
      (* deps already computed for this chain; return same map *)
      map
    with
      Not_found ->
        (* gather deps of successors and add associate to chn_name *)
        let succs = Chn_ast.Chn_graph.succ g chn_name in
        let map = List.fold_left (fun map (succ,_) -> f map succ) map succs in
        let file = file_of_modname ctx.Chn_types.ctx_cfg (Chn_types.chain_modname chn_name) in
        let git_status = Misc.git_status file in
        let (chains, files) = List.fold_left (gather_deps map)
          (Chn_ast.Cset.empty, File_set.singleton (file, git_status)) succs
        in
        let dep = { dep_chains = chains ; dep_files = files } in
        Chn_ast.Cmap.add chn_name dep map
  in
  f Chn_ast.Cmap.empty chain_name
;;
  