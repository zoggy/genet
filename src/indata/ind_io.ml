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

(** *)

module CF = Config_file;;
open Ind_types;;

let input_basename = "spec.in";;

type error = (string * string) (** file * msg *)
exception Error of error

let error ~file msg = raise (Error (Fname.string file, msg));;

let string_of_error (file, msg) =
  Printf.sprintf "File %s: %s" file msg
;;

type input =
{ group : CF.group ;
  in_cp : string list CF.cp ;
  chains_cp : string list CF.cp ;
}

let mk_spec_group () =
  let group = new CF.group in
  let in_cp = new CF.list_cp CF.string_wrappers ~group ["in"] []
    "Input files and directories; order matters"
  in
  let chains_cp = new CF.list_cp CF.string_wrappers ~group ["chains"] []
    "The chains to apply"
  in
  { group ; in_cp ; chains_cp }
;;

let on_type_error file cp _ _ _ =
  let msg = Printf.sprintf "Bad value type for field %s"
    (String.concat "." cp#get_name)
  in
  error ~file msg
;;

let load config dir =
  let from_in_data =
    let data_dir = Config.data_dir config in
    Fname.path_under ~parent: data_dir dir
  in
  let file = Fname.concat_s dir input_basename in
  let g = mk_spec_group () in
  try
    g.group#read ~no_default: true
      ~on_type_error: (on_type_error file) (Fname.string file);
    let in_files = List.map
      (fun file -> 
        let rel = Fname.relative file in
        (rel, Misc.get_git_id (Fname.string (Fname.concat dir rel)))
      )
      g.in_cp#get
    in
    { dir = dir ;
      from_in_data = from_in_data ;
      in_files = in_files ;
      chains = g.chains_cp#get ;
    }
  with
    CF.Missing_cp cp ->
      let msg = Printf.sprintf "Missing field in %s"
        (String.concat "." cp#get_name)
      in
      error ~file msg
  | Stream.Error _ ->
      error ~file "Syntax error"
  | Sys_error s -> error ~file s
;;

let write dir =
  let file = Fname.concat_s dir input_basename in
  let g = mk_spec_group () in
  g.group#read ~on_type_error: (on_type_error file) (Fname.abs_string file);
  g.group#write (Fname.abs_string file)
;;

let list_inputs config =
  let data_dir = Config.data_dir config in
  let pred s = (Filename.basename s) = input_basename in
  let spec_files = Find.find_list
    Find.Stderr [Fname.abs_string data_dir] [Find.Type Unix.S_REG ; Find.Predicate pred]
  in
  List.map
    (fun f -> Fname.path_under ~parent: data_dir (Fname.absolute (Filename.dirname f)))
    spec_files
;;

