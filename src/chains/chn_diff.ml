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

(** Computing diff on (nodes of) instanciated chains. *)

open Chn_types;;

let file_of_out_port ctx uri =
  let config = ctx.ctx_cfg in
  let prefix = config.Config.rest_api in
  let file_uri = Chn_inst.instport_file_uri ctx uri in
  let s_uri = Rdf_uri.string uri in
  match file_uri with
    None -> failwith (Printf.sprintf "No file associated to port uri %S" s_uri)
  | Some file_uri ->
      let s_file_uri = Rdf_uri.string file_uri in
      let out_dir = Config.out_dir config in
      let out_prefix = Grdfs.uri_outfile_path prefix [] in
      try
        let path = Misc.path_under ~parent: (Rdf_uri.string out_prefix) s_file_uri in
        Filename.concat out_dir path
      with _ ->
          let msg = Printf.sprintf "%S is not an output file uri" s_file_uri in
          failwith msg
;;

let mkdir config ports main_dir n =
  let dir = Filename.concat main_dir (string_of_int n) in
  Misc.mkdir dir ;
  let make_link n port =
    let file = file_of_out_port config port in
    try Unix.symlink file (Filename.concat dir (string_of_int n))
    with Unix.Unix_error (e,s1,s2) ->
      let msg = Printf.sprintf "%s: %s %s" (Unix.error_message e) s1 s2 in
      failwith msg
  in
  List.iteri make_link ports
;;

let diff ctx ?(html=false) ?(fragment=false) ?(keepfiles=false) ?(diff="diff -r -u") inst1 inst2 =
  let ports1 = Grdf_port.ports ctx.ctx_rdf inst1 Grdf_port.Out in
  let ports2 = Grdf_port.ports ctx.ctx_rdf inst2 Grdf_port.Out in
  let main_dir = Filename.temp_file "genet" "diff" in
  Sys.remove main_dir ;
  Misc.mkdir main_dir;
  mkdir ctx ports1 main_dir 1 ;
  mkdir ctx ports2 main_dir 2 ;
  let res = Filename.temp_file "genetdiff" "result" in
  let com =
    Printf.sprintf "(cd %s && %s 1 2 %s) > %s 2>&1"
      (Filename.quote main_dir)
      diff
      (if html then
         Printf.sprintf "| highlight --syntax=diff %s"
           (if fragment then " -f" else "")
       else ""
      )
      (Filename.quote res)
  in
  match Sys.command com with
    2 -> failwith (Printf.sprintf "Command failed: %s" com)
  | _ ->
      let result = Misc.string_of_file res in
      if not keepfiles then
        (
         let com = Printf.sprintf "rm -fr %s" (Filename.quote main_dir) in
         (try ignore(Sys.command com) with _ -> ())
        );
      Sys.remove res;
      result
;;
