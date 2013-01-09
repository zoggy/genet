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

(** Main module of the genet-diff program.*)

open Chn_types;;

let options =
  Options.option_version "Genet-diff" ::
  Options.option_config ::
  []
;;

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

let mkdir config ports n =
  let dir = Filename.temp_file "genetdiff" ("-"^(string_of_int n)) in
  Sys.remove dir ; Misc.mkdir dir;
  let make_link n port =
    let file = file_of_out_port config port in
    try Unix.symlink file (Filename.concat dir (string_of_int n))
    with Unix.Unix_error (e,s1,s2) ->
      let msg = Printf.sprintf "%s: %s %s" (Unix.error_message e) s1 s2 in
      failwith msg
  in
  List.iteri make_link ports;
  dir
;;

let diff ctx inst1 inst2 =
  let ports1 = Grdf_port.ports ctx.ctx_rdf inst1 Grdf_port.Out in
  let ports2 = Grdf_port.ports ctx.ctx_rdf inst1 Grdf_port.Out in
  let dir1 = mkdir ctx ports1 1 in
  let dir2 = mkdir ctx ports2 2 in
  let res = Filename.temp_file "genetdiff" "result" in
  let com = Printf.sprintf "diff -r %s %s > %s 2>&1"
    (Filename.quote dir1) (Filename.quote dir2) (Filename.quote res)
  in
  let output = Misc.exec_command com in
  output
;;

let usage =
  Printf.sprintf "Usage: %s [options] <instanciation1> <instanciation2>" Sys.argv.(0);;

let main () =
  let opts = Options.parse options in
  let (inst1, inst2) =
    match opts.Options.args with
      [uri1 ; uri2] -> (Rdf_uri.uri uri1, Rdf_uri.uri uri2)
    | _ -> failwith usage
  in
  let config = Config.read_config opts.Options.config_file in
  let rdf_wld = Grdf_init.open_graph config in
  let ctx = {
      Chn_types.ctx_rdf = rdf_wld ;
      ctx_cfg = config ; ctx_user = None ;
    }
  in
  let output = diff ctx inst1 inst2 in
  print_string output
;;

let () = Misc.safe_main main;;