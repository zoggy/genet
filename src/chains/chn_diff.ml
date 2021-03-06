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

let file_of_out_port ctx iri =
  let config = ctx.ctx_cfg in
  let prefix = config.Config.rest_api in
  let file_iri = Chn_inst.instport_file_iri ctx iri in
  let s_iri = Rdf_iri.string iri in
  match file_iri with
    None -> failwith (Printf.sprintf "No file associated to port iri %S" s_iri)
  | Some file_iri ->
      let s_file_iri = Rdf_iri.string file_iri in
      let out_dir = Config.out_dir config in
      let out_prefix = Grdfs.iri_outfile_path prefix [] in
      try
        let path = Misc.path_under ~parent: (Rdf_iri.string out_prefix) s_file_iri in
        Fname.concat_s out_dir path
      with _ ->
          let msg = Printf.sprintf "%S is not an output file iri" s_file_iri in
          failwith msg
;;

let mkdir ctx ports main_dir n =
  let dir = Fname.concat_s main_dir (string_of_int n) in
  Misc.mkdir (Fname.abs_string dir) ;
  let make_link n port =
    let file = file_of_out_port ctx port in
    try Unix.symlink (Fname.abs_string file)
      (Fname.abs_string (Fname.concat_s dir (string_of_int n)))
    with Unix.Unix_error (e,s1,s2) ->
      let msg = Printf.sprintf "%s: %s %s" (Unix.error_message e) s1 s2 in
      failwith msg
  in
  List.iteri make_link ports
;;

let diff ctx ?(html=false) ?(fragment=false) ?(keepfiles=false) ?diff inst1 inst2 =
  let ports1 = Grdf_port.ports ctx.ctx_rdf inst1 Grdf_port.Out in
  let ports2 = Grdf_port.ports ctx.ctx_rdf inst2 Grdf_port.Out in
  let main_dir = Fname.absolute (Filename.temp_file "genet" "diff") in
  Sys.remove (Fname.abs_string main_dir) ;
  Misc.mkdir (Fname.abs_string main_dir);
  mkdir ctx ports1 main_dir 1 ;
  mkdir ctx ports2 main_dir 2 ;
  let res = Fname.absolute (Filename.temp_file "genetdiff" "result") in
  let diff =
    match diff with
      None -> "diff -r -u"
    | Some s ->
        let iri = Grdfs.iri_diffcommand ~prefix: ctx.ctx_cfg.Config.rest_api ~name: s in
        match Grdf_diff.command_path ctx.ctx_rdf iri with
          None -> s
        | Some path -> path
  in
  let com =
    Printf.sprintf "(cd %s && %s 1 2 %s) > %s 2>&1"
      (Fname.quote main_dir)
      diff
      (if html then
         Printf.sprintf "| highlight --syntax=diff %s"
           (if fragment then " -f" else "")
       else ""
      )
      (Fname.quote res)
  in
  prerr_endline com;
  match Sys.command com with
    2 -> failwith (Printf.sprintf "Command failed: %s" com)
  | _ ->
      let result = Misc.string_of_file (Fname.abs_string res) in
      if not keepfiles then
        (
         let com = Printf.sprintf "rm -fr %s" (Fname.quote main_dir) in
         (try ignore(Sys.command com) with _ -> ())
        );
      Sys.remove (Fname.abs_string res);
      result
;;

let diff_url ctx ?(form=false) ?diff_command ~inst1 ~inst2 () =
  let config = ctx.ctx_cfg in
  let url = Rdf_iri.concat config.Config.rest_api Grdfs.suffix_diff in
  let url = Rdf_iri.concat url Grdfs.suffix_ichains in
  let query = Netencoding.Url.mk_url_encoded_parameters
    [ "form", (if form then "true" else "false") ;
      "inst1", Rdf_iri.string inst1 ;
      "inst2", Rdf_iri.string inst2 ;
      "diffcmd", (Misc.string_of_opt diff_command) ;
    ]
  in
  Printf.sprintf "%s?%s" (Rdf_uri.string (Rdf_iri.to_uri url)) query
;;
