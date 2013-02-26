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

open Cmdline;;
open Chn_types;;

let usage =
  Printf.sprintf "Usage: %s diff [options] <instanciation1> [<instanciation2>]" Sys.argv.(0);;
let html_output = ref false;;
let diff_command = ref None;;
let keep_files = ref false;;
let url_if_diff = ref false;;

let options =
  [
    "--diff", Cmdline.String (None, fun c -> diff_command := Some c),
    "<com> use com as diff command; default is diff -r -u" ;

    "--html", Cmdline.Set html_output, " output HTML instead of raw diff output" ;

    "--keep-files", Cmdline.Set keep_files, " do not erase files after computing diffs";

    "--url-if-diff", Cmdline.Set url_if_diff,
    " if there are diffs, print url instead (useful for nightly reports)";
  ]
;;


let diff ctx opts =
  let (inst1, inst2) =
    match opts.Options.args with
      [uri1] ->
        begin
          let uri1 = Rdf_uri.uri uri1 in
          match Chn_inst.reference_inst_of_inst ctx uri1 with
            None ->
              let msg = Printf.sprintf
                "No reference inst chain found for inst chain %S"
                (Rdf_uri.string uri1)
              in
              failwith msg
          | Some uri2 ->
              (uri2, uri1)
        end
    | [uri1 ; uri2] -> (Rdf_uri.uri uri1, Rdf_uri.uri uri2)
    | _ -> failwith usage
  in
  let output = Chn_diff.diff ctx
    ~html: !html_output
    ~keepfiles: !keep_files
    ?diff: !diff_command inst1 inst2
  in
  if !url_if_diff then
    if output <> "" then
      (
       let url = Chn_diff.diff_url ctx
         ?diff_command: !diff_command ~inst1 ~inst2 ()
       in
       print_endline url
      )
    else
      ()
  else
    print_string output
;;

let command = {
    com_options = options ;
    com_usage = "<instanciation1> [<instanciation2>]" ;
    com_kind = Main_cmd.mk_final_fun (Main_cmd.mk_ctx_fun diff) ;
  }
;;

Main_cmd.register_subcommand "diff" command "compute differences between executions" ;;
