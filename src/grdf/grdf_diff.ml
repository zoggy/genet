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

open Rdf_term;;
open Grdf_types;;

let dbg = Misc.create_log_fun
  ~prefix: "Grdf_diff"
    "GENET_GRDF_DIFF_DEBUG_LEVEL"
;;

let diffcommands wld =
  dbg ~level: 1 (fun () -> "Grdf_diff.diff_commands");
  let l = Grdfs.subject_iris wld ~pred: Grdfs.rdf_type ~obj: (Iri Grdfs.genet_diffcommand) in
  Grdfs.iriset_of_list l
;;

let name wld iri = Grdfs.name wld (Iri iri);;

let command_path wld iri =
  let pred = Grdfs.genet_haspath in
  Grdfs.object_literal wld ~sub: (Iri iri) ~pred
;;

let set_command_path wld iri path =
  let pred = Grdfs.genet_haspath in
  Grdfs.add_triple wld ~sub: (Rdf_term.Iri iri) ~pred
    ~obj: (Rdf_term.term_of_literal_string path)
;;

let diffcommand_exists wld iri =
  dbg ~level: 1 (fun () -> "Grdf_diff.diffcommand_exists iri="^(Rdf_iri.string iri));
  if Grdfs.is_a_diffcommand wld iri then
    Some (name wld iri)
  else
    None
;;

let do_add wld iri name =
  dbg ~level: 1 (fun () -> "Grdf_diff.do_add iri="^(Rdf_iri.string iri)^" name="^name);
  let sub = Iri iri in
  Grdfs.add_type wld ~sub ~obj: (Iri Grdfs.genet_diffcommand);
  Grdfs.add_name wld sub name
;;

let add prefix wld ?(force=false) ~name ~path =
  dbg ~level: 1 (fun () -> "Grdf_diff.add name="^name);
  let iri = Grdfs.iri_diffcommand ~prefix ~name in
  begin
    match diffcommand_exists wld iri with
      Some _ ->
        begin
          match force with
            false -> failwith ("Diff command already exists: "^(Rdf_iri.string iri))
          | true -> set_command_path wld iri path
        end
    | None ->
        do_add wld iri name;
        set_command_path wld iri path
  end;
  iri
;;

let parse_diffcommand_ident str =
  let lexbuf = Lexing.from_string str in
  try Grdf_parser.diffcommand_ident Lexer.main lexbuf
  with
    Grdf_parser.Error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      let loc = { Loc.loc_start = pos ; Loc.loc_end = pos } in
      Loc.raise_problem loc (Printf.sprintf "Invalid diffcommand ident: %s" str)
;;