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

open Rdf_node;;
open Grdf_types;;
open Rdf_sparql;;

let dbg = Misc.create_log_fun
  ~prefix: "Grdf_ftype"
    "GENET_GRDF_FTYPES_DEBUG_LEVEL"
;;

let filetypes wld =
  dbg ~level: 1 (fun () -> "Grdf_ftype.filetypes");
  Grdfs.subject_uris wld
    ~pred: Grdfs.rdf_type ~obj: (Uri Grdfs.genet_filetype)
;;

let parse_filetype_ident str =
  let lexbuf = Lexing.from_string str in
  try Grdf_parser.filetype_ident Lexer.main lexbuf
  with
    Grdf_parser.Error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      let loc = { Loc.loc_start = pos ; Loc.loc_end = pos } in
      Loc.raise_problem loc (Printf.sprintf "Invalid filetype ident: %s" str)
;;

let name wld uri = Grdfs.name wld (Uri uri);;

let desc wld uri = Grdfs.desc wld (Uri uri);;

let extension wld uri =
  dbg ~level: 1 (fun () -> "Grdf_ftype.extension uri="^(Rdf_uri.string uri));
  let source = Uri uri in
  Misc.string_of_opt (Grdfs.object_literal wld source Grdfs.genet_file_ext)
;;

let filetype_exists wld uri =
  dbg ~level: 1 (fun () -> "Grdf_ftype.filetype_exists uri="^(Rdf_uri.string uri));
  if Grdfs.is_a_filetype wld uri then
    Some (Grdfs.name wld (Uri uri))
  else
    None
;;

let do_add wld uri ~name ~desc ~extension =
  dbg ~level: 1 (fun () -> "Grdf_ftype.do_add uri="^(Rdf_uri.string uri)^" name="^name);
  let sub = Uri uri in
  let obj = Uri Grdfs.genet_filetype in
  Grdfs.add_type wld ~sub ~obj;
  Grdfs.add_name wld sub name;
  Grdfs.add_desc wld sub desc;
  let pred = Uri Grdfs.genet_file_ext in
  let obj = Rdf_node.node_of_literal_string extension in
  Grdfs.add_triple wld ~sub ~pred ~obj
;;

let add wld ~name ~desc ~extension =
  dbg ~level: 1 (fun () -> "Grdf_ftype.add name="^name);
  let uri = Grdfs.uri_filetype wld.wld_prefix name in
  match filetype_exists wld uri with
    Some name-> Grdf_types.error (Grdf_types.Filetype_exists name)
  | None -> do_add wld uri ~name ~desc ~extension; uri
;;

let string_of_filetype wld uri =
  let name = name wld uri in
  let ext = extension wld uri in
  Printf.sprintf "%s (.%s)" name ext
;;
