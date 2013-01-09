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

type t = { loc_start: Lexing.position; loc_end: Lexing.position }

let dummy = { loc_start = Lexing.dummy_pos ; loc_end = Lexing.dummy_pos };;

type problem = t Checks.problem

exception Problem of problem

let raise_problem loc msg =
  let pb = Checks.problem loc msg in
  raise (Problem pb)
;;


(* Print the location in some way or another *)

open Format
open Lexing

let (msg_file, msg_line, msg_char, msg_chars, msg_to, msg_colon, msg_head) =
  ("File \"", "\", line ", ", character ", ", characters ", "-", ":", "")

(* return file, line, char from the given position *)
let get_pos_info pos =
  let (filename, linenum, linebeg) =
    if pos.pos_fname = "" then
      ("", -1, 0)
    else
      (pos.pos_fname, pos.pos_lnum, pos.pos_bol)
  in
  (filename, linenum, pos.pos_cnum - linebeg)
;;

let print ppf loc =
  let (file, line, startchar) = get_pos_info loc.loc_start in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
  let (startchar, endchar) =
    if startchar < 0 then (0, 1) else (startchar, endchar)
  in
  fprintf ppf "%s%s%s%i" msg_file file msg_line line;
  if startchar <> endchar then
    begin
      fprintf ppf "%s%i" msg_chars startchar;
      fprintf ppf "%s%i%s@.%s" msg_to endchar msg_colon msg_head
    end
  else
    begin
      fprintf ppf "%s%i" msg_char startchar;
      fprintf ppf "%s@.%s" msg_colon msg_head
    end
;;

let string_of_loc loc =
  print Format.str_formatter loc;
  Format.flush_str_formatter ()
;;

let string_of_problem pb =
  Printf.sprintf "%sError: %s" (string_of_loc pb.Checks.pb_ref) pb.Checks.pb_msg
;;
