(*********************************************************************************)
(*                Genet                                                          *)
(*                                                                               *)
(*    Copyright (C) 2012 Institut National de Recherche en Informatique          *)
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

let print_warning = ref (fun s -> prerr_endline ("Warning: "^s));;
let set_print_warning_fun f = print_warning := f;;
let print_warning s = !print_warning s;;

type 'a problem =
  { pb_ref : 'a ;
    pb_msg : string ;
    pb_exn: exn option ;
  }

let problem ?exn t s = { pb_ref = t ; pb_msg = s ; pb_exn = exn};;

type 'a check_result = {
  chk_errors : 'a problem list ;
  chk_warnings : 'a problem list ;
}

let check_result ?(warns=[]) errs =
  { chk_errors = errs; chk_warnings = warns };;

let empty_check_result = { chk_errors = []; chk_warnings = [] };;

let is_empty c = c.chk_errors = [] && c.chk_warnings = [];;

let merge_check_result t1 t2 =
  { chk_errors = t1.chk_errors @ t2.chk_errors ;
    chk_warnings = t1.chk_warnings @ t2.chk_warnings ;
  }
;;

let fold_check ?(acc=empty_check_result) f l =
  List.fold_right
    (fun d acc -> merge_check_result (f d) acc)
    l acc
  ;;

let string_of_problem f_ref pb =
  Printf.sprintf "%s: %s"
    (f_ref pb.pb_ref) pb.pb_msg
;;

let string_of_check_result f_ref chk =
  let b = Buffer.create 256 in
  let print_pb_list =
    let f pb =
      Printf.bprintf
        b "%s\n" (string_of_problem f_ref pb)
    in
    List.iter f
  in
  begin
    match chk.chk_errors with
      [] -> Buffer.add_string b "No error.\n"
    | l ->
        let n = List.length l in
        Printf.bprintf b "%d error%s:\n" n (if n > 1 then "s" else "");
        print_pb_list l
  end;
  begin
    match chk.chk_warnings with
      [] -> Buffer.add_string b "No warning.\n"
    | l ->
        let n = List.length l in
        Printf.bprintf b "%d warning%s:\n" n (if n > 1 then "s" else "");
        print_pb_list l
  end;
  Buffer.contents b
;;
