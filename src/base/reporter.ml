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

(** Reporter. Reporting error and messages in multiple treatments. *)

type msg =
  | Error of string
  | Msg of string
  | Context of string * int * msg list
;;

class reporter ?(context="") verb_level =
  object(self)
    val mutable total_errors = 0
    val mutable contexts =
      let t = Stack.create () in
      Stack.push (context, 0, []) t;
      t

    method error s =
      let (label, errors, l) = Stack.pop contexts in
      let l = (Error s) :: l in
      Stack.push (label, errors, l) contexts;

    method msg ?(level=0) s =
      if level <= verb_level then
        begin
          let (label, errors, l) = Stack.pop contexts in
          let l = (Msg s) :: l in
          Stack.push (label, errors, l) contexts
        end
    method incr_errors =
      total_errors <- total_errors + 1;
      let (label, errors, l) = Stack.pop contexts in
      Stack.push (label, errors + 1, l) contexts

    method push_context label =
      Stack.push (label, 0, []) contexts

    method pop_context =
      let (label, n, l) =
        try Stack.pop contexts
        with _ -> assert false
      in
      try
        let (label2, n2, l2) = Stack.pop contexts in
        Stack.push (label2, n2 + n, (Context (label, n, l)) :: l) contexts
      with
        Stack.Empty ->
          Stack.push (label, n, l) contexts;
          failwith "No more context to pop !"

    method messages =
      let t = Stack.copy contexts in
      try
        while true do self#pop_context done; assert false
      with
        Failure _ ->
          let (_,_,l) = Stack.pop contexts in
          contexts <- t;
          l

    method total_errors = total_errors
  end


let prefix_lines pref s =
  let lines = Misc.split_string s ['\n'] in
  let lines =
    match lines with
      [] | [_] -> lines
    | h :: q ->
        let q = List.map (fun s -> pref^s) q in
        h :: q
  in
  String.concat "\n" lines
;;

let rec string_of_msg pad label = function
  Error msg -> Printf.sprintf "%s%s[Error] %s"
    pad (match label with "" -> "" | _ -> label^" ")
    (prefix_lines pad msg)
| Msg msg -> Printf.sprintf "%s%s %s" pad label
    (prefix_lines pad msg)
| Context (label,_,l) ->
    Printf.sprintf "%s%s\n%s" pad label
    (string_of_msg_list ~pad: (pad^"  ")  l)

and string_of_msg_list ?(pad="") ?(label="") l =
  let l = List.rev_map (string_of_msg pad label) l in
  String.concat "\n" l
;;
