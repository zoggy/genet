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

(** Generation of cheat sheet and bash completion parameters for main command line tool. *)

open Cmdline;;

let common_options =
  Options.option_version "Genet" ::
  Options.option_config ::
  Options.option_verbose ::
  []
;;

let command = {
  com_options = common_options ;
  com_usage = "<command> [arguments]" ;
  com_compl = [] ;
  com_kind = Commands (Main_cmd.subcommands()) ;
  }
;;

let pb b = Printf.bprintf b ;;
let p b = Buffer.add_string b ;;
let pn b s = Buffer.add_string b (s^"\n") ;;

let print_option b (name, _, desc) =
  let len = String.length desc in
  let (arg, desc) =
    if len <= 0 || desc.[0] = ' ' then
      (None, desc)
    else
      begin
        try
          let p = String.index desc ' ' in
          let arg = String.sub desc 0 p in
          let desc = String.sub desc (p+1) (len - p - 1) in
          (Some arg, desc)
        with Not_found -> (None, desc)
      end
  in
  pb b "<tr><td><strong>%s%s</strong></td><td><![CDATA[%s]]></td></tr>\n"
    name
    (match arg with
       None -> ""
     | Some s -> Printf.sprintf " <![CDATA[%s]]>" s
    )
    desc
;;

let print_options b l =
  match l with
    [] -> ()
  | _ ->
    pn b "<table class=\"table table-stripped table-bordered table-condensed\">";
    List.iter (print_option b) l;
    pn b "</table>";
;;

let id_of_command_path path = String.concat "" path;;

let print_subcommand_list =
  let print_one path (name, _, _) =
    let id = id_of_command_path (path @ [name]) in
    Printf.sprintf "<a href=\"#%s\"><code>%s</code></a>" id name
  in
  fun b path com ->
    match com.com_kind with
      Cmdline.Final _ -> ()
    | Cmdline.Commands l ->
        let l = List.map (print_one path) l in
        pn b "<p class=\"subcommands\">";
        p b "<strong>Subcommands: </strong>";
        p b (String.concat ", " l);
        pn b "</p>"
;;

let rec print_com_doc b path com =
  let section_tag =
    let len = List.length path in
    match len with
      2 -> "section"
    | 3 -> "subsection"
    | 4 -> "subsubsection"
    | _ -> failwith (Printf.sprintf "command path depth %d not handled" len)
  in
  pb b "<%s id=\"%s\" title=\"%s\">\n"
    section_tag (id_of_command_path path)
    (List.hd (List.rev path));
  (
   match com.com_kind with
     Final _ ->
       pb b "<command-line><![CDATA[%s %s]]></command-line>"
         (String.concat " " path) com.com_usage
   | _ -> ()
  );

  print_options b com.com_options ;
  print_subcommand_list b path com ;
  (
   match com.com_kind with
     Cmdline.Final _ -> ()
   | Cmdline.Commands l ->
       pn b "<div class=\"subcommands\">";
       List.iter (fun (name, com, _) -> print_com_doc b (path@[name]) com) l;
       pn b "</div>";
  );
  pb b "</%s>\n" section_tag
;;

let print_collapsable_options b path com =
  match com.com_options with
    [] -> ()
  | opts ->
      let id = String.concat "" (path@["options"]) in
      pb b "<span class=\"label label-info command-options-button\" data-target=\"#%s\" data-toggle=\"collapse\">+</span>"
        id;
      pb b "<div class=\"collapse\" id=%S>" id;
      print_options b com.com_options ;
      p b "</div>"
;;

let rec print_com_tree b path ?(desc="") com =
  match com.com_kind with
    Final _ ->
      pb b "<code><![CDATA[%s %s]]></code>"
        (String.concat " " path) com.com_usage;
      if desc <> "" or com.com_options <> [] then
        begin
          pb b "<div class=\"command-desc\"><![CDATA[%s ]]> " desc;
          print_collapsable_options b path com;
          p b "</div>"
        end
  | Commands coms ->
      if List.length path > 1 then
        begin
          pb b "<code>%s</code>" (List.hd (List.rev path));
          if desc <> "" || com.com_options <> [] then
            begin
              pb b " <span class=\"command-desc\"><![CDATA[%s ]]> " desc;
              print_collapsable_options b path com;
              p b "</span>";
            end
        end;
      pn b "<ul class=\"synopsis-tree\">";
      let coms = List.sort
        (fun (s1, _, _) (s2, _, _) -> Pervasives.compare s1 s2)
          coms
      in
      List.iter (print_subcom b path) coms;
      pn b "</ul>";
and print_subcom b path (name, com, desc) =
  p b "<li>";
  print_com_tree b (path@[name]) ~desc com;
  p b "</li>"
;;

let print_doc main_name =
  let b = Buffer.create 256 in
  pn b "<main_>";
  pn b "<section id=\"globalopts\" title=\"Global options\">";
  print_options b command.com_options ;
  pn b "</section>";
  pn b "<section id=\"synopsis\" title=\"Synopsis tree\">";
  print_com_tree b ["genet"] command;
  pn b "</section>";
(*
  print_subcommand_list b [] command ;
  (
   match command.com_kind with
     Cmdline.Final _ -> ()
   | Cmdline.Commands l ->
       List.iter (fun (name, com, _) -> print_com_doc b [main_name ; name] com) l
  );
*)
  pn b "</main_>";
  print_endline (Buffer.contents b)
;;

let completion () =
  let len = Array.length Sys.argv in
  let stop = int_of_string Sys.argv.(1) in
  let args = Array.sub Sys.argv 2 (len - 2) in
  (*
     prerr_endline (Printf.sprintf "stop=%d in %d args" stop (Array.length args));
     prerr_endline (Printf.sprintf "args=%s" (String.concat " " (Array.to_list args)));
     *)
  (*      let escape s =
     String.concat "\\:" (Misc.split_string s [':'])
     in
  *)      let to_w choices =
    let choices = List.map Filename.quote choices in
    let s = String.concat " " choices in
    Printf.sprintf "-W %S" s
  in
  let res =
    let t = Cmdline.completion stop args command in
    (match t.compl_words with
       [] -> ""
     | l -> to_w l)^" "^
      (if t.compl_files then "-f" else "")^" "^
      (match t.compl_xfiles with
             None -> ""
       | Some pat -> Printf.sprintf "-X %S" pat)
      ^" -- "
  in
  print_endline res
;;

let len = Array.length Sys.argv;;
let () =
  match len with
  | 2 when Sys.argv.(1) = "--doc" -> print_doc "genet"
  | n when n >= 3 -> completion ()
  | _ -> exit 0
;;
