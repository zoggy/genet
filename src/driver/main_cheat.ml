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

(** Generation of cheat sheet for main command line tool. *)

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
  com_kind = Commands (Main_cmd.subcommands()) ;
  }
;;
let len = Array.length Sys.argv;;
let () =
  if len < 3 then
    exit 0
  else
    begin
      let stop = int_of_string Sys.argv.(1) in
      let args = Array.sub Sys.argv 2 (len - 2) in
      Cmdline.bash_completion stop args command
    end
;;