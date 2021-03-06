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

(** Initialization of genet directory and database. *)

open Cmdline;;

let git_repo = ref None;;


let init_dir opts =
  let dir =
    match opts.Options.args with
      [] -> Fname.absolute (Sys.getcwd())
    | dir :: _ ->
        if Filename.is_relative dir then
          Fname.concat (Fname.absolute (Sys.getcwd())) (Fname.relative dir)
        else
          Fname.absolute dir
  in
  let verbose = opts.Options.verb_level > 0 in
  let mkdir = Misc.mkdir ~verbose in
  mkdir (Fname.abs_string dir);
  let config_file = Install.default_config_file in
  let config = Config.read_config (Filename.concat (Fname.abs_string dir) config_file) in
  let config = { config with Config.root_dir = dir } in
  mkdir (Fname.abs_string (Config.out_dir config));
  let in_dir = Config.in_dir config in
  begin
    match !git_repo with
      None ->
        mkdir (Fname.abs_string (Config.chains_dir config));
        mkdir (Fname.abs_string (Config.data_dir config));
        let web_dir = Config.web_dir config in
        begin
          let com = Printf.sprintf "cp -r %s %s"
            (Filename.quote Install.share_web_dir)
            (Fname.quote web_dir)
          in
          if verbose then
            print_endline
              (Printf.sprintf "copying %s to %s"
               Install.share_web_dir (Fname.abs_string web_dir));
          match Sys.command com with
            0 -> ()
          | _ -> failwith (Printf.sprintf "Command failed: %s" com)
        end
    | Some repo ->
        let com = Printf.sprintf "git clone %s %s"
          (Filename.quote repo) (Fname.quote in_dir)
        in
        if verbose then
          print_endline (Printf.sprintf "Cloning %s into %s" repo (Fname.abs_string in_dir));
        match Sys.command com with
          0 -> ()
        | _ -> failwith (Printf.sprintf "Command failed: %s" com)
  end
;;

let com_init_dir = {
    com_options = [
      "--git", Cmdline.String (None, fun s -> git_repo := Some s),
      "<repo> will create the 'in' directory by cloning the repository" ;
    ] ;
    com_usage = "[<directory>]" ;
    com_compl = [ Cmdline.Compfun Main_cmd.compl_file ] ;
    com_kind = Final (fun () -> Main_cmd.set_final_fun init_dir) ;
  }
;;

let init_db _  = ();;
let com_init_db = {
    com_options = [] ;
    com_usage = "" ;
    com_compl = [] ;
    com_kind = Final (fun () -> Main_cmd.set_final_fun init_db);
  }
;;

Main_cmd.register_subcommand "init-dir" com_init_dir "initialize directory";;
Main_cmd.register_subcommand "init-db" com_init_db "initialize database";;
