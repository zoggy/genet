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

let options =
  Options.option_version "Genet-diff" ::
  Options.option_config ::
  []
;;

let diff ctx inst1 inst2 = ()

let usage =
  Printf.sprintf "Usage: %s [options] <instanciation1> <instanciation2>" Sys.argv.(0);;

let main () =
  let opts = Options.parse options in
  let (inst1, inst2) =
    match opts.Options.args with
      [uri1 ; uri2] -> (Rdf_uri.uri uri1, Rdf_uri.uri uri2)
    | _ -> failwith usage
  in
  let config = Config.read_config opts.Options.config_file in
  let rdf_wld = Grdf_init.open_graph config in
  let ctx = {
      Chn_types.ctx_rdf = rdf_wld ;
      ctx_cfg = config ; ctx_user = None ;
    }
  in
  diff ctx inst1 inst2
;;

let () = Misc.safe_main main;;