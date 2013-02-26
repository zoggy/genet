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

type option_spec = string * Cmdline.spec * string

let print_version pref () =
  Printf.printf "%s version %s\n" pref Version.version;
  exit 0
;;

let option_version pref =
  "--version", Cmdline.Unit (print_version pref),
  " Print version number and exit"
;;

let config_file = ref Install.default_config_file;;
let option_config =
  "--config", Cmdline.Set_string
  (Some (fun ()-> Cmdline.Files ([], None)), config_file),
  "<file> use <file> as config file instead of "^ !config_file
;;

let verb_level = ref 0;;
let option_verbose =
  "-v", Cmdline.Unit (fun () -> incr verb_level),
  " increase verbosity level"
;;

let rdf_output_format = ref "turtle";;
let mk_rdf_output_format f =
  "--"^f, Cmdline.Unit (fun () -> rdf_output_format := f),
  " set rdf output format to "^f^" instead of "^ !rdf_output_format
;;

let option_rdfxml = mk_rdf_output_format "rdfxml";;
let option_ntriples = mk_rdf_output_format "ntriples";;

type option_values = {
  config_file : string ;
  args : string list ;
  verb_level : int ;
  rdf_output_format : string ;
  }

let remaining = ref [] ;;

let build_option_values () =
 { config_file = !config_file ;
    args = List.rev !remaining ;
    verb_level = !verb_level ;
    rdf_output_format = !rdf_output_format ;
  }
;;

let parse options =
  let options = Cmdline.specs_to_arg_specs options in
  Arg.parse (Arg.align options) (fun s -> remaining := s :: !remaining)
    (Printf.sprintf "Usage: %s [options] [arguments]" Sys.argv.(0));
  build_option_values ()
;;

let parse_command com =
  remaining := Cmdline.parse com ;
  build_option_values ()
;;