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

(** Common options for drivers. *)

type option_spec = string * Cmdline.spec * string

(** [option_version prefix] returns an option spec to print
   the given prefix followed by " version ...". *)
val option_version : string -> option_spec

val option_config : option_spec

val option_verbose : option_spec

val option_rdfxml : option_spec
val option_ntriples : option_spec

type option_values = {
  config_file : string  ; (** config filename *)
  args : string list ; (** arguments *)
  verb_level : int ; (** verbosity level *)
  rdf_output_format : string ; (** format to use when output rdf *)
  }

(** [parse options] parses the command line using the given option specifications.
     @return the values of common options. *)
val parse : option_spec list -> option_values

(** [parse_command command] parses the command line according to the given
  specification or type {!Cmdline.command} .
     @return the values of common options. *)
val parse_command : Cmdline.command -> option_values







