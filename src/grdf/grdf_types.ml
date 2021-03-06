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

type iri = Rdf_iri.iri

type world =
  { wld_graph : Rdf_graph.graph ;
    wld_prefix : iri ;
  }

type error =
  | Tool_exists of string
  | Branch_exists of string
  | Version_exists of string
  | Intf_exists of string
  | Filetype_exists of string
  | Not_tool_or_branch of iri
  | Not_branch_or_version of iri
  | Not_a_tool of iri
  | Not_a_version of iri
  | Tool_of_branch_differs of iri * iri * iri

exception Error of error

let error e = raise (Error e);;

let string_of_error = function
| Tool_exists name -> Printf.sprintf "Tool %s already exists." name
| Branch_exists name -> Printf.sprintf "Branch %s already exists." name
| Version_exists name -> Printf.sprintf "Version %s already exists." name
| Intf_exists name -> Printf.sprintf "Interface %s already exists." name
| Filetype_exists name -> Printf.sprintf "Filetype %s already exists." name
| Not_tool_or_branch iri -> Printf.sprintf "%s is neither a tool nor a branch." (Rdf_iri.string iri)
| Not_branch_or_version iri -> Printf.sprintf "%s is neither a branch nor a version." (Rdf_iri.string iri)
| Not_a_tool iri -> Printf.sprintf "%s is not a tool." (Rdf_iri.string iri)
| Not_a_version iri -> Printf.sprintf "%s is not a version." (Rdf_iri.string iri)
| Tool_of_branch_differs (branch, branch_tool, tool) ->
    Printf.sprintf "The tool of branch %s is %s but %s was expected."
    (Rdf_iri.string branch) (Rdf_iri.string branch_tool) (Rdf_iri.string tool)
;;

type 'a port_type = Var of string | T of 'a | Set of 'a port_type | Tuple of ('a port_type list) ;;
