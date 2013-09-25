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

(** Types for REST API. *)

type arg = string * string

val get_arg : arg list -> string -> string

type iri = Grdf_types.iri
type path = string

type json = string (*Yojson.Basic.json*)

type met =
  | Get of path * arg list
  | Delete of path
  | Post of path * json
  | Put of path * json
;;

type content_type = Xhtml | Json

type inst_chain_query =
  { iq_chain : Rdf_iri.iri option ;
    iq_tools : Rdf_iri.iri Rdf_iri.Irimap.t ;
    iq_input : (string list * string option) option ; (** input path * git id option *)
  }
;;

type thing =
  | Tool of iri
  | Branch of iri
  | Version of iri
  | Intf of iri
  | Filetype of iri
  | Tools
  | Branches of iri (** iri of tool *)
  | Versions of iri (** iri of tool or branch *)
  | Intfs of iri (** iri of tool or branch *)
  | Filetypes
  | Static_file of string * string
  | Other of Rdf_iri.iri
  | Chains
  | Chain_module of Chn_types.chain_modname
  | Chain of Chn_types.chain_name
  | Flat_chains
  | Flat_chain_module of Chn_types.chain_modname
  | Flat_chain of Rdf_iri.iri
  | Flat_chain_list of Chn_types.fchain_name
  | Inst_chain of Rdf_iri.iri
  | Out_file of string list * bool (** true = raw access *)
  | Inst_producers_of of string list (** inst ports producing file path *)
  | Inputs
  | Input of string list
  | Input_file of string list * string list * bool (** true = raw access *)
  | Inst_chains
  | Inst_chain_query of inst_chain_query
  | Inst_chain_op of Rdf_iri.iri
  | Diff_inst_chains
;;

type response = arg list * string

type user = Chn_types.user

type context = Chn_types.context =
  { ctx_rdf: Grdf_types.world ;
    ctx_cfg: Config.t ;
    ctx_user : user option ;
  }

type get_handler = context -> thing -> arg list -> response
type delete_handler = context -> thing -> response
type post_handler = context -> thing -> json -> response
type put_handler = context -> thing -> json -> response

type content_type_handlers = {
  h_get : get_handler ;
  h_del : delete_handler ;
  h_post : post_handler ;
  h_put : put_handler ;
  }

