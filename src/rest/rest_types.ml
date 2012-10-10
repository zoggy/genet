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

type arg = string * string


let get_arg args name =
  try List.assoc name args
  with Not_found -> ""
;;

type uri = Grdf_types.uri
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
  { iq_chain : Rdf_uri.uri option ;
    iq_tools : Rdf_uri.uri Urimap.t ;
    iq_input : (string list * string option) option ; (* input path * git id option *)
  }
;;

type thing =
  | Tool of uri
  | Branch of uri
  | Version of uri
  | Intf of uri
  | Filetype of uri
  | Tools
  | Branches of uri (* uri of tool *)
  | Versions of uri (* uri of tool or branch *)
  | Intfs of uri (* uri of tool or branch *)
  | Filetypes
  | Static_file of string * string
  | Other of Rdf_uri.uri
  | Chains
  | Chain_module of Chn_types.chain_modname
  | Chain of Chn_types.chain_name
  | Flat_chains
  | Flat_chain_module of Chn_types.chain_modname
  | Flat_chain of Rdf_uri.uri
  | Flat_chain_list of Chn_types.fchain_name
  | Inst_chain of Rdf_uri.uri
  | Out_file of string list * bool (* true = raw access *)
  | Inst_producers_of of string list (* inst ports producing file path *)
  | Inputs
  | Input of string list
  | Input_file of string list * string list * bool
  | Inst_chains
  | Inst_chain_query of inst_chain_query
  | Inst_chain_op of Rdf_uri.uri
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
