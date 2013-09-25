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

(** Basic types for chains. *)

type user = string
type version_id = string

type context =
  { ctx_rdf: Grdf_types.world ;
    ctx_cfg: Config.t ;
    ctx_user : user option ;
  }

type chain_modname
type chain_basename
type chain_name
val chain_modname : chain_name -> chain_modname
val chain_basename : chain_name -> chain_basename
val mk_chain_name : chain_modname -> chain_basename -> chain_name
val string_of_chain_modname : chain_modname -> string
val string_of_chain_basename : chain_basename -> string
val string_of_chain_name : chain_name -> string
val chain_modname_of_string : string -> chain_modname
val chain_basename_of_string : string -> chain_basename
val chain_name_of_string : string -> chain_name

val compare_chain_modname : chain_modname -> chain_modname -> int
val compare_chain_basename : chain_basename -> chain_basename -> int
val compare_chain_name : chain_name -> chain_name -> int

val iri_chain_module : Grdf_types.iri -> chain_modname -> Grdf_types.iri
val iri_chain : Grdf_types.iri -> chain_name -> Grdf_types.iri

val is_iri_chain_module : Grdf_types.iri -> Grdf_types.iri -> chain_modname option
val is_iri_chain : Grdf_types.iri -> Grdf_types.iri -> chain_name option
val iri_intf_of_interface_spec : prefix: Grdf_types.iri -> string -> Grdf_types.iri

type fchain_name
val fchain_id : fchain_name -> version_id option
val fchain_chainname : fchain_name -> chain_name
val fchain_modname : fchain_name -> chain_modname
val fchain_basename : fchain_name -> chain_basename
val mk_fchain_name : chain_name -> version_id -> fchain_name

val iri_fchain_module : Grdf_types.iri -> chain_modname -> Grdf_types.iri
val iri_fchain : Grdf_types.iri -> fchain_name -> Grdf_types.iri

val is_iri_fchain_module : Grdf_types.iri -> Grdf_types.iri -> chain_modname option
val is_iri_fchain : context -> Grdf_types.iri -> fchain_name option

type ichain_name
val ichain_id : ichain_name -> string
val ichain_chainname : ichain_name -> chain_name
val ichain_modname : ichain_name -> chain_modname
val ichain_basename : ichain_name -> chain_basename
val mk_ichain_name : chain_name -> string -> ichain_name
val string_of_ichain_name : ichain_name -> string

val iri_ichain_module : Grdf_types.iri -> chain_modname -> Grdf_types.iri
val iri_ichain : Grdf_types.iri -> ichain_name -> Grdf_types.iri

val iri_inst_opn_of_flat_opn :
  ?ichain: bool -> ?cpt: int ->
  prefix: Grdf_types.iri -> inst: Grdf_types.iri -> flat: Grdf_types.iri
  -> Grdf_types.iri

val iri_inst_port_of_flat_port :
  ?ichain: bool -> ?cpt: int ->  context -> inst: Grdf_types.iri -> flat: Grdf_types.iri -> Grdf_types.iri

val is_iri_ichain : Grdf_types.iri -> Grdf_types.iri -> ichain_name option

val ichain_op_name : ichain:Rdf_iri.iri -> Rdf_iri.iri -> string
val iri_ichain_op : Rdf_iri.iri -> string list -> Rdf_iri.iri

