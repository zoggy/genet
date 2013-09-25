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

(** Access to genet main configuration file. *)

type t = {
  project_name : string;
  project_id : string;
  db_storage : string;
  db_engine : string;
  db_name : string;
  db_user : string;
  db_passwd : string;
  db_host : string;
  rest_api : Rdf_iri.iri ;
  root_dir : [`Absolute] Fname.filename ;
}
val read_config : string -> t
val string_of_config : t -> string

val in_dir : t -> [`Absolute] Fname.filename
val out_dir : t -> [`Absolute] Fname.filename
val chains_dir : t -> [`Absolute] Fname.filename
val data_dir : t -> [`Absolute] Fname.filename
val web_dir : t -> [`Absolute] Fname.filename
