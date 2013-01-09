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

(*i==m=[File.Find]=0.1=t==*)
(** Finding files.
@author Didier Rémy
@version 0.1
@cgname File.Find*)

      type filter =
	  Maxdepth of int
	| Type of Unix.file_kind
	| Follow
	| Regexp of Str.regexp
	| Atime of interval
	| Predicate of (string -> bool)
      and interval =
	  Le of int | Eq of int | Ge of int
      type mode =
	| Ignore
	| Stderr
	| Failure
	| Custom of (Unix.error * string * string -> unit)

      val find : mode -> string list -> filter list ->
        (string -> unit) -> unit

      val find_list : mode -> string list -> filter list -> string list
    
(*/i==m=[File.Find]=0.1=t==*)

