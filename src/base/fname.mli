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

(** Using type system to distinguish relative and absolute filenames. *)

type 'a filename

val absolute : string -> [`Absolute] filename
val relative : string -> [`Relative] filename

val quote : [<`Absolute] filename -> string
val rel_quote : [<`Relative] filename -> string

val concat : [<`Absolute] filename -> [<`Relative] filename -> [<`Absolute] filename
val concat_s : [<`Absolute] filename -> string -> [<`Absolute] filename

val path_under : parent: [<`Absolute] filename -> [<`Absolute] filename -> [<`Relative] filename

val string : [< `Absolute | `Relative] filename -> string
val abs_string : [< `Absolute] filename -> string
val rel_string : [< `Relative] filename -> string

val compare : 'a filename -> 'a filename -> int

