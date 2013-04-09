/*********************************************************************************/
/*                Genet                                                          */
/*                                                                               */
/*    Copyright (C) 2012-2013 Institut National de Recherche en Informatique     */
/*    et en Automatique. All rights reserved.                                    */
/*                                                                               */
/*    This program is free software; you can redistribute it and/or modify       */
/*    it under the terms of the GNU General Public License version 3             */
/*    or later as published by the Free Software Foundation.                     */
/*                                                                               */
/*    This program is distributed in the hope that it will be useful,            */
/*    but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              */
/*    GNU General Public License for more details.                               */
/*                                                                               */
/*    You should have received a copy of the GNU General Public License          */
/*    along with this program; if not, write to the Free Software Foundation,    */
/*    Inc., 59 Temple Place, Suite 330, Boston, MA                               */
/*    02111-1307  USA                                                            */
/*                                                                               */
/*    Contact: Maxence.Guesdon@inria.fr                                          */
/*                                                                               */
/*                                                                               */
/*********************************************************************************/

(** Type parser *)

%start <string Grdf_types.port_type> port_type
%start <string> filetype_ident
%start <string> port_ident

%%

port_type: t=the_port_type option(EOF) { t }

the_port_type:
  s=Ident { Grdf_types.T s }
| s=Var { Grdf_types.Var s }
| t=the_port_type SET { Grdf_types.Set t }
| LPAR l=separated_nonempty_list (STAR, the_port_type) RPAR { Grdf_types.Tuple l }

filetype_ident: s=Ident option(EOF) { s }
port_ident: s=Ident option(EOF) { s }
