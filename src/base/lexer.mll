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

{
(** Common lexer. *)

open Tokens;;

let error lexbuf msg =
  let pos = lexbuf.Lexing.lex_curr_p in
  let loc = { Loc.loc_start = pos ; Loc.loc_end = pos } in
  Loc.raise_problem loc "Syntax error"
;;

let keywords = [
    "chain", CHAIN ;
    "foreach", FOREACH ;
    "operation", OPERATION ;
    "in", IN ;
    "out", OUT ;
    "set", SET ;
  ]

let string_buffer = Buffer.create 256 ;;
}


let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let digit = ['0'-'9']
let identchar = lowercase | uppercase | digit | '_'

let int = '-'?digit+

let ident = lowercase identchar*
let capident = uppercase identchar*

rule main = parse
| '"' { Buffer.reset string_buffer; string lexbuf }
| "(*" { Buffer.reset string_buffer; comment lexbuf }
| '\n'
    {
      let module L = Lexing in
      let pos = lexbuf.L.lex_curr_p in
      lexbuf.L.lex_curr_p <-
        { pos with
          L.pos_lnum = pos.L.pos_lnum + 1 ;
          pos_bol = pos.L.pos_cnum ;
        };
      main lexbuf
    }
| ' ' { main lexbuf }
| '\''ident {
    let lexeme = Lexing.lexeme lexbuf in
    let var = String.sub lexeme 1 (String.length lexeme - 1) in
    Var var
  }
| ident {
    let lexeme = Lexing.lexeme lexbuf in
    try List.assoc (String.lowercase lexeme) keywords
    with Not_found -> Ident lexeme
  }
| capident {
    let lexeme = Lexing.lexeme lexbuf in
    try List.assoc (String.lowercase lexeme) keywords
    with Not_found ->
        CapIdent lexeme
  }
| '.' { DOT }
| '{' { LBRACE }
| '}' { RBRACE }
| '(' { LPAR }
| ')' { RPAR }
| '*' { STAR }
| ';' { SEMICOLON }
| ':' { COLON }
| ',' { COMMA }
| int { Int (int_of_string (Lexing.lexeme lexbuf)) }
| "->" { RIGHTARROW }
| eof { EOF }
| _ { error lexbuf (Printf.sprintf "Invalid character %s" (Lexing.lexeme lexbuf)) }

and string = parse
 "\\\""  { Buffer.add_char string_buffer '"'; string lexbuf }
| "\\\\" { Buffer.add_char string_buffer '\\'; string lexbuf }
| '"'  { String (Buffer.contents string_buffer) }
| '\n' {
      let module L = Lexing in
      let pos = lexbuf.L.lex_curr_p in
      lexbuf.L.lex_curr_p <-
        { pos with
          L.pos_lnum = pos.L.pos_lnum + 1 ;
          pos_bol = pos.L.pos_cnum ;
        };
      Buffer.add_string string_buffer (Lexing.lexeme lexbuf);
      string lexbuf
    }
| _ { Buffer.add_string string_buffer (Lexing.lexeme lexbuf); string lexbuf }
| eof { error lexbuf "String not terminated." }

and comment = parse
| "*)"  {
    let s = Misc.strip_string (Buffer.contents string_buffer) in
    Comment s
    }
| '\n' {
      let module L = Lexing in
      let pos = lexbuf.L.lex_curr_p in
      lexbuf.L.lex_curr_p <-
        { pos with
          L.pos_lnum = pos.L.pos_lnum + 1 ;
          pos_bol = pos.L.pos_cnum ;
        };
      Buffer.add_string string_buffer (Lexing.lexeme lexbuf);
      comment lexbuf
    }
| _ { Buffer.add_string string_buffer (Lexing.lexeme lexbuf); comment lexbuf }
| eof { error lexbuf "Comment not terminated." }

