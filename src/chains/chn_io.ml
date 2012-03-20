(** Reading and writing chain ASTs. *)

let input_ast lexbuf =
  try Chn_parser.ast Chn_lexer.main lexbuf
  with
    Chn_parser.Error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      let loc = { Loc.loc_start = pos ; Loc.loc_end = pos } in
      Loc.raise_problem loc "Syntax error"
;;

let ast_of_file file =
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  let lexbuf =
    { lexbuf with
      Lexing.lex_curr_p = { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = file }
    }
  in
  Misc.try_finalize input_ast lexbuf close_in ic
;;

let print_ast oc ast =
  output_string oc ((Chn_ast.printer ())#string_of_ast ast)
;;

let chain_files config =
  let dir = Config.chains_dir config in
  Find.find_list Find.Stderr [dir]
  [ Find.Maxdepth 1 ; Find.Type Unix.S_REG ;
    Find.Regexp (Str.regexp ".*\\.gnt$") ]
;;