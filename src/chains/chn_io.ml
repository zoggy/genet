(** Reading and writing chain ASTs. *)

let modname_of_file f =
  Chn_types.chain_modname_of_string
  (String.capitalize (Filename.basename (Filename.chop_extension f)))
;;

let file_of_modname config name =
  let dir = Config.chains_dir config in
  let name = String.uncapitalize (Chn_types.string_of_chain_modname name) in
  Filename.concat dir (Printf.sprintf "%s.gnt" name)
;;

let input_chn_module name lexbuf =
  try
    let l = Chn_parser.ast Chn_lexer.main lexbuf in
    { Chn_ast.cmod_name = name ;
      Chn_ast.cmod_chains = l ;
    }
  with
    Chn_parser.Error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      let loc = { Loc.loc_start = pos ; Loc.loc_end = pos } in
      Loc.raise_problem loc "Syntax error"
;;

let chn_module_of_file file =
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  let lexbuf =
    { lexbuf with
      Lexing.lex_curr_p = { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = file }
    }
  in
  let modname = modname_of_file file in
  Misc.try_finalize (input_chn_module modname) lexbuf close_in ic
;;

let print_chn_module oc m =
  output_string oc ((Chn_ast.printer ())#string_of_chn_module m)
;;

let chain_files config =
  let dir = Config.chains_dir config in
  Find.find_list Find.Stderr [dir]
  [ Find.Maxdepth 1 ; Find.Type Unix.S_REG ;
    Find.Regexp (Str.regexp ".*\\.gnt$") ]
;;

let load_chain_files config =
  let f (cmods, errors) file =
    try
      let cmod = chn_module_of_file file in
      (cmod :: cmods, errors)
    with
      e ->
        let msg =
          match e with
            Sys_error s | Failure s -> s
          | Loc.Problem pb -> Loc.string_of_problem pb
          | _ -> raise e
        in
        (cmods, msg :: errors)
  in
  List.fold_left f ([], []) (chain_files config)
;;

  