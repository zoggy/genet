(** Main module of the [genet-chain] tool. *)

open Chn_ast;;

let gen_dot file chain =
  let outfile = Printf.sprintf "%s.%s.dot"
    (String.capitalize (Filename.basename file))
    (Chn_types.string_of_chain_basename chain.chn_name)
  in
  let dot = Chn_ast.Dot.dot_of_chain chain in
  Misc.file_of_string ~file: outfile dot
;;

let test_file ?(dot=false) file =
  try
    let cmod = Chn_io.chn_module_of_file file in
    Chn_io.print_chn_module stdout cmod;
    if dot then List.iter (gen_dot file) cmod.cmod_chains;
    true
  with
    Loc.Problem pb ->
      prerr_endline (Loc.string_of_problem pb);
      false
;;

let dot = ref false;;

let options = [
    "--dot", Arg.Set dot, " create dot dumps" ;
  ]
;;

let main () =
  let opts = Options.parse options in
  let n = List.fold_left
    (fun acc file -> if test_file ~dot: !dot file then acc else acc + 1)
    0 opts.Options.args
  in
  flush stdout;
  if n > 0 then
    prerr_endline (Printf.sprintf "%d problem(s) encountered" n);
  exit n
;;

let () = Misc.safe_main main;;