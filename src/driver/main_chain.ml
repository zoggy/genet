(** Main module of the [genet-chain] tool. *)

let test_file file =
  try
    let ast = Chn_io.ast_of_file file in
    Chn_io.print_ast stdout ast;
    true
  with
    Loc.Problem pb ->
      prerr_endline (Loc.string_of_problem pb);
      false
;;

let main () =
  let opts = Options.parse [] in
  let n = List.fold_left
    (fun acc file -> if test_file file then acc else acc + 1)
    0 opts.Options.args
  in
  flush stdout;
  if n > 0 then
    prerr_endline (Printf.sprintf "%d problem(s) encountered" n);
  exit n
;;

let () = Misc.safe_main main;;