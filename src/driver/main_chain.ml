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

let flatten opts acc s =
  try
    let config = Config.read_config opts.Options.config_file in
    let rdf_wld = Grdf_init.open_storage config in
    let ctx = { Chn_types.ctx_rdf = rdf_wld ; ctx_cfg = config ; ctx_user = None } in
    let fullname = Chn_types.chain_name_of_string s in
    ignore(Chn_flat.flatten ctx fullname);
    acc
  with
    e ->
      let msg =
        match e with
          Failure s | Sys_error s ->  s
        | Loc.Problem pb -> Loc.string_of_problem pb
        | e -> Printexc.to_string e
      in
      let msg = Printf.sprintf "flatten %s: %s" s msg in
      prerr_endline msg;
      acc + 1
;;

let dot = ref false;;
type action = Test of string | Flatten of string;;
let actions = ref [];;

let do_action opts acc = function
  Test file -> if test_file ~dot: !dot file then acc else acc + 1
| Flatten s -> flatten opts acc s
;;

let options =
  Options.option_config ::
  [
    "--dot", Arg.Set dot, " generate dot files when testing a file" ;
    "-t", Arg.String (fun s -> actions := Test s :: !actions), "<file> test the given file" ;
    "-f", Arg.String (fun s -> actions := Flatten s :: !actions), "<Mod.chain> flatten the given chain" ;
  ]
;;


let main () =
  let opts = Options.parse options in
  let n = List.fold_left (do_action opts) 0 !actions in
  flush stdout;
  if n > 0 then
    prerr_endline (Printf.sprintf "%d problem(s) encountered" n);
  exit n
;;

let () = Misc.safe_main main;;