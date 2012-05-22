(** Main module of the [genet-chain] tool. *)

open Chn_ast;;

let gen_chain_dot file chain =
  let dot = (new Chn_ast.chain_dot_printer)#dot_of_chain chain in
  Misc.file_of_string ~file: file dot
;;

let gen_fchain_dot ctx file fchain =
  let dot = (new Chn_flat.fchain_dot_printer)#dot_of_fchain ctx fchain in
  Misc.file_of_string ~file: file dot
;;



let test_file ?dot file =
  try
    let cmod = Chn_io.chn_module_of_file file in
    Chn_io.print_chn_module stdout cmod;
    begin
      match dot with
        None -> ()
      | Some prefix ->
          let f chn =
            let outfile = Printf.sprintf "%s%s.%s.dot"
              prefix
              (String.capitalize (Filename.basename file))
              (Chn_types.string_of_chain_basename chn.chn_name)
            in
            gen_chain_dot outfile chn
          in
          List.iter f cmod.cmod_chains
    end;
    true
  with
    Loc.Problem pb ->
      prerr_endline (Loc.string_of_problem pb);
      false
;;

let flatten opts ?dot acc s =
  try
    let config = Config.read_config opts.Options.config_file in
    let rdf_wld = Grdf_init.open_graph config in
    let ctx = { Chn_types.ctx_rdf = rdf_wld ; ctx_cfg = config ; ctx_user = None } in
    let fullname = Chn_types.chain_name_of_string s in
    let uri = Chn_flat.flatten ctx fullname in
    print_endline (Printf.sprintf "%s => %s" s (Rdf_uri.string uri));
    begin match dot with None -> () | Some file -> gen_fchain_dot ctx file uri end;
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

let string_of_comb ctx comb =
  let f_version (tool, version) =
    Printf.sprintf "%s %s"
      (Grdf_tool.name ctx.Chn_types.ctx_rdf tool)
      (Grdf_version.name ctx.Chn_types.ctx_rdf version)
  in
  String.concat ", " (List.map f_version comb)
;;

let show_combinations opts acc s =
  let uri = Rdf_uri.uri s in
  let config = Config.read_config opts.Options.config_file in
  let rdf_wld = Grdf_init.open_graph config in
  let ctx = { Chn_types.ctx_rdf = rdf_wld ; ctx_cfg = config ; ctx_user = None } in
  let combs = Chn_inst.version_combinations ctx uri in
  print_endline (Printf.sprintf "combinations for %s:" (Rdf_uri.string uri));
  List.iter (fun c -> print_endline (string_of_comb ctx c)) combs;
  acc
;;

let dot = ref None;;
type action =
  | Test of string
  | Flatten of string
  | Show_combs of string
;;
let actions = ref [];;

let do_action opts acc = function
  Test file -> if test_file ?dot: !dot file then acc else acc + 1
| Flatten s -> flatten opts ?dot: !dot acc s
| Show_combs s -> show_combinations opts acc s
;;

let options =
  Options.option_config ::
  [
    "--dot", Arg.String (fun file -> dot := Some file), " generate dot files" ;
    "-t", Arg.String (fun s -> actions := Test s :: !actions), "<file> test the given file" ;
    "-f", Arg.String (fun s -> actions := Flatten s :: !actions), "<Mod.chain> flatten the given chain" ;

    "--show-combs", Arg.String (fun s -> actions := Show_combs s :: !actions),
    "<flat chain uri> show tool combinations usable to instanciate this chain";
  ]
;;


let main () =
  let opts = Options.parse options in
  let n = List.fold_left (do_action opts) 0 (List.rev !actions) in
  flush stdout;
  if n > 0 then
    prerr_endline (Printf.sprintf "%d problem(s) encountered" n);
  exit n
;;

let () = Misc.safe_main main;;
