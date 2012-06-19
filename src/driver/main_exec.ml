(** *)

let verbose opts ?(level=1) msg =
  if opts.Options.verb_level >= level then
    prerr_endline msg
;;

let exec_one opts errors input =
  verbose opts (Printf.sprintf "Handling input %S" input);
  try
    let config = Config.read_config opts.Options.config_file in
    let rdf_wld = Grdf_init.open_graph config in
    let ctx = { Chn_types.ctx_rdf = rdf_wld ; ctx_cfg = config ; ctx_user = None } in
    let spec_dir = Filename.concat (Config.data_dir config) input in
    let spec = Ind_io.load spec_dir in
    Chn_exec.exec ctx spec;
    errors
  with
    exc ->
      begin
        match exc with
          Ind_io.Error e ->
            prerr_endline
            (Printf.sprintf "Input %s: %s" input
             (Ind_io.string_of_error e)
            );
        | Failure s -> prerr_endline s
        | e -> prerr_endline (Printexc.to_string e)
      end;
      errors + 1
;;

let options =
  Options.option_config ::
  Options.option_verbose ::
  [
  ]
;;

let main () =
  let opts = Options.parse options in
  match opts.Options.args with
    [] -> failwith "Please give the name of one input"
  | inputs ->
      let errors = List.fold_left (exec_one opts) 0 inputs in
      if errors > 0 then
        prerr_endline
        (Printf.sprintf "%d error%s" errors (if errors > 1 then "s" else ""));
      exit errors
;;

let () = Misc.safe_main main;;