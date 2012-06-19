(** *)

let exec_one opts errors input =
  try
    let spec_dir = Filename.concat (Config.data_dir config) input in
    let spec = Ind_io.load spec_dir in
    ignore(Chn_inst.instanciate ctx uri spec c)
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
;;

let () = Misc.safe_main main;;