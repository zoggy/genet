(** Generate ocsigen configuration file from genet project configuration file. *)

let options = [];;
let main () =
  let opts = Options.parse options in
  let _config = Config.read_config opts.Options.config_file in
  let env = List.fold_left
    (fun env (name, value) -> Xtmpl.env_add_att name value env)
    Xtmpl.env_empty
    []
  in
  let contents = Xtmpl.apply_from_file env Install.ocsigen_conf_xtmpl in
  print_endline contents
;;

let () = Misc.safe_main main;;