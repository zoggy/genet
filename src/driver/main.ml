(** Main module of genet tool. *)

let options =
    Options.option_version "Genet" ::
    Options.option_config ::
    []
;;

let main () =
  let opts = Options.parse options in
  let config = Config.read_config opts.Options.config_file in
  print_string (Config.string_of_config config);
  let (_rdf_storage, _rdf_world) = Grdf_init.open_storage config in
  ()
;;

let () = Misc.safe_main main;;
