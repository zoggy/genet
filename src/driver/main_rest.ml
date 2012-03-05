(** Main module of the REST web API. *)

let options =
  Options.option_version "Genet-rest" ::
  Options.option_config ::
  Options.option_verbose ::
  []
;;

let main () =
  let opts = Options.parse options in
  let config = Config.read_config opts.Options.config_file in
  let rdf_wld = Grdf_init.open_storage config in
  ignore(rdf_wld)
;;

let () = Misc.safe_main main;;

