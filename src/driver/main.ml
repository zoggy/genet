(** Main module of genet tool. *)

type mode =
  | Init
  | Serialize_rdf

let mode = ref None;;

let options =
    Options.option_version "Genet" ::
    Options.option_config ::
    ("--init", Arg.Unit (fun () -> mode := Some Init), " init directory and database") ::
    ("--serialize-rdf", Arg.Unit (fun () -> mode := Some Serialize_rdf), " print rdf model") ::
    []
;;

let main () =
  let opts = Options.parse options in
  let config = Config.read_config opts.Options.config_file in
  prerr_endline (Config.string_of_config config);
  let rdf_wld = Grdf_init.open_storage config in
  begin
    match !mode with
      None -> ()
    | Some Init ->
        Grdf_init.init rdf_wld config.Config.uri_prefix
    | Some Serialize_rdf ->
        match Rdf_model.to_string rdf_wld.Grdf_types.wld_model ~name: "turtle" with
          None -> failwith "Failed to serialize model"
        | Some string -> print_string string
  end;
  Grdf_init.close rdf_wld
;;

let () = Misc.safe_main main;;
