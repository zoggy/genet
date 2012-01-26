(** Main module of genet tool. *)

open Cmdline;;

type mode =
  | Init
  | Serialize_rdf

let mode = ref None;;

let com_serialize = {
  com_options = [ Options.option_ntriples ; Options.option_rdfxml ] ;
  com_usage = "" ;
  com_kind = Final (fun () -> mode := Some Serialize_rdf) ;
  }
;;

let com_init = {
  com_options = [] ;
  com_usage = "" ;
  com_kind = Final (fun () -> mode := Some Init)
  }
;;


let common_options =
    Options.option_version "Genet" ::
    Options.option_config ::
    []
;;

let commands = [
    "init", com_init, "init directory and database" ;
    "serialize-rdf", com_serialize, "print rdf model" ;
  ];;

let command = {
  com_options = common_options ;
  com_usage = "[arguments]" ;
  com_kind = Commands commands
  }

let main () =
  let opts = Options.parse_command command in
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
