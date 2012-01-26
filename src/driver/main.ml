(** Main module of genet tool. *)

open Cmdline;;

type mode =
  | Init
  | Serialize_rdf
  | Add_tool
  | Add_branch

let mode = ref None;;

let set_mode m () = mode := Some m;;

let com_add_tool = {
  com_options = [] ;
  com_usage = "<name>" ;
  com_kind = Final (set_mode Add_tool) ;
  }
;;

let com_add_branch = {
  com_options = [] ;
  com_usage = "<parent uri> <branch name>" ;
  com_kind = Final (set_mode Add_branch) ;
  }
;;

let add_commands = [
    "tool", com_add_tool, "add new tool" ;
    "branch", com_add_branch, "add new branch" ;
  ]
;;

let com_add = {
  com_options = [] ;
  com_usage = "" ;
  com_kind = Commands add_commands ;
  }
;;

let com_serialize = {
  com_options = [ Options.option_ntriples ; Options.option_rdfxml ] ;
  com_usage = "" ;
  com_kind = Final (set_mode Serialize_rdf) ;
  }
;;

let com_init = {
  com_options = [] ;
  com_usage = "" ;
  com_kind = Final (set_mode Init) ;
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
    "add", com_add, "add elements to rdf model" ;
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
        begin
          match Rdf_model.to_string rdf_wld.Grdf_types.wld_model ~name: "turtle" with
            None -> failwith "Failed to serialize model"
          | Some string -> print_string string
        end
    | Some Add_tool
    | Some Add_branch -> failwith "not implemened"

  end;
  Grdf_init.close rdf_wld
;;

let () = Misc.safe_main main;;
