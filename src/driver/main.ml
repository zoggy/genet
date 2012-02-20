(** Main module of genet tool. *)

open Cmdline;;
open Options;;

let add_tool config wld options =
  match options.args with
  | [name] ->
      begin
        let uri = Grdf_tool.add_tool wld config.Config.uri_prefix name in
        print_endline uri
      end
  | _ -> failwith "Please give one tool name"
;;

let add_branch config wld options =
  match options.args with
  | [parent ; name] ->
      let uri = Grdf_branch.add wld parent name in
      print_endline uri
  | _ -> failwith "Please give parent uri and name of the new branch"
;;

let add_version config wld options =
  match options.args with
  | [tool ; name] ->
      let uri = Grdf_version.add wld ~tool name in
      print_endline uri
  | [tool ; parent ; name] ->
      let uri = Grdf_version.add wld ~tool ~parent name in
      print_endline uri
  | _ -> failwith "Please give tool uri, optional branch uri and name of the new version"
;;

let add_intf config wld options =
  match options.args with
  | [parent ; name] ->
      let uri = Grdf_intf.add wld ~parent name in
      print_endline uri
  | _ -> failwith "Please give tool or branch uri and name of the new interface"
;;

let add_filetype config wld options =
  match options.args with
  | [name ; extension ; desc] ->
      if String.length name <= 0 then failwith "Name must not be empty";
      let len_ext = String.length extension in
      if len_ext <= 0 then failwith "Extension must not be empty";
      let extension =
        match extension.[0] with
          '.' ->
            if len_ext <= 1 then
              failwith "Extension must not be empty (after removing heading '.')";
            String.sub extension 1 (len_ext - 1)
        | _ -> extension
      in
      let uri = Grdf_ftype.add wld config.Config.uri_prefix ~name ~desc ~extension in
      print_endline uri
  | _ -> failwith "Please give the name, file extension and description of the new filetype"
;;

(** {2 Command-line specification} *)

type mode =
  | Init
  | Serialize_rdf
  | Add_tool
  | Add_branch
  | Add_version
  | Add_intf
  | Add_filetype

let mode = ref None;;

let set_mode m () = mode := Some m;;

let com_add_tool = {
  com_options = [] ; com_usage = "<name>" ;
  com_kind = Final (set_mode Add_tool) ;
  }
;;

let com_add_branch = {
  com_options = [] ; com_usage = "<parent uri> <branch name>" ;
  com_kind = Final (set_mode Add_branch) ;
  }
;;

let com_add_version = {
  com_options = [] ; com_usage = "<tool uri> [<branch uri>] <name>" ;
  com_kind = Final (set_mode Add_version) ;
  }
;;

let com_add_intf = {
  com_options = [] ; com_usage = "<tool|branch uri> <name>" ;
  com_kind = Final (set_mode Add_intf) ;
  }
;;

let com_add_filetype = {
  com_options = [] ; com_usage = "<name> <extension> <description>" ;
  com_kind = Final (set_mode Add_filetype) ;
  }
;;

let add_commands = [
    "tool", com_add_tool, "add new tool" ;
    "branch", com_add_branch, "add new branch" ;
    "version", com_add_version, "add new version" ;
    "interface", com_add_intf, "add new interface" ;
    "filetype", com_add_filetype, "add new filetype" ;
  ]
;;

let com_add = {
  com_options = [] ; com_usage = "" ;
  com_kind = Commands add_commands ;
  }
;;

let com_serialize = {
  com_options = [ Options.option_ntriples ; Options.option_rdfxml ] ;
  com_usage = "" ; com_kind = Final (set_mode Serialize_rdf) ;
  }
;;

let com_init = {
  com_options = [] ; com_usage = "" ;
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
    try
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
      | Some Add_tool -> add_tool config rdf_wld opts
      | Some Add_branch -> add_branch config rdf_wld opts
      | Some Add_version -> add_version config rdf_wld opts
      | Some Add_intf -> add_intf config rdf_wld opts
      | Some Add_filetype -> add_filetype config rdf_wld opts
    with
  Grdf_types.Error e ->
        prerr_endline (Grdf_types.string_of_error e);
        exit 1
  end;
  Grdf_init.close rdf_wld
;;

let () = Misc.safe_main main;;
