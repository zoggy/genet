(** Main module of genet tool. *)

open Cmdline;;
open Options;;

let add_tool config wld options =
  match options.args with
  | [name] ->
      begin
        let uri = Grdf_tool.add_tool wld name in
        print_endline (Rdf_uri.string uri)
      end
  | _ -> failwith "Please give one and only one tool name"
;;

let add_branch config wld options =
  match options.args with
  | [parent ; name] ->
      let uri = Grdf_branch.add wld (Rdf_uri.uri parent) name in
      print_endline (Rdf_uri.string uri)
  | _ -> failwith "Please give parent uri and name of the new branch"
;;

let add_version config wld options =
  match options.args with
  | [tool ; name] ->
      let tool = Rdf_uri.uri tool in
      let uri = Grdf_version.add wld ~tool name in
      print_endline (Rdf_uri.string uri)
  | [tool ; parent ; name] ->
      let tool = Rdf_uri.uri tool in
      let parent = Rdf_uri.uri parent in
      let uri = Grdf_version.add wld ~tool ~parent name in
      print_endline (Rdf_uri.string uri)
  | _ -> failwith "Please give tool uri, optional branch uri and name of the new version"
;;

let add_intf config wld options =
  match options.args with
  | [parent ; name] ->
      let parent = Rdf_uri.uri parent in
      let uri = Grdf_intf.add wld ~parent name in
      print_endline (Rdf_uri.string uri)
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
      let uri = Grdf_ftype.add wld ~name ~desc ~extension in
      print_endline (Rdf_uri.string uri)
  | _ -> failwith "Please give the name, file extension and description of the new filetype"
;;

let add_port config wld ?pos options =
  match options.args with
    s_intf :: s_dir :: ptype :: q ->
      let uri_intf = Rdf_uri.uri s_intf in
      let dir = Grdf_port.dir_of_string s_dir in
      let ptype =
        try Grdf_port.parse_port_type ptype
        with
        | Grdf_port.Invalid_type s ->
            failwith (Printf.sprintf "Invalid type: %s" s)
        | Grdf_port.Invalid_type_id s ->
            failwith (Printf.sprintf "Invalid type id: %s" s)
      in
      let name = match q with [] -> None | s :: _ -> Some s in
      if Grdf_intf.intf_exists wld uri_intf = None then
        failwith (Printf.sprintf "Unknown interface %S" (Rdf_uri.string uri_intf));
      begin
        match Grdf_port.port_file_type_uri config.Config.rest_api ptype with
          None -> ()
        | Some uri_ftype ->
            if Grdf_ftype.filetype_exists wld uri_ftype = None then
              failwith (Printf.sprintf "Unknown filetype %S" (Rdf_uri.string uri_ftype))
      end;

      Grdf_port.add_port wld uri_intf dir ?pos ?name ptype;
      let uri_port = Grdf_intf.get_port wld uri_intf ?pos dir in
      print_endline (Rdf_uri.string uri_port)
  | _ ->
      let msg =
      "Please give at least the interface uri, the direction (in|out) and the filetype name"
      in
      failwith msg
;;

let rem_port config wld options =
  match options.args with
    [] -> failwith "Please give at least one port uri"
  | ports ->
      List.iter (fun s -> Grdf_port.delete_port wld (Rdf_uri.uri s)) ports
;;

let add_input config options =
  match options.args with
    [dir] ->
      begin
        try
          let data_dir = Config.data_dir config in
          let fulldir = Filename.concat data_dir dir in
          Misc.mkdir ~verbose: (options.verb_level > 0) fulldir;
          Ind_io.write fulldir
        with
          Ind_io.Error e ->
            failwith (Ind_io.string_of_error e)
      end
  | _ -> failwith "Please give one and only one input name"
;;

(** {2 Command-line specification} *)

type mode =
  | Init_dir
  | Init_db
  | Serialize_rdf
  | Add_tool
  | Add_branch
  | Add_version
  | Add_intf
  | Add_filetype
  | Add_port
  | Rem_port
  | Add_input
;;

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

let com_rem_port = {
  com_options = [] ; com_usage = "<port uris>" ;
  com_kind = Final (set_mode Rem_port) ;
  }
;;

let port_position = ref None;;
let com_add_port = {
    com_options = [
      "-p", Arg.Int (fun n -> port_position := Some n),
      "<n> 1-based position to insert port at; default is to append" ;
    ];
    com_usage = "<interface uri> <in|out> <filetype-name> [port name]" ;
    com_kind = Final (set_mode Add_port) ;
  }
;;

let com_add_input = {
  com_options = [] ; com_usage = "<name>" ;
  com_kind = Final (set_mode Add_input) ;
  }
;;
let add_commands = [
    "tool", com_add_tool, "add new tool" ;
    "branch", com_add_branch, "add new branch" ;
    "version", com_add_version, "add new version" ;
    "interface", com_add_intf, "add new interface" ;
    "filetype", com_add_filetype, "add new filetype" ;
    "port", com_add_port, "add new port" ;
    "input", com_add_input, "add new input";
  ]
;;

let com_add = {
  com_options = [] ; com_usage = "" ;
  com_kind = Commands add_commands ;
  }
;;


let remove_commands = [
    "port", com_rem_port, "remove port" ;
  ]
;;

let com_remove = {
  com_options = [] ; com_usage = "" ;
  com_kind = Commands remove_commands ;
  }
;;

let com_serialize = {
  com_options = [ Options.option_ntriples ; Options.option_rdfxml ] ;
  com_usage = "" ; com_kind = Final (set_mode Serialize_rdf) ;
  }
;;

let git_repo = ref None;;
let com_init_dir = {
    com_options = [
      "--git", Arg.String (fun s -> git_repo := Some s),
      "<repo> will create the 'in' directory by cloning the repository" ;
    ] ;
    com_usage = "[<directory>]" ;
    com_kind = Final (set_mode Init_dir) ;
  }
;;

let com_init_db = {
  com_options = [] ; com_usage = "" ;
  com_kind = Final (set_mode Init_db) ;
  }
;;

let common_options =
  Options.option_version "Genet" ::
  Options.option_config ::
  Options.option_verbose ::
  []
;;

let commands = [
    "init-dir", com_init_dir, "init directory" ;
    "init-db", com_init_db, "init database" ;
    "serialize-rdf", com_serialize, "print rdf model" ;
    "add", com_add, "add elements to rdf model" ;
    "remove", com_remove, "remove elements from rdf model" ;
  ];;

let command = {
  com_options = common_options ;
  com_usage = "[arguments]" ;
  com_kind = Commands commands
  }

let init_dir ?git_repo opts =
  let dir =
    match opts.Options.args with
      [] -> Filename.current_dir_name
    | dir :: _ -> dir
  in
  let verbose = opts.Options.verb_level > 0 in
  let mkdir = Misc.mkdir ~verbose in
  mkdir dir;
  let config_file = Install.default_config_file in
  let config = Config.read_config config_file in
  mkdir (Config.out_dir config);
  let in_dir = Config.in_dir config in
  begin
    match git_repo with
      None ->
        List.iter mkdir [Config.chains_dir config; Config.data_dir config];
        let web_dir = Config.web_dir config in
        begin
          let com = Printf.sprintf "cp -r %s %s"
            (Filename.quote Install.share_web_dir)
            (Filename.quote web_dir)
          in
          if verbose then
            print_endline
            (Printf.sprintf "copying %s to %s"
             Install.share_web_dir web_dir);
          match Sys.command com with
            0 -> ()
          | _ -> failwith (Printf.sprintf "Command failed: %s" com)
        end
    | Some repo ->
        let com = Printf.sprintf "git clone %s %s"
          (Filename.quote repo) (Filename.quote in_dir)
        in
        if verbose then
          print_endline (Printf.sprintf "Cloning %s into %s" repo in_dir);
        match Sys.command com with
          0 -> ()
        | _ -> failwith (Printf.sprintf "Command failed: %s" com)
  end;
;;

let main () =
  let opts = Options.parse_command command in
  match !mode with
    None -> ()
  | Some Init_dir -> init_dir ?git_repo: !git_repo opts
  | Some Add_input ->
      let config = Config.read_config opts.Options.config_file in
      add_input config opts
  | Some mode ->
      let config = Config.read_config opts.Options.config_file in
      (*prerr_endline (Config.string_of_config config);*)
      let rdf_wld = Grdf_init.open_graph config in
      begin
        try
          match mode with
          | Init_db -> ()
          | Serialize_rdf -> failwith "Serialization not implemented"
(*
              begin
                match Rdf_model.to_string rdf_wld.Grdf_types.wld_model
                  ~name: opts.Options.rdf_output_format
                with
                  None -> failwith "Failed to serialize model"
                | Some string -> print_string string
              end
*)
          | Add_tool -> add_tool config rdf_wld opts
          | Add_branch -> add_branch config rdf_wld opts
          | Add_version -> add_version config rdf_wld opts
          | Add_intf -> add_intf config rdf_wld opts
          | Add_filetype -> add_filetype config rdf_wld opts
          | Add_port -> add_port config rdf_wld ?pos: !port_position opts
          | Rem_port -> rem_port config rdf_wld opts
          | Init_dir
          | Add_input -> assert false
        with
          Grdf_types.Error e ->
            prerr_endline (Grdf_types.string_of_error e);
            exit 1
      end
;;

let () = Misc.safe_main main;;
