(*********************************************************************************)
(*                Genet                                                          *)
(*                                                                               *)
(*    Copyright (C) 2012-2013 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License version 3             *)
(*    or later as published by the Free Software Foundation.                     *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software Foundation,    *)
(*    Inc., 59 Temple Place, Suite 330, Boston, MA                               *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*                                                                               *)
(*********************************************************************************)

(** Edition operations for the command line tool. *)

open Cmdline;;
open Options;;

let add_tool config wld options =
  match options.args with
  | [name] ->
      begin
        let iri = Grdf_tool.add_tool wld name in
        print_endline (Rdf_iri.string iri)
      end
  | _ -> failwith "Please give one and only one tool name"
;;
let com_add_tool = {
  com_options = [] ; com_usage = "<name>" ;
  com_compl = [] ;
  com_kind = Main_cmd.mk_final_fun add_tool ;
  }
;;

let add_branch config wld options =
  match options.args with
  | [parent ; name] ->
      let iri = Grdf_branch.add wld (Rdf_iri.iri parent) name in
      print_endline (Rdf_iri.string iri)
  | _ -> failwith "Please give parent iri and name of the new branch"
;;
let com_add_branch = {
  com_options = [] ; com_usage = "<parent iri> <branch name>" ;
  com_compl = [ Cmdline.Compfun Main_cmd.compl_tool_or_branch ] ;
  com_kind = Main_cmd.mk_final_fun add_branch ;
  }
;;

let add_version config wld options =
  match options.args with
  | [parent ; name] ->
      let parent = Rdf_iri.iri parent in
      let iri = Grdf_version.add wld ~parent name in
      print_endline (Rdf_iri.string iri)
  | _ -> failwith "Please give tool or branch iri and name of the new version"
;;
let com_add_version = {
  com_options = [] ; com_usage = "<(tool|branch) iri> <name>" ;
  com_compl = [
      Cmdline.Compfun Main_cmd.compl_tool_or_branch ;
    ] ;
  com_kind = Main_cmd.mk_final_fun add_version ;
  }
;;

let intf_path = ref None;;
let intf_tools = ref [];;

let add_intf config wld options =
  let tools = !intf_tools in
  match options.args with
  | [parent ; name] ->
      let parent = Rdf_iri.iri parent in
      let iri = Grdf_intf.add wld ~parent name in
      (match !intf_path with
         None -> ()
       | Some p -> Grdf_intf.set_command_path wld iri p
      );
      let f_tool tool =
        let iri_tool = Grdfs.iri_tool config.Config.rest_api tool in
        match Grdf_tool.tool_exists wld iri_tool with
          None -> failwith (Printf.sprintf "Unknown tool %S" tool)
        | _ -> Grdfs.add_triple_iris wld
          ~sub: iri ~pred: Grdfs.genet_usetool ~obj: iri_tool
      in
      List.iter f_tool tools;
      print_endline (Rdf_iri.string iri)
  | _ -> failwith "Please give tool or branch iri and name of the new interface"
;;

let com_add_intf = {
  com_options = [
      "-p", Cmdline.String (None, fun s -> intf_path := Some s),
      "<path> path to command (when used, %v will be replaced by version)" ;

      "-t", Cmdline.String (Some Main_cmd.compl_tool_name, fun s -> intf_tools := !intf_tools @ [s]),
      "<toolname> make interface depend on tool\n\t (when used, %{version-name>} will be replaced by version of tool)" ;
    ];
  com_usage = "<tool|branch iri> <name>" ;
  com_compl = [ Cmdline.Compfun Main_cmd.compl_tool_or_branch ] ;
  com_kind = Main_cmd.mk_final_fun add_intf ;
  }
;;

let add_no_intf config wld options =
  match options.args with
  | [parent; intf] ->
      let parent = Rdf_iri.iri parent in
      let intf = Rdf_iri.iri intf in
      Grdf_intf.add_no_intf wld ~parent intf
  | _ -> failwith "Please give (branch|version) iri and interface iri"
;;

let com_add_no_intf = {
  com_options = [];
  com_usage = "<branch|version iri> <interface iri>" ;
  com_compl = [
      Cmdline.Compfun Main_cmd.compl_branch_or_version ;
      Cmdline.Compfun Main_cmd.compl_intf ;
    ] ;
  com_kind = Main_cmd.mk_final_fun add_no_intf ;
  }
;;

let force_add_diffcommand = ref false;;
let add_diffcommand config wld options =
  match options.args with
  | [name ; path] ->
      let name =
        try Grdf_diff.parse_diffcommand_ident name
        with _ -> failwith (Printf.sprintf "Invalid diff command name %S" name)
      in
      let iri = Grdf_diff.add config.Config.rest_api
        wld ~force: !force_add_diffcommand ~name ~path
      in
      print_endline (Rdf_iri.string iri)
  | _ -> failwith "Please give a name and a command"
;;

let com_add_diffcommand = {
  com_options = [
      "--force", Cmdline.Set force_add_diffcommand,
      " if a diff command with same name exists, replace the associated command";
    ];
  com_usage = "<name> <command>" ;
  com_compl = [ ] ;
  com_kind = Main_cmd.mk_final_fun add_diffcommand ;
  }
;;

let add_filetype config wld options =
  match options.args with
  | [name ; extension ; desc] ->
      if String.length name <= 0 then failwith "Name must not be empty";
      let name =
        try Grdf_ftype.parse_filetype_ident name
        with _ -> failwith (Printf.sprintf "Invalid filetype name %S" name)
      in
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
      let iri = Grdf_ftype.add wld ~name ~desc ~extension in
      print_endline (Rdf_iri.string iri)
  | _ -> failwith "Please give the name, file extension and description of the new filetype"
;;
let com_add_filetype = {
  com_options = [] ; com_usage = "<name> <extension> <description>" ;
  com_compl = [] ;
  com_kind = Main_cmd.mk_final_fun add_filetype ;
  }
;;

let port_position = ref None;;

let add_port config wld options =
  let pos = !port_position in
  match options.args with
    s_intf :: s_dir :: ptype :: q ->
      let iri_intf = Rdf_iri.iri s_intf in
      let dir = Grdf_port.dir_of_string s_dir in
      let ptype =
        try Grdf_port.parse_port_type ptype
        with
        | Grdf_port.Invalid_type s ->
            failwith (Printf.sprintf "Invalid type: %s" s)
        | Grdf_port.Invalid_type_id s ->
            failwith (Printf.sprintf "Invalid type id: %s" s)
      in
      let name =
        match q with
          [] -> None
        | name :: _ ->
            try Some (Grdf_ftype.parse_filetype_ident name)
            with _ -> failwith (Printf.sprintf "Invalid port name %S" name)
      in
      if Grdf_intf.intf_exists wld iri_intf = None then
        failwith (Printf.sprintf "Unknown interface %S" (Rdf_iri.string iri_intf));
      begin
        match Grdf_port.port_file_type_iri config.Config.rest_api ptype with
          None -> ()
        | Some iri_ftype ->
            if Grdf_ftype.filetype_exists wld iri_ftype = None then
              failwith (Printf.sprintf "Unknown filetype %S" (Rdf_iri.string iri_ftype))
      end;

      Grdf_port.add_port wld iri_intf dir ?pos ?name ptype;
      let iri_port = Grdf_intf.get_port wld iri_intf ?pos dir in
      print_endline (Rdf_iri.string iri_port)
  | _ ->
      failwith
        "Please give at least the interface iri, \
         the direction (in|out) and the filetype name"
;;
let com_add_port = {
    com_options = [
      "-p", Cmdline.Int (None, fun n -> port_position := Some n),
      "<n> 1-based position to insert port at; default is to append" ;
    ];
    com_usage = "<interface iri> <in|out> <filetype-name> [port name]" ;
    com_compl = [
      Cmdline.Compfun Main_cmd.compl_intf ;
      Cmdline.Compfun Main_cmd.compl_in_out ;
      Cmdline.Compfun Main_cmd.compl_filetype_name ;
    ] ;
    com_kind = Main_cmd.mk_final_fun add_port ;
  }

let rem_port config wld options =
  match options.args with
    [] -> failwith "Please give at least one port iri"
  | ports ->
      List.iter (fun s -> Grdf_port.delete_port wld (Rdf_iri.iri s)) ports
;;
let com_rem_port = {
  com_options = [] ; com_usage = "<port iris>" ;
  com_compl = [ Cmdline.Complist Main_cmd.compl_port ] ;
  com_kind = Main_cmd.mk_final_fun rem_port ;
  }
;;

let add_input config _ options =
  match options.args with
    [dir] ->
      begin
        try
          let data_dir = Config.data_dir config in
          let fulldir = Fname.concat_s data_dir dir in
          Misc.mkdir ~verbose: (options.verb_level > 0) (Fname.abs_string fulldir);
          Ind_io.write fulldir
        with
          Ind_io.Error e ->
            failwith (Ind_io.string_of_error e)
      end
  | _ -> failwith "Please give one and only one input name"
;;
let com_add_input = {
  com_options = [] ; com_usage = "<name>" ;
  com_compl = [] ;
  com_kind = Main_cmd.mk_final_fun add_input ;
  }
;;

let add_ref_inst config wld options =
  match options.args with
    [input ; chainname ; inst_iri] ->
      begin
        let input = Fname.relative input in
        let chain = Chn_types.chain_name_of_string chainname in
        let chain = Chn_types.iri_chain config.Config.rest_api chain in
        let inst = Rdf_iri.iri inst_iri in
        let ctx =  { Chn_types.ctx_rdf = wld ; ctx_cfg = config ; ctx_user = None } in
        Chn_inst.add_reference_inst ctx ~input ~chain ~inst
      end
  | _ -> failwith "Please give one input name, one chain fullname and one inst chain url"
;;
let com_add_refinst = {
    com_options = [] ; com_usage = "<input name> <chain fullname> <inst chain url>" ;
    com_compl = [
      Cmdline.Compfun Main_cmd.compl_input_name ;
      Cmdline.Compfun Main_cmd.compl_chain_name ;
      Cmdline.Compfun Main_cmd.compl_ichain ;
    ] ;
    com_kind = Main_cmd.mk_final_fun add_ref_inst ;
  }
;;

let set_active config wld options =
  let (url, active) =
    match options.args with
      [ url_version ] -> (url_version, true)
    | [ url_version ; "true" ] -> (url_version, true)
    | [ url_version ; "false" ] -> (url_version, false)
    | _ ->
      let msg = Printf.sprintf "Usage: %s set active <url of version> [true|false]" Sys.argv.(0) in
      failwith msg
  in
  let iri = Rdf_iri.iri url in
  match Grdf_version.version_exists wld iri with
    None -> failwith (Printf.sprintf "Unknown version %S" (Rdf_iri.string iri))
  | Some _ -> Grdfs.set_is_active_iri wld iri active
;;
let com_set_active = {
  com_options = [] ; com_usage = "<url of tool version> [true|false]" ;
  com_compl = [
      Cmdline.Compfun Main_cmd.compl_version ;
      Cmdline.Compfun Main_cmd.compl_bool ;
    ] ;
  com_kind = Main_cmd.mk_final_fun set_active ;
  }
;;

let add_commands = [
    "tool", com_add_tool, "add new tool" ;
    "branch", com_add_branch, "add new branch" ;
    "version", com_add_version, "add new version" ;
    "interface", com_add_intf, "add new interface" ;
    "no-interface", com_add_no_intf, "indicate a branch or version does not implement a given interface" ;
    "filetype", com_add_filetype, "add new filetype" ;
    "port", com_add_port, "add new port" ;
    "input", com_add_input, "add new input";
    "refinst", com_add_refinst, "set an instanciated chain as reference";
    "diff-command", com_add_diffcommand, "add a predefined diff command";
  ]
;;
let com_add = {
    com_options = [] ; com_usage = "" ;
    com_compl = [] ;
    com_kind = Commands add_commands ;
  }
;;

let set_commands = [
  "active", com_set_active, "set a tool version as active or not"
  ]
;;

let com_set = {
  com_options = [] ; com_usage = "" ;
  com_compl = [] ;
  com_kind = Commands set_commands ;
}

let remove_commands = [
    "port", com_rem_port, "remove port" ;
  ]
;;

let com_remove = {
  com_options = [] ; com_usage = "" ;
  com_compl = [] ;
  com_kind = Commands remove_commands ;
  }
;;

List.iter
  (fun (name, com, desc) ->
     Main_cmd.register_subcommand name com desc
  )
    [ "add", com_add, "add elements to RDF model" ;
      "remove", com_remove, "remove elements from rdf model" ;
      "set", com_set, "set flags" ;
    ]
;;
