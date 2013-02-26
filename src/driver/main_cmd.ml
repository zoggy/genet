(** Common stuff for command line tool. *)

open Cmdline;;

let subcommands = ref ([] : (string * Cmdline.command * string) list);;

let register_subcommand name com desc =
  subcommands := (name, com, desc) :: !subcommands
;;

let subcommands () = !subcommands;;

let final_fun = ref (None: (Options.option_values -> unit) option);;
let set_final_fun f = final_fun := Some f;;

let mk_final_fun f =
  let g opts =
    let config = Config.read_config opts.Options.config_file in
    let rdf_wld = Grdf_init.open_graph config in
    f config rdf_wld opts
  in
  Cmdline.Final (fun () -> set_final_fun g);;

let call_final_fun opts =
  match !final_fun with
    None -> failwith "No final fun set!"
  | Some f -> f opts
;;

let mk_ctx_fun f config rdf_wld opts =
  let ctx = { Chn_types.ctx_rdf = rdf_wld ; ctx_cfg = config ; ctx_user = None} in
  f ctx opts
;;

let compl_file () = Cmdline.Files ([], None);;
let compl_toolname () = Cmdline.Choices [];;

let compl_tool () =
  Cmdline.Choices
    (try Misc.split_string (Misc.exec_command "genet query tools") ['\n']
     with _ -> [])
;;

let compl_branch () =
  Cmdline.Choices
    (try Misc.split_string (Misc.exec_command "genet query branches") ['\n']
     with _ -> [])
;;

let compl_version () =
  Cmdline.Choices
    (try Misc.split_string (Misc.exec_command "genet query branches") ['\n']
     with _ -> [])
;;

let compl_intf () =
  Cmdline.Choices
    (try Misc.split_string (Misc.exec_command "genet query interfaces") ['\n']
     with _ -> [])
;;

let compl_filetype () =
  Cmdline.Choices
    (try Misc.split_string (Misc.exec_command "genet query filetypes") ['\n']
     with _ -> [])
;;

let compl_port () = Choices ["http://"]
(*
  Cmdline.Choices
    (try Misc.split_string (Misc.exec_command "genet query ports") ['\n']
     with _ -> [])
*)

let compl_ichain () = Cmdline.Choices ["http://"];;
let compl_tool_or_branch () = Cmdline.Choices ["http://"];;
let compl_intf_provider () = Cmdline.Choices ["http://"];;
let compl_file_uri () = Cmdline.Choices ["http://"];;
let compl_input_name () = Cmdline.Choices [];;
let compl_chain_name () = Cmdline.Choices [];;
let compl_fchain () = Cmdline.Choices ["http://"];;
let compl_in_out () = Cmdline.Choices  ["in" ; "out"];;
let compl_bool () = Cmdline.Choices ["true" ; "false"];;
