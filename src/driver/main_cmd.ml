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

let compl_file () = Cmdline.compl_choices ~files: true ();;
let compl_tool_name () = Cmdline.compl_choices ();;

let compl_tool () =
  Cmdline.compl_choices ~words:
    (try Misc.split_string (Misc.exec_command "genet query tools") ['\n']
     with _ -> [])
    ()
;;

let compl_branch () =
  Cmdline.compl_choices ~words:
    (try Misc.split_string (Misc.exec_command "genet query branches") ['\n']
     with _ -> [])
    ()
;;

let compl_version () =
  Cmdline.compl_choices ~words:
    (try Misc.split_string (Misc.exec_command "genet query branches") ['\n']
     with _ -> [])
    ()
;;

let compl_intf () =
  Cmdline.compl_choices ~words:
    (try
       let l = Misc.split_string
         (Misc.exec_command "genet query interfaces") ['\n']
       in
       let f acc s =
         match Misc.split_string s [' '] with
           [] -> acc
         | h :: _ -> h :: acc
       in
       List.fold_left f [] l
     with _ -> []
     )
    ()
;;

let compl_filetype_name () =
  let l =
    (try Misc.split_string (Misc.exec_command "genet query filetypes") ['\n']
     with _ -> [])
  in
  let words = List.fold_left
    (fun acc s ->
      match Misc.split_string s [' '] with
         [] -> acc
       | h :: _ -> h :: acc
    )
    [] l
  in
  Cmdline.compl_choices ~words  ()
;;

let compl_port () = Cmdline.compl_choices ~words: ["http://"] ()
(*
  Cmdline.compl_choices ~words:
    (try Misc.split_string (Misc.exec_command "genet query ports") ['\n']
     with _ -> [])
*)

let compl_ichain () = Cmdline.compl_choices ~words: ["http://"] ();;
let compl_tool_or_branch () =
  let tools =
    try Misc.split_string (Misc.exec_command "genet query tools") ['\n']
    with _ -> []
  in
  let branches =
    try Misc.split_string (Misc.exec_command "genet query branches") ['\n']
    with _ -> []
  in
  Cmdline.compl_choices ~words: (tools @ branches) ()
;;

let compl_intf_provider () = Cmdline.compl_choices ~words: ["http://"] ();;
let compl_file_uri () = Cmdline.compl_choices ~words: ["http://"] ();;
let compl_input_name () = Cmdline.compl_choices ~words: [] ();;
let compl_chain_name () = Cmdline.compl_choices ~words: [] ();;
let compl_fchain () = Cmdline.compl_choices ~words: ["http://"] ();;
let compl_in_out () = Cmdline.compl_choices ~words:  ["in" ; "out"] ();;
let compl_bool () = Cmdline.compl_choices ~words: ["true" ; "false"] ();;
