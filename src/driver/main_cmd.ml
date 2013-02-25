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