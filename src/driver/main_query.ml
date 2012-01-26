(** Main module of the genet-query program.*)

open Grdf_tool;;

type mode =
  | Tools

let mode = ref None;;

let options =
    Options.option_version "Genet-query" ::
    Options.option_config ::
    ("--tools", Arg.Unit (fun () -> mode := Some Tools), " print tools") ::
    []
;;

let list_tools wld =
  let tools = Grdf_tool.tools wld in
  List.iter
  (fun t ->
    print_endline (Printf.sprintf "%s <%s>" t.tool_name t.tool_uri)
  )
  tools
;;

let main () =
  let opts = Options.parse options in
  let config = Config.read_config opts.Options.config_file in
  let rdf_wld = Grdf_init.open_storage config in
  begin
    match !mode with
      None -> ()
    | Some Tools -> list_tools rdf_wld
  end;
  Grdf_init.close rdf_wld
;;

let () = Misc.safe_main main;;
