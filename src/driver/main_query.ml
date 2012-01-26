(** Main module of the genet-query program.*)

open Grdf_tool;;

type mode =
  | Tools
  | Branches

let mode = ref None;;

let options =
  Options.option_version "Genet-query" ::
  Options.option_config ::
  ("--tools", Arg.Unit (fun () -> mode := Some Tools), " print tools") ::

  ("--branches", Arg.Unit (fun () -> mode := Some Branches),
   " print branches (all or only the ones of the given parent)") ::

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

let list_branches wld options =
  let branches =
    match options.Options.args with
      [] -> List.map (fun b -> b.Grdf_branch.bch_uri) (Grdf_branch.branches wld)
    | l ->
        let add set elt = Sset.add elt set in
        let f set uri =
          List.fold_left add set (Grdf_branch.subs wld uri)
        in
        let set = List.fold_left f Sset.empty l in
        Sset.elements set
  in
  List.iter print_endline branches
;;

let main () =
  let opts = Options.parse options in
  let config = Config.read_config opts.Options.config_file in
  let rdf_wld = Grdf_init.open_storage config in
  begin
    match !mode with
      None -> ()
    | Some Tools -> list_tools rdf_wld
    | Some Branches -> list_branches rdf_wld opts
  end;
  Grdf_init.close rdf_wld
;;

let () = Misc.safe_main main;;
