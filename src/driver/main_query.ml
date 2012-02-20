(** Main module of the genet-query program.*)

open Grdf_tool;;

type mode =
  | Tools
  | Branches
  | Versions
  | Interfaces
  | Filetypes
  | Dot
  | Test

let mode = ref None;;

let options =
  Options.option_version "Genet-query" ::
  Options.option_config ::
  ("--tools", Arg.Unit (fun () -> mode := Some Tools), " print tools") ::

  ("--branches", Arg.Unit (fun () -> mode := Some Branches),
   " print branches (all or only the ones of the given parents)") ::

  ("--versions", Arg.Unit (fun () -> mode := Some Versions),
   " print versions (all or only the ones of the given tools or branches)") ::

  ("--interfaces", Arg.Unit (fun () -> mode := Some Interfaces),
   " print interfaces (all or only the ones of the given tools or branches)") ::

  ("--filetypes", Arg.Unit (fun () -> mode := Some Filetypes),
   " print filetypes") ::

  ("--dot", Arg.Unit (fun () -> mode := Some Dot),
   " print graph in graphviz format") ::

  ("--test", Arg.Unit (fun () -> mode := Some Test),
   " not documented (testing purpose)") ::

  []
;;

let list_tools wld =
  let tools = Grdf_tool.tools wld in
  List.iter print_endline tools
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

let list_versions wld options =
  let versions =
    match options.Options.args with
      [] -> Grdf_version.versions wld
    | l ->
        let add set elt = Sset.add elt set in
        let f set uri =
          List.fold_left add set (Grdf_version.versions_of ~recur: true wld uri)
        in
        let set = List.fold_left f Sset.empty l in
        Sset.elements set
  in
  List.iter print_endline versions
;;

let list_interfaces wld options =
  let intfs =
    match options.Options.args with
      [] -> Grdf_intf.intfs wld
    | l ->
        let add set elt = Sset.add elt set in
        let f set uri =
          List.fold_left add set (Grdf_intf.intfs_of ~recur: true wld uri)
        in
        let set = List.fold_left f Sset.empty l in
        Sset.elements set
  in
  List.iter (fun uri -> print_endline (Grdf_intf.string_of_intf wld uri)) intfs
;;

let list_filetypes wld =
  let l = Grdf_ftype.filetypes wld in
  List.iter (fun ft -> prerr_endline (Grdf_ftype.string_of_filetype wld ft)) l
;;

let dot wld = print_endline (Grdf_dot.dot wld);;

let test wld =
  match Grdf_intf.intfs wld with
    [] -> failwith "no interfaces to use to test"
  | intf :: _ ->
      match Grdf_ftype.filetypes wld with
        [] -> failwith "no filetype to use to test"
      | filetypes ->
          let filetypes = Array.of_list filetypes in
          let n = Array.length filetypes in
          prerr_endline (Grdf_intf.string_of_intf wld intf);
          Random.self_init();
          let dir = if Random.int 2 = 0 then Grdf_intf.In else Grdf_intf.Out in
          Grdf_intf.set_ports wld dir intf [];
          let ftype = filetypes.(Random.int n) in
          let port =
            if Random.int 3 = 0 then Grdf_intf.List ftype else Grdf_intf.One ftype
          in
          Grdf_intf.add_port wld dir intf port;
          prerr_endline (Grdf_intf.string_of_intf wld intf);
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
    | Some Versions -> list_versions rdf_wld opts
    | Some Interfaces -> list_interfaces rdf_wld opts
    | Some Filetypes -> list_filetypes rdf_wld
    | Some Dot -> dot rdf_wld
    | Some Test -> test rdf_wld
  end;
  Grdf_init.close rdf_wld
;;

let () = Misc.safe_main main;;
