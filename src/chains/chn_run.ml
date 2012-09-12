(** Running commands to instanciate a chain. *)

open Grdf_types;;
open Chn_types;;

let dbg = Misc.create_log_fun
  ~prefix: "Chn_run"
    "GENET_CHN_RUN_DEBUG_LEVEL"
;;

let ichain_start_date ctx uri =
  match Grdfs.start_date_uri ctx.ctx_rdf uri with
    None -> None
  | Some d -> Some (Netdate.mk_mail_date (Netdate.since_epoch d))
;;

let ichain_stop_date ctx uri =
  match Grdfs.stop_date_uri ctx.ctx_rdf uri with
    None -> None
  | Some d -> Some (Netdate.mk_mail_date (Netdate.since_epoch d))
;;


module Graph = Graph.Make_with_map
  (struct
     type t = Rdf_uri.uri
     let compare = Rdf_uri.compare
   end
  )
  (struct
     type t = Rdf_uri.uri * Rdf_uri.uri
     let compare (p1, p2) (p3, p4) =
       match Rdf_uri.compare p1 p3 with
         0 -> Rdf_uri.compare p2 p4
       | n -> n
   end)
;;

let replace_version ctx str version =
  let name = Grdf_version.name ctx.ctx_rdf version in
  Str.global_replace (Str.regexp "%v") name str
;;

let extract_git_file config ~file ~id ~target =
  let data_dir = Config.data_dir config in
  let base_dir = Misc.path_under ~parent: data_dir file in
  let target_dir = Filename.quote (Filename.dirname target) in
  let target_base = Filename.quote (Filename.basename target) in
  let com = Printf.sprintf
    "mkdir -p %s/.tmp; \
    (cd %s ; git archive --format tar %s %s| (cd %s/.tmp ; tar xvf -)) ; \
    mv %s/.tmp/%s %s/%s ; \
    rm -fr %s/.tmp/"
    target_dir
    (Filename.quote data_dir) id (Filename.quote base_dir) target_dir
    target_dir (Filename.quote base_dir) target_dir target_base
    target_dir
  in
  prerr_endline com;
  match Sys.command com with
    0 -> ()
  | n ->
     let msg = Printf.sprintf "Command failed [%d]: %s" n com in
     failwith msg
;;

let filename_of_md5 ctx md5 =
  Filename.concat (Config.out_dir ctx.ctx_cfg) md5
;;

let record_file ctx reporter file port =
  if not (Sys.file_exists file) then
    failwith (Printf.sprintf "File %S not found" file);
  let md5 =
    match (Unix.stat file).Unix.st_kind with
      Unix.S_DIR -> Misc.dir_md5sum file
    | Unix.S_REG -> Misc.file_md5sum file
    | _ -> failwith (Printf.sprintf "Invalid file kind for %S" file)
  in
  let outfile = Filename.concat (Config.out_dir ctx.ctx_cfg) md5 in
  if not (Sys.file_exists outfile) then
    Misc.copy_file ~src: file ~dst: outfile;
  Grdfs.add_triple ctx.ctx_rdf
    ~sub: (Rdf_node.Uri port) ~pred: (Rdf_node.Uri Grdfs.genet_filemd5)
    ~obj: (Rdf_node.node_of_literal_string md5)
;;

(*
let rec run_node ctx reporter comb g port_to_file tmp_dir uri_node =
  let in_ports = Grdf_port.ports ctx.ctx_rdf uri_node Grdf_port.In in
  let in_files = List.map (fun uri -> Urimap.find uri port_to_file) in_ports in
  let test file =
     let file = Filename.concat tmp_dir file in
     if not (Sys.file_exists file) then
      failwith (Printf.sprintf "File %s does not exist" file)
  in
  List.iter test in_files;

  let out_ports = Grdf_port.ports ctx.ctx_rdf uri_node Grdf_port.Out in
  let out_files = List.map (fun uri -> Urimap.find uri port_to_file) out_ports in

  let uri_from = Chn_flat.get_op_origin ctx uri_node in
  match Grdf_intf.intf_exists ctx.ctx_rdf uri_from with
    None -> assert false
  | Some _ ->
      match Grdf_intf.command_path ctx.ctx_rdf uri_from with
        None ->
          failwith
          (Printf.sprintf "No path for interface %s" (Rdf_uri.string uri_from))
      | Some path ->
          let tool = Grdf_intf.tool_of_intf uri_from in
          let version = Urimap.find tool comb in
          let path = replace_version ctx path version in
          prerr_endline (Printf.sprintf "path = %s" path);
          Grdfs.set_start_date_uri ctx.ctx_rdf uri_node ();
          begin
            try
              (
               run_command ctx reporter tmp_dir path in_files out_ports out_files;
              );
              Grdfs.set_stop_date_uri ctx.ctx_rdf uri_node ();
            with
              e ->
                Grdfs.set_stop_date_uri ctx.ctx_rdf uri_node ();
                raise e
          end;
          List.iter2
                (fun port file ->
                 record_file ctx reporter (Filename.concat tmp_dir file) port)
                out_ports out_files;
          let g = Graph.remove_node g uri_node in
          run_nodes ctx reporter comb g port_to_file tmp_dir
          (Graph.pred_roots g)

(*and run_foreach ctx reporter comb gport_to_file tmp_dir uri_node =*)


and run_nodes ctx reporter comb g port_to_file tmp_dir uri_nodes =
  List.fold_left
  (fun g uri -> run_node ctx reporter comb g port_to_file tmp_dir uri)
  g uri_nodes
;;

let init_run ctx reporter uri_inst input g port_to_file tmp_dir =
  let in_files = input.Ind_types.in_files in
  prerr_endline (Printf.sprintf "uri_inst = %S" (Rdf_uri.string uri_inst));
  let port_files =
    let ports = Grdf_port.ports ctx.ctx_rdf uri_inst Grdf_port.In in
    List.map (fun uri ->
       prerr_endline (Printf.sprintf "port uri = %S" (Rdf_uri.string uri));
       (uri, Urimap.find uri port_to_file))
       ports
  in
  let nb_in_files = List.length in_files in
  let nb_port_files = List.length port_files in
  if nb_in_files <> nb_port_files then
    failwith
    (Printf.sprintf "Numbers of input files (%d) and ports (%d) differ."
      nb_in_files nb_port_files);

  let f (in_file, id) (port, port_file) =
    let target = Filename.concat tmp_dir port_file in
    let file = Filename.concat input.Ind_types.dir in_file in
    extract_git_file ctx.ctx_cfg ~file ~id ~target;
    record_file ctx reporter target port
  in
  List.iter2 f in_files port_files
;;

let run_graph ctx reporter uri_inst comb input g port_to_file =
  Grdfs.set_start_date_uri ctx.ctx_rdf uri_inst ();
  print_endline "run_graph";

  let tmp_dir = Filename.temp_file "genet-run" ".dir" in
  Sys.remove tmp_dir;
  Misc.mkdir tmp_dir ;

  Urimap.iter
    (fun uri file -> prerr_endline (Printf.sprintf "%s => %s" (Rdf_uri.string uri) file))
    port_to_file;

  init_run ctx reporter uri_inst input g port_to_file tmp_dir;
  let g = Graph.remove_node g uri_inst in
(*  try*)
  let g = run_nodes ctx reporter comb g port_to_file tmp_dir (Graph.pred_roots g) in

  (* copy out files ? *)
  let _out_files = input.Ind_types.out_files in
  let _port_files =
    let ports = Grdf_port.ports ctx.ctx_rdf uri_inst Grdf_port.Out in
    List.map (fun uri -> Urimap.find uri port_to_file) ports
  in

  (* graph should be empty now *)
  begin
    match Graph.fold_succ g (fun k _ acc -> k :: acc) [] with
      [] ->  Grdfs.set_stop_date_uri ctx.ctx_rdf uri_inst ()
    | l ->
        Grdfs.set_stop_date_uri ctx.ctx_rdf uri_inst ();
        let l = List.map Rdf_uri.string l in
        let msg = Printf.sprintf "The following nodes were not executed:\n%s"
          (String.concat "\n" l)
        in
        failwith msg
  end
;;
*)

(****************************** new version ********************************)

let run_command ctx reporter tmp_dir path in_files inst_out_ports out_files =
  let com = Printf.sprintf
    "cd %s ; %s %s %s"
    (Filename.quote tmp_dir)
    path
    (String.concat " " (List.map Filename.quote in_files))
    (String.concat " " (List.map Filename.quote out_files))
  in
  match Sys.command com with
    0 ->
      List.iter2
      (fun port file ->
         record_file ctx reporter (Filename.concat tmp_dir file) port)
      inst_out_ports out_files
  | n ->
      failwith (Printf.sprintf "Command failed [%d]: %s" n com)
;;
let gen_file =
  let cpt = ref 0 in
  fun ?ext () ->
    incr cpt;
    Printf.sprintf "file%d%s" !cpt (match ext with None -> "" | Some s -> "."^s)
;;

let copy_flat_port ctx ~inst ~container port_map flat_port  =
  let inst_port = Chn_types.uri_inst_port_of_flat_port
    ctx ~inst ~flat: flat_port
  in
  let ptype = Grdf_port.port_type ctx.ctx_rdf flat_port in
  Grdf_port.set_port_type ctx.ctx_rdf inst_port ptype;
  Grdfs.add_triple_uris ctx.ctx_rdf
    ~sub: inst_port ~pred: Grdfs.genet_opfrom ~obj: flat_port;

  let pred = Grdf_port.pred_of_dir (Grdf_port.port_dir flat_port) in
  Grdfs.add_triple_uris ctx.ctx_rdf ~sub: container ~pred ~obj: inst_port;
  (inst_port, Urimap.add flat_port inst_port port_map)
;;

(* we make sure to keep the order of flat_ports when producing inst_ports *)
let copy_flat_ports ctx ~inst ~container port_map flat_ports =
  let f flat_port (inst_ports, port_map) =
    let (inst_port, port_map) = copy_flat_port ctx ~inst ~container port_map flat_port in
    (inst_port :: inst_ports, port_map)
  in
  List.fold_right f flat_ports ([], port_map)
;;

let new_file ctx tmp_dir port_to_file inst_port =
  let (ext, is_dir) =
    let typ = Grdf_port.port_type ctx.ctx_rdf inst_port in
    match typ with
      Grdf_types.T s ->
        let uri = Grdfs.uri_filetype ctx.ctx_cfg.Config.rest_api s in
        (Some (Grdf_ftype.extension ctx.ctx_rdf uri), false)
    | Var _ -> None, false
    | Set _ | Tuple _ -> (None, true)
  in
  let file = gen_file ?ext () in
  if is_dir then Misc.mkdir (Filename.concat tmp_dir file);
  (file, Urimap.add inst_port file port_to_file)
;;

(* we make sure to keep order of inst_ports in the returned file list *)
let new_files ctx tmp_dir port_to_file inst_ports =
  let f inst_port  (files, port_to_file) =
    let (file, port_to_file) = new_file ctx tmp_dir port_to_file inst_port in
    (file :: files, port_to_file)
  in
  List.fold_right f inst_ports ([], port_to_file)
;;

let init_run ctx reporter ~inst ~fchain input tmp_dir =
  let in_files = input.Ind_types.in_files in
  prerr_endline (Printf.sprintf "inst = %S" (Rdf_uri.string inst));
  let flat_in_ports = Grdf_port.ports ctx.ctx_rdf fchain Grdf_port.In in
  let (inst_in_ports, port_map) =
    copy_flat_ports ctx ~inst ~container: inst Urimap.empty flat_in_ports
  in

  let nb_in_files = List.length in_files in
  let nb_in_ports = List.length inst_in_ports in
  if nb_in_files <> nb_in_ports then
    failwith
    (Printf.sprintf "Numbers of input files (%d) and ports (%d) differ."
      nb_in_files nb_in_ports);

  let f port_to_file (in_file, id) inst_port =
    let (port_file, port_to_file) = new_file ctx tmp_dir port_to_file inst_port in
    let target = Filename.concat tmp_dir port_file in
    let file = Filename.concat input.Ind_types.dir in_file in
    extract_git_file ctx.ctx_cfg ~file ~id ~target;
    record_file ctx reporter target inst_port;
    port_to_file
  in
  (List.fold_left2 f Urimap.empty in_files inst_in_ports, port_map)
;;

let get_node_input_file ctx port_to_file port_map flat_port =
  match Chn_flat.port_producers ctx flat_port with
    [] -> failwith (Printf.sprintf "No producer for flat port %S" (Rdf_uri.string flat_port))
  | _ :: _ :: _ ->
      failwith (Printf.sprintf "More than one producer for flat port %S" (Rdf_uri.string flat_port))
  | [src_port] ->
      let inst_src_port =
        try Urimap.find src_port port_map
        with Not_found ->
            failwith (Printf.sprintf "No inst port for flat port %S" (Rdf_uri.string src_port))
      in
      try Urimap.find inst_src_port port_to_file
      with Not_found ->
          failwith (Printf.sprintf "No file for inst port %S" (Rdf_uri.string inst_src_port))
;;

let get_node_input_files ctx port_to_file port_map flat_ports =
  List.map (get_node_input_file ctx port_to_file port_map) flat_ports
;;

let new_inst_node ctx ~inst flat_node =
  let inst_node = Chn_types.uri_inst_opn_of_flat_opn
      ~prefix: ctx.ctx_cfg.Config.rest_api ~inst ~flat: flat_node
  in
  Grdfs.add_triple_uris ctx.ctx_rdf
  ~sub: inst_node ~pred: Grdfs.genet_opfrom ~obj: flat_node;

  Grdfs.add_type ctx.ctx_rdf
  ~sub: (Rdf_node.Uri inst_node) ~obj: (Rdf_node.Uri Grdfs.genet_instopn);

  Chn_flat.add_containsop ctx ~src: inst ~dst: inst_node;
  inst_node
;;

let run_explode ctx inst tmp_dir ~inst_node ~flat_node (g, port_to_file, port_map) =
  dbg ~level:1 (fun () -> Printf.sprintf "run_explode inst_node=%S" (Rdf_uri.string inst_node));
  let inst_in_port =
      match Grdf_port.ports ctx.ctx_rdf inst_node Grdf_port.In with
      [p] -> p
    | _ -> assert false
  in
  let in_file = try Urimap.find inst_in_port port_to_file with Not_found -> assert false in
  let flat_exploded =
    let inst_out_port =
      match Grdf_port.ports ctx.ctx_rdf inst_node Grdf_port.Out with
        [p] -> p
      | _ -> assert false
    in
    let flat_out_port = Urimap.find inst_out_port port_map in
    let flat_dst_port =
      match Chn_flat.port_consumers ctx flat_out_port with
        [x] -> x
      | _ -> assert false
    in
    Grdfs.port_container flat_dst_port
  in
  let in_files =
     let root = Filename.concat tmp_dir in_file in
     let entries = Find.find_list Find.Ignore [root] [Find.Maxdepth 1] in
     List.filter ((<>) root) entries
  in
  let f_in_file (g, port_to_file, port_map) =
     assert false
  in
  let (g, port_to_file, port_map) = List.fold_left f_in_file (g,port_to_file,port_map) in_files in
  let g = Graph.remove_node g flat_node in
  (g, port_to_file, port_map)
;;

let run_implode ctx inst tmp_dir ~inst_node ~flat_node (g, port_to_file, port_map) =
  dbg ~level:1 (fun () -> Printf.sprintf "run_implode inst_node=%S" (Rdf_uri.string inst_node));
  let g = Graph.remove_node g flat_node in
  (g, port_to_file, port_map)
;;

let rec run_node ctx reporter inst comb tmp_dir (g, port_to_file, port_map) flat_node =
  dbg ~level: 1
    (fun () -> Printf.sprintf "run_node %S" (Rdf_uri.string flat_node));
  let inst_node = new_inst_node ctx ~inst flat_node in

  let flat_in_ports = Grdf_port.ports ctx.ctx_rdf flat_node Grdf_port.In in
  let (inst_in_ports, port_map) =
    copy_flat_ports ctx ~inst ~container: inst_node port_map flat_in_ports
  in
  let in_files = get_node_input_files ctx port_to_file port_map flat_in_ports in

  let test file =
     let file = Filename.concat tmp_dir file in
     if not (Sys.file_exists file) then
      failwith (Printf.sprintf "File %s does not exist" file)
  in
  List.iter test in_files;

  let flat_out_ports = Grdf_port.ports ctx.ctx_rdf flat_node Grdf_port.Out in
  let (inst_out_ports, port_map) =
    copy_flat_ports ctx ~inst ~container: inst_node port_map flat_out_ports
  in

  let (out_files, port_to_file) = new_files ctx tmp_dir port_to_file inst_out_ports in

  let uri_from = Chn_flat.get_op_origin ctx flat_node in
  let (g, port_to_file, port_map) =
    match uri_from with
    | _ when Rdf_uri.equal uri_from Grdfs.genet_explode ->
        run_explode ctx inst tmp_dir ~inst_node ~flat_node (g, port_to_file, port_map)
    | _ when Rdf_uri.equal uri_from Grdfs.genet_implode ->
        run_implode ctx inst tmp_dir ~inst_node ~flat_node (g, port_to_file, port_map)
    | _ ->
        match Grdf_intf.intf_exists ctx.ctx_rdf uri_from with
          None -> assert false
        | Some _ ->
            match Grdf_intf.command_path ctx.ctx_rdf uri_from with
              None ->
                failwith
                (Printf.sprintf "No path for interface %s" (Rdf_uri.string uri_from))
            | Some path ->
                let tool = Grdf_intf.tool_of_intf uri_from in
                let version = Urimap.find tool comb in
                let path = replace_version ctx path version in
                prerr_endline (Printf.sprintf "path = %s" path);
                Grdfs.set_start_date_uri ctx.ctx_rdf inst_node ();
                begin
                  try
                    run_command ctx reporter tmp_dir path in_files inst_out_ports out_files;
                  Grdfs.set_stop_date_uri ctx.ctx_rdf inst_node ();
                  with
                    e ->
                      Grdfs.set_stop_date_uri ctx.ctx_rdf inst_node ();
                      raise e
                end;
                let g = Graph.remove_node g flat_node in
                (g, port_to_file, port_map)
  in
  run_nodes ctx reporter inst comb tmp_dir (g, port_to_file, port_map)

and run_nodes ctx reporter inst comb tmp_dir (g, port_to_file, port_map) =
  dbg ~level: 1 (fun () -> "run_nodes");
  match Graph.pred_roots g with
    [] -> (g, port_to_file, port_map)
  | flat_node :: _ ->
      run_node ctx reporter inst comb tmp_dir (g, port_to_file, port_map) flat_node
;;

let run ctx reporter ~inst ~fchain input comb g =
  Grdfs.set_start_date_uri ctx.ctx_rdf inst ();
  (* break cycles due to having only one node for the flat chain
    in the graph, with input ports and output ports associated to it. *)
  let g = Graph.remove_node g fchain in

  let tmp_dir = Filename.temp_file "genet-run" ".dir" in
  Sys.remove tmp_dir;
  Misc.mkdir tmp_dir ;

  let (port_to_file, port_map) = init_run ctx reporter ~inst ~fchain input tmp_dir in
  let g = Graph.remove_node g inst in
  let (g, port_to_file, port_map) =
    run_nodes ctx reporter inst comb tmp_dir (g, port_to_file, port_map)
  in

  let flat_out_ports = Grdf_port.ports ctx.ctx_rdf fchain Grdf_port.Out in
  let (inst_out_ports, port_map) =
    copy_flat_ports ctx ~inst ~container: inst port_map flat_out_ports
  in
  let out_files = get_node_input_files ctx port_to_file port_map flat_out_ports in
  let out_files = List.map (Filename.concat tmp_dir) out_files in

  List.iter2 (record_file ctx reporter) out_files inst_out_ports;

  (* graph should be empty now *)
  begin
    match Graph.fold_succ g (fun k _ acc -> k :: acc) [] with
      [] ->  Grdfs.set_stop_date_uri ctx.ctx_rdf inst ()
    | l ->
        Grdfs.set_stop_date_uri ctx.ctx_rdf inst ();
        let l = List.map Rdf_uri.string l in
        let msg = Printf.sprintf "The following nodes were not executed:\n%s"
          (String.concat "\n" l)
        in
        failwith msg
  end
;;
