(** Running commands to instanciate a chain. *)

open Chn_types;;


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

let run_command ctx reporter tmp_dir path in_files out_ports out_files =
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
      out_ports out_files
  | n ->
      failwith (Printf.sprintf "Command failed [%d]: %s" n com)
;;

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
  (* break cycles due to having only one node for the flat chain
    in the graph, with input ports and output ports associated to it. *)
  let preds = Graph.pred g uri_inst in
  let g = List.fold_left
    (fun g (k,_) -> Graph.rem_all g (k, uri_inst)) g preds
  in
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
  end;


(*  with
    Failure s | Sys_error s ->
      reporter#error s ;
      reporter#incr_errors;
*)
  (* clean tmp_dir *)
;;
