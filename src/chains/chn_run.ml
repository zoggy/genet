(** Running commands to instanciate a chain. *)

open Chn_types;;

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
  Str.global_replace (Str.regexp "%v") str name
;;

let extract_git_file config ~file ~id ~target =
  let in_dir = Config.in_dir config in
  let base_dir =
    if Misc.is_prefix in_dir file then
      begin
        let len = String.length in_dir + 1 in
        String.sub file len (String.length file - len)
      end
    else
      failwith (Printf.sprintf "%s is not under %s" file in_dir)
  in
  let target_dir = Filename.quote (Filename.dirname target) in
  let target_base = Filename.quote (Filename.basename target) in
  let com = Printf.sprintf
    "mkdir -p %s/.tmp; \
    (cd %s ; git archive --format tar %s %s| (cd %s/.tmp ; tar xvf -)) ; \
    mv %s/.tmp/%s %s/%s ; \
    rm -fr %s/.tmp/"
    target_dir
    (Filename.quote in_dir) id (Filename.quote base_dir) target_dir
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

let rec run_node ctx reporter uri_fchain comb g port_to_file tmp_dir uri_node =
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
          let com = Printf.sprintf
            "cd %s ; %s %s %s"
            (Filename.quote tmp_dir)
            path
            (String.concat " " (List.map Filename.quote in_files))
            (String.concat " " (List.map Filename.quote out_files))
          in
          match Sys.command com with
            0 ->
              let g = Graph.remove_node g uri_node in
              run_nodes ctx reporter uri_fchain comb g port_to_file tmp_dir
              (Graph.pred_roots g)
          | n ->
              failwith (Printf.sprintf "Command failed [%d]: %s" n com)

and run_nodes ctx reporter uri_fchain comb g port_to_file tmp_dir uri_nodes =
  List.fold_left
  (fun g uri -> run_node ctx reporter uri_fchain comb g port_to_file tmp_dir uri)
  g uri_nodes
;;

let init_run ctx reporter uri_fchain input g port_to_file tmp_dir =
  let in_files = input.Ind_types.in_files in
  let port_files =
    let ports = Grdf_port.ports ctx.ctx_rdf uri_fchain Grdf_port.In in
    List.map (fun uri -> Urimap.find uri port_to_file) ports
  in
  if List.length in_files <> List.length port_files then
    failwith "Numbers of input files and ports differ.";

  let f (in_file, id) port_file =
    let target = Filename.concat tmp_dir port_file in
    let file = Filename.concat input.Ind_types.dir in_file in
    extract_git_file ctx.ctx_cfg ~file ~id ~target
  in
  List.iter2 f in_files port_files
;;

let run_graph ctx reporter uri_fchain comb input g port_to_file =
  print_endline "run_graph";
  (* break cycles due to having only one node for the flat chain
    in the graph, with input ports and output ports associated to it. *)
  let preds = Graph.pred g uri_fchain in
  let g = List.fold_left
    (fun g (k,_) -> Graph.rem_all g (k, uri_fchain)) g preds
  in
  let tmp_dir = Filename.temp_file "genet-run" ".dir" in
  Sys.remove tmp_dir;
  Misc.mkdir tmp_dir ;
  init_run ctx reporter uri_fchain input g port_to_file tmp_dir;
  let g = Graph.remove_node g uri_fchain in
(*  try*)
  run_nodes ctx reporter uri_fchain comb g port_to_file tmp_dir (Graph.pred_roots g)
(*  with
    Failure s | Sys_error s ->
      reporter#error s ;
      reporter#incr_errors;
*)
  (* clean tmp_dir *)
;;
