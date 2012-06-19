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

let rec run_node ctx reporter uri_fchain g port_to_file uri_node =
  ()

and run_nodes ctx reporter uri_fchain g port_to_file uri_nodes =
  ()
;;

let init_run ctx reporter uri_fchain input g port_to_file tmp_dir =
  let in_files = input.Ind_types.in_files in
  let port_files =
    let ports = Grdf_port.ports ctx.ctx_rdf uri_fchain Grdf_port.In in
    List.map (fun uri -> Urimap.find uri port_to_file) ports
  in
  let f (in_file, id) port_file =
    let target = Filename.concat tmp_dir port_file in
    let file = Filename.concat input.Ind_types.dir in_file in
    extract_git_file ctx.ctx_cfg ~file ~id ~target
  in
  List.iter2 f in_files port_files
;;

let run_graph ctx reporter uri_fchain input g port_to_file =
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
  init_run ctx reporter uri_fchain input g port_to_file tmp_dir ;

  (* clean tmp_dir *)
;;
