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

type g_uri = Flat of Rdf_uri.uri | Inst of Rdf_uri.uri;;
let compare_g_uri t1 t2 =
  match t1, t2 with
    Flat uri1, Flat uri2
  | Flat uri1, Inst uri2
  | Inst uri1, Flat uri2
  | Inst uri1, Inst uri2 -> Rdf_uri.compare uri1 uri2
;;
let compare_g_uri_pair   (p1, p2) (p3, p4) =
  match compare_g_uri p1 p3 with
    0 -> compare_g_uri p2 p4
  | n -> n
;;
let uri_of_g_uri = function Flat u -> u | Inst u -> u;;
let g_uri_string u = Rdf_uri.string (uri_of_g_uri u);;
module Guri_ord_type = struct type t = g_uri let compare = compare_g_uri end;;

module Guriset = Set.Make(Guri_ord_type)
module Gurimap = Map.Make(Guri_ord_type)
module Graph = Graph.Make_with_map (Guri_ord_type)
  (struct
     type t = g_uri * g_uri
     let compare = compare_g_uri_pair
   end)
;;

let dot_of_graph ctx g =
  let f_edge (p1, p2) =
    let p1 = uri_of_g_uri p1 in
    let p2 = uri_of_g_uri p2 in
    let type1 = Grdf_port.port_type ctx.ctx_rdf p1 in
    let type2 = Grdf_port.port_type ctx.ctx_rdf p2 in
    let f p = Grdf_port.string_of_port_type (fun x -> x) p in
    let label = Printf.sprintf "%s:%s" (f type1) (f type2) in
    (label, [])
  in
  let f_node uri =
    let uri = uri_of_g_uri uri in
    let label =
      try
        let uri_from = Chn_flat.get_op_origin ctx uri in
        match Grdf_intf.intf_exists ctx.ctx_rdf uri_from with
          None -> Filename.basename (Rdf_uri.string uri)
        | Some name -> name
      with
        _ ->
          Filename.basename (Rdf_uri.string uri)
    in
    let href = Rdf_uri.string uri in
    let id = "n"^(Digest.to_hex (Digest.string href)) in
    (id, label, [ "href", href ])
  in
  Graph.dot_of_graph ~f_edge ~f_node g
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
    ~sub: (Rdf_node.Uri (uri_of_g_uri port)) ~pred: (Rdf_node.Uri Grdfs.genet_filemd5)
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

type state =
  { g : Graph.t ;
    port_to_file : string Gurimap.t ;
    nodes_run : Guriset.t ;
  }

let sort_ports =
  let comp p1 p2 =
    Pervasives.compare
    (Grdf_port.port_rank (uri_of_g_uri p1))
    (Grdf_port.port_rank (uri_of_g_uri p2))
  in
  List.sort comp
;;

let in_ports state node =
  let preds = Graph.pred state.g node in
  sort_ports
  (Guriset.elements
   (List.fold_left (fun set (_,(_,p)) -> Guriset.add p set) Guriset.empty preds)
  )
;;
let out_ports state node =
  let preds = Graph.succ state.g node in
  sort_ports
  (Guriset.elements
   (List.fold_left (fun set (_,(p,_)) -> Guriset.add p set) Guriset.empty preds)
  )
;;

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

let copy_flat_port ctx ~inst ~container flat_port =
  let flat_port = uri_of_g_uri flat_port in
  let container = uri_of_g_uri container in
  let inst_port = Chn_types.uri_inst_port_of_flat_port
    ctx ~inst ~flat: flat_port
  in
  let ptype = Grdf_port.port_type ctx.ctx_rdf flat_port in
  Grdf_port.set_port_type ctx.ctx_rdf inst_port ptype;
  Grdfs.add_triple_uris ctx.ctx_rdf
    ~sub: inst_port ~pred: Grdfs.genet_opfrom ~obj: flat_port;

  let dir = Grdf_port.port_dir flat_port in
  let pred = Grdf_port.pred_of_dir dir in
  Grdfs.add_triple_uris ctx.ctx_rdf ~sub: container ~pred ~obj: inst_port;
  Inst inst_port
;;

(* we make sure to keep the order of flat_ports when producing inst_ports *)
let copy_flat_ports ctx ~inst ~container flat_ports =
  let f flat_port (inst_ports, port_map) =
    let inst_port = copy_flat_port ctx ~inst ~container flat_port in
    (inst_port :: inst_ports, Gurimap.add flat_port inst_port port_map)
  in
  List.fold_right f flat_ports ([], Gurimap.empty)
;;

let new_file ctx tmp_dir state inst_port =
  let (ext, is_dir) =
    let typ = Grdf_port.port_type ctx.ctx_rdf (uri_of_g_uri inst_port) in
    match typ with
      Grdf_types.T s ->
        let uri = Grdfs.uri_filetype ctx.ctx_cfg.Config.rest_api s in
        (Some (Grdf_ftype.extension ctx.ctx_rdf uri), false)
    | Var _ -> None, false
    | Set _ | Tuple _ -> (None, true)
  in
  let file = gen_file ?ext () in
  if is_dir then Misc.mkdir (Filename.concat tmp_dir file);
  let state = { state with port_to_file = Gurimap.add inst_port file state.port_to_file } in
   (file, state)
;;

(* we make sure to keep order of inst_ports in the returned file list *)
let new_files ctx tmp_dir port_to_file inst_ports =
  let f inst_port  (files, port_to_file) =
    let (file, port_to_file) = new_file ctx tmp_dir port_to_file inst_port in
    (file :: files, port_to_file)
  in
  List.fold_right f inst_ports ([], port_to_file)
;;

let runnable_nodes state =
  let g = Guriset.fold (fun node g -> Graph.remove_node g node)
    state.nodes_run state.g
  in
  Graph.pred_roots g
;;

let set_node_as_run state node =
  { state with nodes_run = Guriset.add node state.nodes_run }
;;

let rem_pred data x = compare_g_uri_pair data x = 0 ;;

let init_run ctx reporter ~inst ~fchain input tmp_dir g =
  let in_files = input.Ind_types.in_files in
  dbg ~level: 1 (fun () -> Printf.sprintf "inst = %S" (Rdf_uri.string inst));
  let state = { g ; port_to_file = Gurimap.empty ; nodes_run = Guriset.empty } in
  let flat_in_ports = out_ports state (Flat fchain) in
  let (inst_in_ports, port_map_in) =
    copy_flat_ports ctx ~inst ~container: (Inst inst) flat_in_ports
  in
  let flat_out_ports = in_ports state (Flat fchain) in
  let (inst_out_ports, port_map_out) =
      copy_flat_ports ctx ~inst ~container: (Inst inst) flat_out_ports
  in
  (* replace flat node by inst node in graph *)
  let g =
    let k = Flat fchain in
    let succs = Graph.succ state.g k in
    let preds = Graph.pred state.g k in
    let f_succ g (succ,(p1,p2)) =
      let g = Graph.rem g (k,succ) (rem_pred (p1,p2)) in
      let p1 =
        prerr_endline (Printf.sprintf "replacing %S" (g_uri_string p1));
        try Gurimap.find p1 port_map_in
        with Not_found -> assert false
      in
      Graph.add g (Inst inst, succ, (p1, p2))
    in
    let f_pred g (pred, (p1,p2)) =
      let g = Graph.rem g (pred, k) (rem_pred (p1,p2)) in
      let p2 =
       try Gurimap.find p2 port_map_out
       with Not_found -> assert false
      in
      Graph.add g (pred, Inst inst, (p1,p2))
    in
    let g = List.fold_left f_succ g succs in
    let g = List.fold_left f_pred g preds in
    g
  in
  let state = { state with g } in

  let nb_in_files = List.length in_files in
  let nb_in_ports = List.length inst_in_ports in
  if nb_in_files <> nb_in_ports then
    failwith
    (Printf.sprintf "Numbers of input files (%d) and ports (%d) differ."
      nb_in_files nb_in_ports);

  let state = { state with g = Graph.remove_node state.g (Flat fchain) } in
  let f state (in_file, id) inst_port =
    let (port_file, state) = new_file ctx tmp_dir state inst_port in
    let target = Filename.concat tmp_dir port_file in
    let file = Filename.concat input.Ind_types.dir in_file in
    extract_git_file ctx.ctx_cfg ~file ~id ~target;
    record_file ctx reporter target inst_port;
    state
  in
  List.fold_left2 f state in_files inst_in_ports
;;

let get_port_input_file ctx state port =
  let node = Grdfs.port_container (uri_of_g_uri port) in
  let src =
    let preds =
      try Graph.pred state.g (Inst node)
      with Not_found ->
          try Graph.pred state.g (Flat node)
          with Not_found ->
              failwith (Printf.sprintf "port container %S not in graph" (g_uri_string port))
    in
    let (_,(src,_)) =
      try List.find (fun (_,(src,dst)) -> compare_g_uri dst port = 0) preds
      with Not_found ->
          failwith (Printf.sprintf "No ancestor found for port %S" (g_uri_string port))
    in
    src
  in
  try Gurimap.find src state.port_to_file
  with Not_found ->
      failwith (Printf.sprintf "No file for inst port %S" (g_uri_string src))
;;

let get_port_input_files ctx state ports =
  List.map (get_port_input_file ctx state) ports
;;

let new_inst_node ctx ~inst node =
  let node = uri_of_g_uri node in
  let inst_node = Chn_types.uri_inst_opn_of_flat_opn
      ~prefix: ctx.ctx_cfg.Config.rest_api ~inst ~flat: node
  in
  Grdfs.add_triple_uris ctx.ctx_rdf
  ~sub: inst_node ~pred: Grdfs.genet_opfrom ~obj: node;

  Grdfs.add_type ctx.ctx_rdf
  ~sub: (Rdf_node.Uri inst_node) ~obj: (Rdf_node.Uri Grdfs.genet_instopn);

  Chn_flat.add_containsop ctx ~src: inst ~dst: inst_node;
  Inst inst_node
;;

let run_explode ctx reporter inst tmp_dir ~inst_node ~orig_node state =
  dbg ~level:1 (fun () -> Printf.sprintf "run_explode inst_node=%S" (g_uri_string inst_node));
  let inst_in_port = match in_ports state inst_node with [p] -> p | _ -> assert false in
  let in_file = try Gurimap.find inst_in_port state.port_to_file with Not_found -> assert false in
  let in_files =
     let root = Filename.concat tmp_dir in_file in
     let entries = Find.find_list Find.Ignore [root] [Find.Maxdepth 1] in
     List.filter ((<>) root) entries
  in
  let exploded_nodes =
    match Grdfs.object_uri ctx.ctx_rdf
      ~sub: (Rdf_node.Uri (uri_of_g_uri inst_node)) ~pred: Grdfs.genet_hasimplode
    with
      None -> failwith "No implode node associated to explode!"
    | Some uri_implode ->
        (* FIXME: maybe a problem here in case of nested explodes: how to find the correct
          implode, which may be a Inst node now ? *)
        let after_implode =
          let l = Graph.recursive_succs state.g (Flat uri_implode) in
          List.fold_right Guriset.add (Flat uri_implode :: l) Guriset.empty
        in
        let after_explode =
          let l = Graph.recursive_succs state.g inst_node in
          List.fold_right Guriset.add l Guriset.empty
        in
        Guriset.diff after_explode after_implode
  in
  let map cpt node =
    if Guriset.mem node exploded_nodes then
      (
       let uri = uri_of_g_uri node in
       let name = match List.rev (Rdf_uri.path uri) with [] -> assert false | name :: _ -> name in
       let uri = Rdf_uri.concat (Rdf_uri.parent uri) (Printf.sprintf "%s-%d" name cpt) in
       Inst uri
      )
    else
      node
  in
  let map_pair cpt node port =
    match node, port with
      Inst _, Flat _
    | Flat _, Inst _ -> assert false
    | node, _ ->
        let node2 = map cpt node in
        match compare_g_uri node node2 with
          0 -> (node, port)
        | _ ->
            let port2 = copy_flat_port ctx ~inst ~container: node2 port in
            (node2, port2)
  in
  let new_out_port =
    fun cpt state in_file ->
      let uri = Grdfs.uri_intf_out_port (uri_of_g_uri inst_node) cpt in
      record_file ctx reporter in_file (Inst uri);
      let port = Inst uri in
      (port,
       { state with port_to_file = Gurimap.add port in_file state.port_to_file }
      )
  in
  let rem_edge g i j data = Graph.rem g (i, j) (rem_pred data) in
  let f_succ cpt k g (succ,(p1,p2)) =
    let g = rem_edge g k succ (p1, p2) in
    let (k,p1) = map_pair cpt k p1 in
    let (succ,p2) = map_pair cpt succ p2 in
    Graph.add g (k,succ,(p1,p2))
  in
  let f_pred cpt out_port k state (pred,(p1,p2)) =
    let g = rem_edge state.g pred k (p1, p2) in
    let state = { state with g } in
    let (k,p2) = map_pair cpt k p2 in
    let (pred,p1) =
      if compare_g_uri pred inst_node = 0 then
        (* predecessor is the explode node, we add a the new port of inst_node
           as prececessor of p2 *)
         (pred, out_port)
      else
         map_pair cpt pred p1
    in
    { state with g = Graph.add state.g (pred,k,(p1,p2)) }
  in
  let f_node cpt in_file expl_node state =
    let (out_port, state) = new_out_port cpt state in_file in
    let preds = Graph.pred state.g expl_node in
    let succs = Graph.succ state.g expl_node in
    let state = List.fold_left (f_pred cpt out_port expl_node) state preds in
    let g = List.fold_left (f_succ cpt expl_node) state.g succs in
    { state with g }
  in
  let insert_graph (state, cpt) in_file =
    let state = Guriset.fold (f_node cpt in_file) exploded_nodes state in
    (state, cpt+1)
  in
  let (state,_) = List.fold_left insert_graph (state,1) in_files in
  let g = Guriset.fold (fun node g -> Graph.remove_node g node) exploded_nodes state.g in
  let state = { state with g } in
  state


(*

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
*)
;;

let run_implode ctx inst tmp_dir ~inst_node ~orig_node state =
  dbg ~level:1 (fun () -> Printf.sprintf "run_implode inst_node=%S" (g_uri_string inst_node));
  set_node_as_run state orig_node
;;

let rec run_node ctx reporter inst comb tmp_dir state orig_node =
  dbg ~level: 1
    (fun () -> Printf.sprintf "run_node %S" (g_uri_string orig_node));
  let inst_node =
    match orig_node with
      Flat _ -> new_inst_node ctx ~inst orig_node
    | Inst _ -> orig_node (* already instanciated *)
  in

  let orig_in_ports = in_ports state orig_node in
  (* even if these ports are not used to compute what we need,
    copying will register them as ports of inst_node *)
  let (_inst_in_ports, _port_map_in) =
    copy_flat_ports ctx ~inst ~container: inst_node orig_in_ports
  in
  let in_files = get_port_input_files ctx state orig_in_ports in

  let test file =
     let file = Filename.concat tmp_dir file in
     if not (Sys.file_exists file) then
      failwith (Printf.sprintf "File %s does not exist" file)
  in
  List.iter test in_files;

  let uri_from = Chn_flat.get_op_origin ctx (uri_of_g_uri orig_node) in
  let state =
    match uri_from with
    | _ when Rdf_uri.equal uri_from Grdfs.genet_explode ->
        run_explode ctx reporter inst tmp_dir ~inst_node ~orig_node state
    | _ when Rdf_uri.equal uri_from Grdfs.genet_implode ->
        run_implode ctx inst tmp_dir ~inst_node ~orig_node state
    | _ ->
        match Grdf_intf.intf_exists ctx.ctx_rdf uri_from with
          None -> assert false
        | Some _ ->
            match Grdf_intf.command_path ctx.ctx_rdf uri_from with
              None ->
                failwith
                (Printf.sprintf "No path for interface %s" (Rdf_uri.string uri_from))
            | Some path ->
                let orig_out_ports = out_ports state orig_node in
                let (inst_out_ports, port_map_out) =
                  copy_flat_ports ctx ~inst ~container: inst_node orig_out_ports
                in
                let (out_files, state) = new_files ctx tmp_dir state inst_out_ports in
                let g =
                  let f_succ g (succ, (src, dst)) =
                    try
                      let src2 = Gurimap.find src port_map_out in
                      let g = Graph.rem g (orig_node, succ) (rem_pred (src, dst)) in
                      Graph.add g (inst_node, succ, (src2, dst))
                    with Not_found -> g
                  in
                  let g = List.fold_left f_succ state.g (Graph.succ state.g orig_node) in
                  let f_pred g (pred, (src, dst)) =
                    let g = Graph.rem g (pred, orig_node) (rem_pred (src, dst)) in
                    Graph.add g (pred, inst_node, (src, dst))
                  in
                  let g = List.fold_left f_pred g (Graph.pred g orig_node) in
                  g
                in
                let state = { state with g } in
                let tool = Grdf_intf.tool_of_intf uri_from in
                let version = Urimap.find tool comb in
                let path = replace_version ctx path version in
                prerr_endline (Printf.sprintf "path = %s" path);
                Grdfs.set_start_date_uri ctx.ctx_rdf (uri_of_g_uri inst_node) ();
                begin
                  try
                    run_command ctx reporter tmp_dir path in_files inst_out_ports out_files;
                    Grdfs.set_stop_date_uri ctx.ctx_rdf (uri_of_g_uri inst_node) ();
                  with
                    e ->
                      Grdfs.set_stop_date_uri ctx.ctx_rdf (uri_of_g_uri inst_node) ();
                      raise e
                end;
                let g = match orig_node with
                    Flat _ -> Graph.remove_node g orig_node
                  | Inst _ -> g
                in
                let state = { state with g } in
                set_node_as_run state inst_node
  in
  run_nodes ctx reporter inst comb tmp_dir state

and run_nodes ctx reporter inst comb tmp_dir state =
  match runnable_nodes state with
    [] ->
      dbg ~level: 1 (fun () -> "run_nodes: no more runnable nodes");
      state
  | node :: q ->
      dbg ~level: 1
      (fun () ->
         Printf.sprintf "run_nodes: runnable nodes:\n%s"
         (String.concat "\n" (List.map g_uri_string (node::q)))
      );
      dbg ~level: 2
      (fun () ->
         let file = Printf.sprintf "/tmp/inst_run-%d.dot" (Guriset.cardinal state.nodes_run) in
         Misc.file_of_string ~file (dot_of_graph ctx state.g);
         Printf.sprintf "Graph generated in %S" file);
      run_node ctx reporter inst comb tmp_dir state node
;;

let run ctx reporter ~inst ~fchain input comb g =
  Grdfs.set_start_date_uri ctx.ctx_rdf inst ();

  let tmp_dir = Filename.temp_file "genet-run" ".dir" in
  Sys.remove tmp_dir;
  Misc.mkdir tmp_dir ;

  let state = init_run ctx reporter ~inst ~fchain input tmp_dir g  in
  let state = set_node_as_run state (Flat fchain) in
  let state = set_node_as_run state (Inst inst) in

  dbg ~level: 2
    (fun () ->
      let file = "/tmp/inst_init_run.dot" in
      Misc.file_of_string ~file (dot_of_graph ctx state.g);
      Printf.sprintf "Init run graph generated in %S" file);
  let state = run_nodes ctx reporter inst comb tmp_dir state in

  dbg ~level: 2
      (fun () ->
         let file = Printf.sprintf "/tmp/inst_run-%d.dot" (Guriset.cardinal state.nodes_run) in
         Misc.file_of_string ~file (dot_of_graph ctx state.g);
         Printf.sprintf "Graph generated in %S" file);

  let flat_out_ports =
    List.map (fun p -> Flat p) (Grdf_port.ports ctx.ctx_rdf fchain Grdf_port.Out)
  in
  let (inst_out_ports, port_map) = copy_flat_ports ctx ~inst ~container: (Inst inst) flat_out_ports in
  let out_files = get_port_input_files ctx state inst_out_ports in
  let out_files = List.map (Filename.concat tmp_dir) out_files in

  List.iter2 (record_file ctx reporter) out_files inst_out_ports;

  (* graph should be empty now *)
  Grdfs.set_stop_date_uri ctx.ctx_rdf inst ();
  let not = Graph.fold_pred state.g
    (fun node _ acc ->
       if Guriset.mem node state.nodes_run then acc else (g_uri_string node) :: acc)
    []
  in
  match not with
    [] -> ()
  | _ ->
      let msg = Printf.sprintf "The following nodes were not executed:\n%s"
        (String.concat "\n" not)
      in
      failwith msg
;;
