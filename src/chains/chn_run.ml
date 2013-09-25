(*********************************************************************************)
(*                Genet                                                          *)
(*                                                                               *)
(*    Copyright (C) 2012-2013 Institut National de Recherche en Informatique     *)
(*    et en Automatique. All rights reserved.                                    *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License version 3             *)
(*    or later as published by the Free Software Foundation.                     *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public License          *)
(*    along with this program; if not, write to the Free Software Foundation,    *)
(*    Inc., 59 Temple Place, Suite 330, Boston, MA                               *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*                                                                               *)
(*********************************************************************************)

(** Running commands to instanciate a chain. *)

open Grdf_types;;
open Chn_types;;

let dbg = Misc.create_log_fun
  ~prefix: "Chn_run"
    "GENET_CHN_RUN_DEBUG_LEVEL"
;;

let ichain_start_date ctx iri =
  match Grdfs.start_date_iri ctx.ctx_rdf iri with
    None -> None
  | Some d -> Some (Netdate.mk_mail_date (Netdate.since_epoch d))
;;

let ichain_stop_date ctx iri =
  match Grdfs.stop_date_iri ctx.ctx_rdf iri with
    None -> None
  | Some d -> Some (Netdate.mk_mail_date (Netdate.since_epoch d))
;;

let return_code ctx iri =
  match Grdfs.object_literal ctx.ctx_rdf
    ~sub: (Rdf_term.Iri iri) ~pred: Grdfs.genet_returncode
  with
    None -> 0
  | Some s ->
      try int_of_string s
      with _ ->
          failwith (Printf.sprintf "Invalid command return code %s" s)
;;

type g_iri = Flat of Rdf_iri.iri | Inst of Rdf_iri.iri;;
let compare_g_iri t1 t2 =
  match t1, t2 with
    Flat iri1, Flat iri2
  | Flat iri1, Inst iri2
  | Inst iri1, Flat iri2
  | Inst iri1, Inst iri2 -> Rdf_iri.compare iri1 iri2
;;
let compare_g_iri_pair   (p1, p2) (p3, p4) =
  match compare_g_iri p1 p3 with
    0 -> compare_g_iri p2 p4
  | n -> n
;;
let iri_of_g_iri = function Flat u -> u | Inst u -> u;;
let g_iri_string u = Rdf_iri.string (iri_of_g_iri u);;
module Giri_ord_type = struct type t = g_iri let compare = compare_g_iri end;;

module Giriset = Set.Make(Giri_ord_type)
module Girimap = Map.Make(Giri_ord_type)
module Graph = Graph.Make_with_map (Giri_ord_type)
  (struct
     type t = g_iri * g_iri
     let compare = compare_g_iri_pair
   end)
;;

let rec get_origin ctx iri =
  dbg ~level: 3 (fun() -> Printf.sprintf "get_origin iri=%S" (Rdf_iri.string iri));
  match Grdfs.object_iri ctx.Chn_types.ctx_rdf
    ~sub: (Rdf_term.Iri iri) ~pred: Grdfs.genet_opfrom
  with
    None -> iri
  | Some iri -> get_origin ctx iri
;;

let dot_of_graph ctx g =
  let f_edge (p1, p2) =
    let kind = function Inst _ -> "(i)" | Flat _ -> "(f)" in
    let k1 = kind p1 and k2 = kind p2 in
    let p1 = iri_of_g_iri p1 in
    let p2 = iri_of_g_iri p2 in
    let type1 = Grdf_port.port_type ctx.ctx_rdf (get_origin ctx p1) in
    let type2 = Grdf_port.port_type ctx.ctx_rdf (get_origin ctx p2) in
    let f p = Grdf_port.string_of_port_type (fun x -> x) p in
    let label = Printf.sprintf "%s%s:%s%s" k1 (f type1) k2 (f type2) in
    (label, [])
  in
  let f_node iri =
    let color = match iri with Flat _ -> "orange" | Inst _ -> "green" in
    let iri = iri_of_g_iri iri in
    let label =
      try
        let iri_from = Chn_flat.get_op_origin ctx iri in
        match Grdf_intf.intf_exists ctx.ctx_rdf iri_from with
          None -> Filename.basename (Rdf_iri.string iri)
        | Some name -> name
      with
        _ ->
          Filename.basename (Rdf_iri.string iri)
    in
    let href = Rdf_iri.string iri in
    let id = "n"^(Digest.to_hex (Digest.string href)) in
    (id, label, [ "href", href ; "style", "filled"; "fillcolor", color])
  in
  Graph.dot_of_graph ~f_edge ~f_node g
;;

let extract_git_file config ~file ~id ~target =
  let data_dir = Config.data_dir config in
  let base_dir = Fname.path_under ~parent: data_dir file in
  let target_dir = Filename.quote (Filename.dirname (Fname.abs_string target)) in
  let target_base = Filename.quote (Filename.basename (Fname.abs_string target)) in
  let com = Printf.sprintf
    "mkdir -p %s/.tmp; \
    (cd %s ; git archive --format tar %s %s| (cd %s/.tmp ; tar xvf -)) ; \
    mv %s/.tmp/%s %s/%s ; \
    rm -fr %s/.tmp/"
    target_dir
    (Fname.quote data_dir) id (Fname.rel_quote base_dir) target_dir
    target_dir (Fname.rel_quote base_dir) target_dir target_base
    target_dir
  in
  dbg ~level:3 (fun () -> Printf.sprintf "extract_git_file: %s" com);
  match Sys.command com with
    0 -> ()
  | n ->
     let msg = Printf.sprintf "Command failed [%d]: %s" n com in
     failwith msg
;;

let filename_of_md5 ctx md5 =
  Fname.concat_s (Config.out_dir ctx.ctx_cfg) md5
;;

let record_file ctx reporter ?(pred=Grdfs.genet_filemd5) file iri =
  let file_s = Fname.abs_string file in
  if not (Sys.file_exists file_s) then
    failwith (Printf.sprintf "File %S not found (pwd=%s)" file_s (Sys.getcwd()));
  let md5 =
    match (Unix.stat file_s).Unix.st_kind with
      Unix.S_DIR -> Misc.dir_md5sum file_s
    | Unix.S_REG -> Misc.file_md5sum file_s
    | _ -> failwith (Printf.sprintf "Invalid file kind for %S" file_s)
  in
  let outfile = Fname.concat_s (Config.out_dir ctx.ctx_cfg) md5 in
  if not (Sys.file_exists (Fname.abs_string outfile)) then
    Misc.copy_file ~src: file_s ~dst: (Fname.abs_string outfile);
  Grdfs.add_triple ctx.ctx_rdf
    ~sub: (Rdf_term.Iri (iri_of_g_iri iri)) ~pred
    ~obj: (Rdf_term.term_of_literal_string md5)
;;

let compute_version_replacement ctx comb =
  let (versions, comb) = Rdf_iri.Irimap.fold
    (fun iri version (versions, comb) ->
      let s = Printf.sprintf "%%{version-%s}"
         (Grdf_tool.name ctx.ctx_rdf iri)
       in
       let re = Str.regexp_string s in
       let v = Grdf_version.name ctx.ctx_rdf version in
       ((re, v) :: versions, Rdf_iri.Irimap.add iri v comb)
    )
    comb
    ([], Rdf_iri.Irimap.empty)
  in
  fun default_tool s ->
    let version = Rdf_iri.Irimap.find default_tool comb in
    let versions = (Str.regexp_string "%v", version) :: versions in
    List.fold_right (fun (re,v) s -> Str.global_replace re v s) versions s
;;

type state =
  { g : Graph.t ;
    port_to_file : [`Absolute] Fname.filename Girimap.t ;
    nodes_run : Giriset.t ;
    replace_versions : Rdf_iri.iri -> string -> string ;
  }

exception Exec_failed of state * iri * g_iri * string * int
  (** state * inst_chain * inst_node * command * return code *)

let sort_ports =
  let comp p1 p2 =
    Pervasives.compare
    (Grdf_port.port_rank (iri_of_g_iri p1))
    (Grdf_port.port_rank (iri_of_g_iri p2))
  in
  List.sort comp
;;

let in_ports state node =
  let preds = Graph.pred state.g node in
  sort_ports
  (Giriset.elements
   (List.fold_left (fun set (_,(_,p)) -> Giriset.add p set) Giriset.empty preds)
  )
;;
let out_ports state node =
  let preds = Graph.succ state.g node in
  sort_ports
  (Giriset.elements
   (List.fold_left (fun set (_,(p,_)) -> Giriset.add p set) Giriset.empty preds)
  )
;;

let run_command ctx (reporter: Reporter.reporter)
  state inst_chain tmp_dir inst_node path in_files inst_out_ports out_files =

  let out_file = Fname.absolute (Filename.temp_file "genet" "run_command.out") in
  let com = Printf.sprintf
    "(cd %s ; %s %s %s) > %s 2>&1"
    (Fname.quote tmp_dir)
    path
    (String.concat " " (List.map Fname.quote in_files))
    (String.concat " " (List.map Fname.quote out_files))
    (Fname.quote out_file)
  in
  let msg = "Running " ^ com in
  dbg ~level: 2 (fun () -> msg);
  reporter#msg msg;
  let link_output ?(error=false) () =
    record_file ctx reporter
      ~pred: (Grdfs.genet_commandoutput) out_file inst_node;
    let contents = Misc.string_of_file (Fname.abs_string out_file) in
    if error then reporter#error contents else reporter#msg contents
  in
  match Sys.command com with
    0 ->
      List.iter2
        (fun port file ->
           record_file ctx reporter file port)
        inst_out_ports out_files;
      link_output ()
  | n ->
      let msg = Printf.sprintf "command failed [%d]: %s" n com in
      dbg ~level: 1 (fun () -> msg);
      Grdfs.add_triple ctx.ctx_rdf
        ~sub: (Rdf_term.Iri (iri_of_g_iri inst_node))
        ~pred: Grdfs.genet_returncode
        ~obj: (Rdf_term.term_of_literal_string ~typ: Grdfs.datatype_integer (string_of_int n));
      link_output ~error: true ();
      raise (Exec_failed (state, inst_chain, inst_node, com, n))
;;
let gen_file =
  let cpt = ref 0 in
  fun ?ext () ->
    incr cpt;
    Fname.relative (Printf.sprintf "file%d%s" !cpt (match ext with None -> "" | Some s -> "."^s))
;;

let copy_flat_port ctx ~inst ~container ?cpt flat_port =
  let flat_port = iri_of_g_iri flat_port in
  let container = iri_of_g_iri container in
  let inst_port = Chn_types.iri_inst_port_of_flat_port ~ichain: true ?cpt
    ctx ~inst ~flat: flat_port
  in
  let ptype = Grdf_port.port_type ctx.ctx_rdf flat_port in
  Grdf_port.set_port_type ctx.ctx_rdf inst_port ptype;
  Grdfs.add_triple_iris ctx.ctx_rdf
    ~sub: inst_port ~pred: Grdfs.genet_opfrom ~obj: flat_port;

  let dir = Grdf_port.port_dir flat_port in
  let pred = Grdf_port.pred_of_dir dir in
  Grdfs.add_triple_iris ctx.ctx_rdf ~sub: container ~pred ~obj: inst_port;
  Inst inst_port
;;

(* we make sure to keep the order of flat_ports when producing inst_ports *)
let copy_flat_ports ctx ~inst ~container ?cpt flat_ports =
  let f flat_port (inst_ports, port_map) =
    let inst_port = copy_flat_port ctx ~inst ~container ?cpt flat_port in
    (inst_port :: inst_ports, Girimap.add flat_port inst_port port_map)
  in
  List.fold_right f flat_ports ([], Girimap.empty)
;;

(* do not create a new file if one is already associated
  to the port; this is useful for predecessors of implode
  operations, which are created by [run_explode] operations
  so that they output their result in the implode directory. *)
let new_file ctx tmp_dir ?path state inst_port =
  try (Girimap.find inst_port state.port_to_file, state)
  with Not_found ->
      dbg ~level: 2 (fun () -> Printf.sprintf "New file for port %S" (g_iri_string inst_port));
      let (ext, is_dir) =
        let typ = Grdf_port.port_type ctx.ctx_rdf (iri_of_g_iri inst_port) in
        match typ with
          Grdf_types.T s ->
            let iri = Grdfs.iri_filetype ctx.ctx_cfg.Config.rest_api s in
            (Some (Grdf_ftype.extension ctx.ctx_rdf iri), false)
        | Var _ -> None, false
        | Set _ | Tuple _ -> (None, true)
      in
      let file = gen_file ?ext () in
      let file =
        match path with
          None -> Fname.concat tmp_dir file
        | Some p -> Fname.concat p file
      in
      if is_dir then Misc.mkdir (Fname.abs_string file);
      let state = { state with port_to_file = Girimap.add inst_port file state.port_to_file } in
      (file, state)
;;

(* we make sure to keep order of inst_ports in the returned file list *)
let new_files ctx tmp_dir ?path state inst_ports =
  let f inst_port (files, state) =
    let (file, state) = new_file ctx tmp_dir ?path state inst_port in
    (file :: files, state)
  in
  List.fold_right f inst_ports ([], state)
;;

let runnable_nodes state =
  let g = Giriset.fold (fun node g -> Graph.remove_node g node)
    state.nodes_run state.g
  in
  Graph.pred_roots g
;;

let set_node_as_run state node =
  { state with nodes_run = Giriset.add node state.nodes_run }
;;

let rem_pred data x = compare_g_iri_pair data x = 0 ;;

let new_inst_node ctx ~inst ?cpt node =
  dbg ~level: 1 (fun () -> Printf.sprintf "new_inst_node node=%S" (g_iri_string node));
  let node = iri_of_g_iri node in

  let inst_node = Chn_types.iri_inst_opn_of_flat_opn ~ichain: true ?cpt
      ~prefix: ctx.ctx_cfg.Config.rest_api ~inst ~flat: node
  in
  Grdfs.add_triple_iris ctx.ctx_rdf
  ~sub: inst_node ~pred: Grdfs.genet_opfrom ~obj: node;

  if not (Rdf_iri.equal inst inst_node) then
    (
     Chn_flat.add_containsop ctx ~src: inst ~dst: inst_node;
     Grdfs.add_type ctx.ctx_rdf
     ~sub: (Rdf_term.Iri inst_node) ~obj: (Rdf_term.Iri Grdfs.genet_instopn)
    );
  Inst inst_node
;;

let string_of_giriset set = String.concat "\n"
  (Giriset.fold (fun x acc -> (g_iri_string x) :: acc) set [])
;;

let copy_node ctx ~inst ?cpt orig_node ?inports ?outports state =
  dbg ~level: 1
  (fun () -> Printf.sprintf "copying node %S (cpt=%s)"
     (g_iri_string orig_node) (match cpt with None -> "_" | Some n -> string_of_int n));
  let in_ports = match inports with None -> in_ports state orig_node | Some l -> l in
  let out_ports = match outports with None -> out_ports state orig_node | Some l -> l in
  let new_node = new_inst_node ctx ~inst ?cpt orig_node in
  let (in_ports, port_map_in) = copy_flat_ports ctx ~inst ~container: new_node ?cpt in_ports in
  let (out_ports, port_map_out) = copy_flat_ports ctx ~inst ~container: new_node ?cpt out_ports in
  let f_succ cpt g (succ,(p1,p2)) =
    try
      let p1 =
        try Girimap.find p1 port_map_out
        with Not_found -> Girimap.find p1 port_map_in
      in
      Graph.add g (new_node,succ,(p1,p2))
    with Not_found -> g

  in
  let f_pred cpt g (pred,(p1,p2)) =
    try
      let p2 =
        try Girimap.find p2 port_map_in
        with Not_found -> Girimap.find p2 port_map_out
      in
      Graph.add g (pred,new_node,(p1,p2))
    with Not_found -> g
  in
  let preds = Graph.pred state.g orig_node in
  dbg ~level: 2 (fun () -> Printf.sprintf "Copying %d in ports" (List.length in_ports));
  let succs = Graph.succ state.g orig_node in
  let g = List.fold_left (f_pred cpt) state.g preds in
  let g = List.fold_left (f_succ cpt) g succs in
  (new_node, port_map_in, port_map_out, { state with g })
;;

let init_run ctx reporter comb ~inst ~fchain input tmp_dir g =
  let in_files = input.Ind_types.in_files in
  dbg ~level: 1 (fun () -> Printf.sprintf "inst = %S" (Rdf_iri.string inst));
  let state = {
      g ;
      port_to_file = Girimap.empty ;
      nodes_run = Giriset.empty ;
      replace_versions = compute_version_replacement ctx comb ;
    }
  in

  let (inst_node,_,_,state) = copy_node ctx ~inst (Flat fchain) state in
  let inst_in_ports = out_ports state inst_node in
  dbg ~level: 2
    (fun () -> Printf.sprintf "run_init: in_ports =\n%s"
      (String.concat "\n" (List.map g_iri_string inst_in_ports))
    );

  let nb_in_files = List.length in_files in
  let nb_in_ports = List.length inst_in_ports in
  if nb_in_files <> nb_in_ports then
    failwith
    (Printf.sprintf "Numbers of input files (%d) and ports (%d) differ."
      nb_in_files nb_in_ports);

  let state = { state with g = Graph.remove_node state.g (Flat fchain) } in
  let f state (in_file, id) inst_port =
    let (target, state) = new_file ctx tmp_dir state inst_port in
    let file = Fname.concat input.Ind_types.dir in_file in
    extract_git_file ctx.ctx_cfg ~file ~id ~target;
    record_file ctx reporter target inst_port;
    state
  in
  List.fold_left2 f state in_files inst_in_ports
;;

let get_port_input_file ctx state port =
  let node = Grdfs.port_container (iri_of_g_iri port) in
  dbg ~level: 1
  (fun () -> Printf.sprintf "get_port_input_file port=%S iri=%S" (g_iri_string port) (Rdf_iri.string node));
  let src =
    let preds =
      try Graph.pred state.g (Inst node)
      with Not_found ->
          try Graph.pred state.g (Flat node)
          with Not_found ->
              failwith (Printf.sprintf "port container %S not in graph" (g_iri_string port))
    in
    let (_,(src,_)) =
      try List.find
        (fun (_,(src,dst)) ->
          (*prerr_endline (Printf.sprintf "src=%S\ndst=%S" (g_iri_string src) (g_iri_string dst)) ;*)
          compare_g_iri dst port = 0) preds
      with Not_found ->
          failwith (Printf.sprintf "No ancestor found for port %S" (g_iri_string port))
    in
    src
  in
  try Girimap.find src state.port_to_file
  with Not_found ->
      failwith (Printf.sprintf "No file for inst port %S" (g_iri_string src))
;;

let get_port_input_files ctx state ports =
  List.map (get_port_input_file ctx state) ports
;;


let copy_expl_node ctx ~inst cpt orig_pred_node pred_node pred_out_port state expl_node =
  let (new_expl_node, _, _, state) = copy_node ctx ~inst ~cpt expl_node state in
  let f_pred cpt g (pred,(p1,p2)) =
    if compare_g_iri pred orig_pred_node = 0 then
      (
       let g = Graph.rem g (pred,new_expl_node) (rem_pred (p1,p2)) in
       Graph.add g (pred_node, new_expl_node, (pred_out_port, p2))
      )
    else
      g
  in
  let preds = Graph.pred state.g new_expl_node in
  let g = List.fold_left (f_pred cpt) state.g preds in
  dbg ~level: 2
  (fun () -> Printf.sprintf "%S: %d in ports" (g_iri_string new_expl_node)
     (List.length (in_ports state new_expl_node)));
  (new_expl_node, { state with g })
;;

let add_implode_ports ctx inst_implode orig_impl_in_port tmp_dir impl_out_file cpt state expl_nodes =
  let out_port = match out_ports state inst_implode with
    [p] -> p | _ -> assert false
  in
  let (in_type, path) =
    match Grdf_port.port_type ctx.ctx_rdf (iri_of_g_iri out_port) with
    | Var _ | T _ | Tuple _ -> assert false
    | Set t ->
        match t with
        | Var _ | T _ -> (t, None)
        | Set _ | Tuple _ -> (t, Some (Fname.concat_s tmp_dir (string_of_int cpt)))
  in
  let in_port =
    let iri = Grdfs.iri_intf_in_port (iri_of_g_iri inst_implode) cpt in
    Grdf_port.set_port_type ctx.ctx_rdf iri in_type;
    let pred = Grdf_port.pred_of_dir Grdf_port.In in
    Grdfs.add_triple_iris ctx.ctx_rdf ~sub: (iri_of_g_iri inst_implode) ~pred ~obj: iri;
    Grdfs.add_triple_iris ctx.ctx_rdf ~sub: iri
    ~pred: Grdfs.genet_opfrom ~obj: (iri_of_g_iri orig_impl_in_port);
    Inst iri
  in
  let state = { state with port_to_file = Girimap.add in_port impl_out_file state.port_to_file } in
  let state =
    List.fold_left
    (fun state expl_node ->
       let out_ports = out_ports state expl_node in
       let g = List.fold_left
         (fun g p -> Graph.add g (expl_node,inst_implode,(p,in_port))) state.g out_ports
       in
       let state = { state with g } in
       let (_, state) = new_files ctx tmp_dir ?path state out_ports in
       state
    )
    state
    expl_nodes
  in
  state
;;

let run_explode ctx reporter inst tmp_dir ~orig_node state =
  dbg ~level:1 (fun () -> Printf.sprintf "run_explode orig_node=%S" (g_iri_string orig_node));
  let orig_in_port =
    match in_ports state orig_node with
      [p] -> p
    | [] -> assert false
    | _ -> assert false
  in
  let orig_out_port =
    match out_ports state orig_node with
      [p] -> p
    | [] -> assert false
    | _ -> assert false
  in
  let (inst_node, _, _, state) = copy_node ctx ~inst orig_node ~outports: [] state in
  dbg ~level:1 (fun () -> Printf.sprintf "run_explode inst_node=%S" (g_iri_string inst_node));
  let inst_in_port = match in_ports state inst_node with
    [p] -> p | [] -> assert false | _ -> assert false
  in
  let root = get_port_input_file ctx state orig_in_port in
  let in_files =
    let root_s = Fname.abs_string root in
     let entries = Find.find_list Find.Ignore [root_s] [Find.Maxdepth 1] in
     let entries = List.filter ((<>) root_s) entries in
     List.map Fname.absolute entries
  in
  let iri_implode =
    match Grdfs.object_iri ctx.ctx_rdf
      ~sub: (Rdf_term.Iri (iri_of_g_iri orig_node)) ~pred: Grdfs.genet_hasimplode
    with
      None -> failwith "No implode node associated to explode!"
    | Some iri -> iri
  in
  let exploded_nodes =
    (* remove main node to prevent cycling and getting all nodes *)
    let g = Graph.remove_node state.g (Inst inst) in
    (* FIXME: maybe a problem here in case of nested explodes: how to find the correct
       implode, which may be a Inst node now ? *)
    let after_implode =
      let l = Graph.recursive_succs g (Flat iri_implode) in
      List.fold_right Giriset.add (Flat iri_implode :: l) Giriset.empty
    in
    let after_explode =
      let l = Graph.recursive_succs g orig_node in
      List.fold_right Giriset.add l Giriset.empty
    in
    dbg ~level: 1
    (fun () -> Printf.sprintf "after_implode=\n%s\nafter_explode=\n%s"
       (string_of_giriset after_implode) (string_of_giriset after_explode));
    Giriset.diff after_explode after_implode
  in
  assert (Giriset.cardinal exploded_nodes > 0);

  let f_pred g (pred,(p1,p2)) =
    if compare_g_iri p2 inst_in_port = 0 then
      Graph.add g (pred,inst_node, (p1,inst_in_port))
    else
      Graph.add g (pred,inst_node, (p1,p2))
  in
  let g = List.fold_left f_pred state.g (Graph.pred state.g orig_node) in
  let state = { state with g } in

  (* FIXME: handle nested foreach: implode may already be an inst node *)
  let orig_implode = Flat iri_implode in
  let (inst_implode, map_imp_in, map_imp_out, state) =
    copy_node ctx ~inst orig_implode ~inports: [] state
  in
  (* add a filename for implode output *)
  let impl_out = match out_ports state inst_implode with
      [p] -> p | [] -> assert false | _ -> assert false
  in
  let (impl_out_file, state) = new_file ctx tmp_dir state impl_out in
  let orig_impl_in_port = match in_ports state orig_implode with
      [p] -> p | _ ->  assert false
  in

  let new_out_port =
    fun cpt state in_file ->
      let iri = Grdfs.iri_intf_out_port (iri_of_g_iri inst_node) cpt in
      Grdfs.add_triple_iris ctx.ctx_rdf
      ~sub: iri ~pred: Grdfs.genet_opfrom ~obj: (iri_of_g_iri orig_out_port);

      let pred = Grdf_port.pred_of_dir Grdf_port.Out in
      Grdfs.add_triple_iris ctx.ctx_rdf ~sub: (iri_of_g_iri inst_node) ~pred ~obj: iri;

      record_file ctx reporter in_file (Inst iri);
      let port = Inst iri in
      (port,
       { state with port_to_file = Girimap.add port in_file state.port_to_file }
      )
  in

  let f_node cpt in_file expl_node (expl_nodes, state) =
    let (out_port, state) = new_out_port cpt state in_file in
    let (new_expl_node, state) =
      copy_expl_node ctx ~inst cpt orig_node inst_node out_port state expl_node
    in
    (new_expl_node :: expl_nodes, state)
  in
  let insert_graph (state, cpt) in_file =
    let (expl_nodes, state) = Giriset.fold (f_node cpt in_file) exploded_nodes ([], state) in
    let state = add_implode_ports
      ctx inst_implode orig_impl_in_port
      tmp_dir impl_out_file cpt state expl_nodes
    in
    (state, cpt+1)
  in
  let (state,_) = List.fold_left insert_graph (state,1) in_files in
  let g = Giriset.fold (fun node g -> Graph.remove_node g node) exploded_nodes state.g in
  let state = { state with g } in
  let state = set_node_as_run state inst_node in
  let g = Graph.remove_node g orig_node in
  { state with g }
;;

(* implode only gather input files to output directory;
  it also acts as a synchronisation point before allowing successors to run. *)
let run_implode ctx reporter inst tmp_dir inst_node state =
  dbg ~level:1 (fun () -> Printf.sprintf "run_implode inst_node=%S" (g_iri_string inst_node));
  let in_files = get_port_input_files ctx state (in_ports state inst_node) in
  let (out_port, out_file, state) =
    match out_ports state inst_node with
      [p] ->
        let (file, state) = new_file ctx tmp_dir state p in
        Misc.mkdir (Fname.abs_string file);
        (p, file, state)
     | [] -> assert false | _ -> assert false
   in
  let path = Printf.sprintf "cp -fr" in
  run_command ctx reporter state inst tmp_dir inst_node path in_files [out_port] [out_file];
  set_node_as_run state inst_node
;;

let rec run_node ctx reporter inst tmp_dir state orig_node =
  let msg = Printf.sprintf "run_node %S" (g_iri_string orig_node) in
  dbg ~level: 1 (fun () -> msg);
  reporter#push_context msg;

  let error msg =
    reporter#error msg;
    failwith msg
  in

  let orig_in_ports = in_ports state orig_node in

  let in_files = get_port_input_files ctx state orig_in_ports in
  dbg ~level: 2
  (fun () -> Printf.sprintf "run_node: %d in_files for %S"
     (List.length in_files) (g_iri_string orig_node));

  let test file =
     let file_s = Fname.abs_string file in
     if not (Sys.file_exists file_s) then
       error (Printf.sprintf "File %S does not exist" file_s)
  in
  List.iter test in_files;

  let iri_from = Chn_flat.get_op_origin ctx (iri_of_g_iri orig_node) in
  let state =
    match iri_from with
    | _ when Rdf_iri.equal iri_from Grdfs.genet_explode ->
        run_explode ctx reporter inst tmp_dir ~orig_node state
    | _ when Rdf_iri.equal iri_from Grdfs.genet_implode ->
        run_implode ctx reporter inst tmp_dir orig_node state
    | _ ->
        match Grdf_intf.intf_exists ctx.ctx_rdf iri_from with
          None -> assert false
        | Some _ ->
            match Grdf_intf.command_path ctx.ctx_rdf iri_from with
              None ->
                error ("No path for interface " ^ (Rdf_iri.string iri_from))
            | Some path ->
                let (inst_node, port_map_in, port_map_out, state) =
                 (* FIXME: handled correctly in case of nested foreach *)
                  match orig_node with
                    Flat _ -> copy_node ctx ~inst orig_node state
                  | Inst _ ->
                      (* already instanciated *)
                      (orig_node,
                       List.fold_left (fun acc p -> Girimap.add p p acc) Girimap.empty (in_ports state orig_node),
                       List.fold_left (fun acc p -> Girimap.add p p acc) Girimap.empty (out_ports state orig_node),
                       state)
                in
                let inst_out_ports = out_ports state inst_node in
                let (out_files, state) = new_files ctx tmp_dir state inst_out_ports in
                let g =
                  let f_succ g (succ, (src, dst)) =
                    try
                      let src2 = Girimap.find src port_map_out in
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
                let tool = Grdf_intf.tool_of_intf iri_from in
                let path = state.replace_versions tool path in
                (*prerr_endline (Printf.sprintf "path = %s" path);*)
                Grdfs.set_start_date_iri ctx.ctx_rdf (iri_of_g_iri inst_node) ();
                begin
                  try
                    run_command ctx reporter state inst tmp_dir inst_node path in_files inst_out_ports out_files;
                    Grdfs.set_stop_date_iri ctx.ctx_rdf (iri_of_g_iri inst_node) ();
                  with
                    e ->
                      Grdfs.set_stop_date_iri ctx.ctx_rdf (iri_of_g_iri inst_node) ();
                      raise e
                end;
                let g = match orig_node with
                    Flat _ -> Graph.remove_node g orig_node
                  | Inst _ -> g
                in
                let state = { state with g } in
                set_node_as_run state inst_node
  in
  reporter#pop_context;
  run_nodes ctx reporter inst tmp_dir state

and run_nodes ctx reporter inst tmp_dir state =
  match runnable_nodes state with
    [] ->
      dbg ~level: 1 (fun () -> "run_nodes: no more runnable nodes");
      state
  | node :: q ->
      dbg ~level: 1
      (fun () ->
         Printf.sprintf "run_nodes: runnable nodes:\n%s"
         (String.concat "\n" (List.map g_iri_string (node::q)))
      );
      dbg ~level: 2
      (fun () ->
         let file = Printf.sprintf "/tmp/inst_run-%d.dot" (Giriset.cardinal state.nodes_run) in
         Misc.file_of_string ~file (dot_of_graph ctx state.g);
         Printf.sprintf "Graph generated in %S" file);
      run_node ctx reporter inst tmp_dir state node
;;

let run ctx reporter ~inst ~fchain input comb g =
  Grdfs.set_start_date_iri ctx.ctx_rdf inst ();

  let tmp_dir = Fname.absolute (Filename.temp_file "genet-run" ".dir") in
  Sys.remove (Fname.string tmp_dir);
  Misc.mkdir (Fname.string tmp_dir) ;

  let state = init_run ctx reporter comb ~inst ~fchain input tmp_dir g  in
  let state = set_node_as_run state (Flat fchain) in
  let state = set_node_as_run state (Inst inst) in

  dbg ~level: 2
    (fun () ->
      let file = "/tmp/inst_init_run.dot" in
      Misc.file_of_string ~file (dot_of_graph ctx state.g);
      Printf.sprintf "Init run graph generated in %S" file);
  let (state, run_ok) =
    try (run_nodes ctx reporter inst tmp_dir state, true)
    with Exec_failed (state, _, inst_node, _, _) ->
        Grdfs.add_triple_iris ctx.ctx_rdf
        ~sub: inst
        ~pred: Grdfs.genet_failedcommand
        ~obj: (iri_of_g_iri inst_node);
        Grdfs.set_stop_date_iri ctx.ctx_rdf inst ();
        (state, false)
  in

  dbg ~level: 2
      (fun () ->
         let file = Printf.sprintf "/tmp/inst_run-%d.dot" (Giriset.cardinal state.nodes_run) in
         Misc.file_of_string ~file (dot_of_graph ctx state.g);
         Printf.sprintf "Graph generated in %S" file);

  if run_ok then
    begin
      let flat_out_ports =
        List.map (fun p -> Flat p) (Grdf_port.ports ctx.ctx_rdf fchain Grdf_port.Out)
      in
      let (inst_out_ports, port_map) = copy_flat_ports ctx ~inst ~container: (Inst inst) flat_out_ports in
      let out_files = get_port_input_files ctx state inst_out_ports in
(*      let out_files = List.map (Filename.concat tmp_dir) out_files in*)

      List.iter2 (record_file ctx reporter) out_files inst_out_ports;
    end;
  Grdfs.set_stop_date_iri ctx.ctx_rdf inst ();

  (* add "consumes" links in database between instanciated ports of the final graph,
    to be able to display correctly the execution graph *)
  let f_edge (_, (p1, p2)) =
    match p1, p2 with
      Inst sub, Inst obj ->
        Grdfs.add_triple_iris ctx.ctx_rdf ~sub ~pred: Grdfs.genet_produces ~obj
    | _ -> ()
  in
  let f_succ _ l = List.iter f_edge l in
  Graph.iter_succ state.g f_succ;

  (* graph should be empty now if no error *)
  if run_ok then
    begin
      let not_executed = Graph.fold_pred state.g
        (fun node _ acc ->
           if Giriset.mem node state.nodes_run then acc else (g_iri_string node) :: acc)
        []
      in
      match not_executed with
        [] -> ()
      | _ ->
          let msg = Printf.sprintf "The following nodes were not executed:\n%s"
            (String.concat "\n" not_executed)
          in
          failwith msg
    end
;;
