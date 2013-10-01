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

(** *)

open Rdf_graph;;
open Grdf_types;;
open Chn_types;;

module Iriset = Rdf_iri.Iriset
module Irimap = Rdf_iri.Irimap

let dbg = Misc.create_log_fun
  ~prefix: "Chn_inst"
    "GENET_CHN_INST_DEBUG_LEVEL"
;;
let instance_source ctx inst_iri =
  Grdfs.object_iri ctx.ctx_rdf
    ~sub: (Rdf_term.Iri inst_iri)
    ~pred: Grdfs.genet_instanciate
;;

let instport_file_iri ctx iri =
  match Grdfs.object_literal ctx.Chn_types.ctx_rdf
    ~sub: (Rdf_term.Iri iri) ~pred: Grdfs.genet_filemd5
  with
  | Some md5 ->
      Some (Grdfs.iri_outfile_path ctx.Chn_types.ctx_cfg.Config.rest_api [md5])
  | None ->
      match Chn_flat.port_producers ctx iri with
        [] -> None
      | p :: _ ->
          match
            Grdfs.object_literal ctx.Chn_types.ctx_rdf
              ~sub: (Rdf_term.Iri p) ~pred: Grdfs.genet_filemd5
          with
          | Some md5 ->
              Some (Grdfs.iri_outfile_path ctx.Chn_types.ctx_cfg.Config.rest_api [md5])
          | None -> None
;;

let version_combinations ctx fchain =
  let intfs = Chn_flat.intfs_of_flat_chain ctx fchain in
  let intfs_by_tool = Iriset.fold
    (fun intf acc ->
       let tool = Grdf_intf.tool_of_intf intf in
       let set =
         try Irimap.find tool acc
         with Not_found -> Iriset.empty
       in
       let set = Iriset.add intf set in
       Irimap.add tool set acc
    )
      intfs Irimap.empty
  in
  Chn_flat.check_tool_intfs ctx fchain;
  let intf_tools = Irimap.fold (fun tool intfs acc -> (tool, intfs) :: acc)
    intfs_by_tool []
  in
  let rec f = function
    [] -> []
  | (tool, required_intfs) :: q ->
      dbg ~level: 2 (fun () ->
         Printf.sprintf "required intfs for tool %s:\n%s"
           (Rdf_iri.string tool)
           (String.concat "\n"
            (Iriset.fold (fun iri acc -> (Rdf_iri.string iri)::acc) required_intfs []))
      );
      let active_versions = Grdf_version.active_versions_of
        ctx.ctx_rdf ~recur: true tool
      in
      dbg ~level: 2 (fun () -> Printf.sprintf "active_versions_of %s: %d"
         (Rdf_iri.string tool) (List.length active_versions));
      let combs = f q in
      let f_version acc version =
        (* keep only versions implementing all the required interfaces *)
        let implemented =
          let (explicit, explicit_no, inherited) = Grdf_intf.compute_intfs_of ctx.ctx_rdf version in
          Iriset.diff (Iriset.union explicit inherited) explicit_no
        in
        dbg ~level:2
          (fun () -> Printf.sprintf "Implemented:\n%s"
             (String.concat "\n"
              (Iriset.fold (fun iri acc -> (Rdf_iri.string iri) :: acc) implemented []))
          );
        if Iriset.for_all
          (fun intf -> Iriset.exists (Rdf_iri.equal intf) implemented)
            required_intfs
        then
          match combs with
            [] ->
              (Irimap.singleton tool version) :: acc
          | _ ->
              (List.map (fun comb -> Irimap.add tool version comb) combs) @ acc
        else
          acc
      in
      List.fold_left f_version [] active_versions
  in
  f intf_tools
;;

let inst_input ctx iri_inst =
  let input = Grdfs.object_literal ctx.ctx_rdf
    ~sub: (Rdf_term.Iri iri_inst) ~pred: Grdfs.genet_useinput
  in
  let input_id = Grdfs.object_literal ctx.ctx_rdf
    ~sub: (Rdf_term.Iri iri_inst) ~pred: Grdfs.genet_useinputcommitid
  in
  match input, input_id with
    None, _ | _, None -> None
  | Some s, Some id -> Some (Fname.relative s, id)
;;

let equal_tool_versions = Irimap.equal Rdf_iri.equal;;

let inst_versions ctx iri_inst =
  let versions = Grdfs.object_iris ctx.ctx_rdf
    ~sub: (Rdf_term.Iri iri_inst) ~pred: Grdfs.genet_usetoolversion
  in
  List.fold_left
    (fun acc v -> Irimap.add
       (Grdf_version.tool_of_version v)
         v
         acc)
    Irimap.empty
    versions
;;

let instances ctx iri_fchain =
  let insts = Grdfs.subject_iris ctx.ctx_rdf
    ~pred: Grdfs.genet_instanciate ~obj: (Rdf_term.Iri iri_fchain)
  in
  let f acc iri_i =
    let versions = inst_versions ctx iri_i in
    let input = inst_input ctx iri_i in
    (iri_i, versions, input) :: acc
  in
  List.fold_left f [] insts
;;

let set_input_info ctx iri_inst input =
  let input_name = input.Ind_types.from_in_data in
  let input_id = Misc.get_git_id (Fname.abs_string input.Ind_types.dir) in

  let sub = Rdf_term.Iri iri_inst in

  let pred = Grdfs.genet_useinput in
  let obj = Rdf_term.term_of_literal_string (Fname.rel_string input_name) in
  Grdfs.add_triple ctx.ctx_rdf ~sub ~pred ~obj;

  let pred = Grdfs.genet_useinputcommitid in
  let obj = Rdf_term.term_of_literal_string input_id in
  Grdfs.add_triple ctx.ctx_rdf ~sub ~pred ~obj;
;;

(** @todo[3] This could be rewritten when OCaml-RDF offers a
   Sparql implementation. *)
let inst_chain_exists ctx iri_fchain input comb =
  let insts = instances ctx iri_fchain in
  let input =
    (input.Ind_types.from_in_data,
     Misc.get_git_id (Fname.abs_string input.Ind_types.dir))
  in
  let pred (_, versions, input_info) =
    equal_tool_versions comb versions &&
      (match input_info with None -> false | Some i -> i = input)
  in
  try
    let (iri_i, _, _) = List.find pred insts in
    Some iri_i
  with Not_found -> None
;;

module Graph = Chn_run.Graph;;

let create_flat_graph ctx iri_fchain =
  let g = Graph.create () in
  let f_consumer iri_src p_src (g, set) p_dst =
    let iri_dst = Grdfs.port_container p_dst in
    if Rdf_iri.equal iri_dst iri_fchain then
      begin
        let g = Graph.add g
          (Chn_run.Flat iri_src, Chn_run.Flat iri_dst,
           (Chn_run.Flat p_src, Chn_run.Flat p_dst)
          ) in
        (g, set)
      end
    else
      begin
        let g = Graph.add g
          (Chn_run.Flat iri_src, Chn_run.Flat iri_dst,
           (Chn_run.Flat p_src, Chn_run.Flat p_dst)
          ) in
        let set =
          let iri_from = Chn_flat.get_op_origin ctx iri_dst in
          match iri_from with
          | _ when Rdf_iri.equal iri_from Grdfs.genet_explode ->
              Iriset.add iri_dst set
          | _ when Rdf_iri.equal iri_from Grdfs.genet_implode ->
              Iriset.add iri_dst set
          | _ ->
              match Grdf_intf.intf_exists ctx.ctx_rdf iri_from with
                None -> set
              | Some _ -> Iriset.add iri_dst set
        in
        (g, set)
      end
  in
  let f_producer iri (g, port_set, set) p =
    if Iriset.mem p port_set then
      (
       dbg ~level: 1 (fun () -> Printf.sprintf "port %s already seen, stop" (Rdf_iri.string p));
       (* port already handled; do nothing more *)
       (g, port_set, set)
      )
    else
      (
       let consumers = Chn_flat.port_consumers ctx p in
       dbg ~level: 1
         (fun () -> Printf.sprintf "%d consumers for port %s"
            (List.length consumers) (Rdf_iri.string p));
       let port_set = Iriset.add p port_set in
       let (g, set) = List.fold_left
         (f_consumer iri p) (g, set) consumers
       in
       (g, port_set, set)
      )
  in
  let rec fill iri (g, port_set) =
    dbg ~level: 1 (fun () -> Printf.sprintf "fill iri=%s" (Rdf_iri.string iri));
    let dir =
      if Rdf_iri.equal iri iri_fchain
      then Grdf_port.In
      else Grdf_port.Out
    in
    let ports = Grdf_port.ports ctx.ctx_rdf iri dir in
    dbg ~level: 1 (fun () -> Printf.sprintf "%d %s port(s)" (List.length ports) (Grdf_port.string_of_dir dir));
    let (g, port_set, set) = List.fold_left (f_producer iri)
      (g, port_set, Iriset.empty) ports
    in
    Iriset.fold fill set (g, port_set)
  in
  let (g, _) = fill iri_fchain (g, Iriset.empty) in
  g
;;



let do_instanciate ctx reporter iri_fchain input comb =
  let prefix = ctx.ctx_cfg.Config.rest_api in
  reporter#push_context (Printf.sprintf "Running chain %S" (Rdf_iri.string iri_fchain));
  match Chn_types.is_iri_fchain ctx iri_fchain with
    None -> assert false
  | Some fchain_name ->
      let id = Misc.unique_id () in
      let inst_name = Chn_types.mk_ichain_name
        (Chn_types.fchain_chainname fchain_name) id
      in
      let iri_inst = Chn_types.iri_ichain prefix inst_name in
      Grdfs.add_type ctx.ctx_rdf
        ~sub: iri_inst
        ~obj: Grdfs.genet_instchain;
      Grdfs.add_triple_iris ctx.ctx_rdf
        ~sub: iri_inst ~pred: Grdfs.genet_instanciate ~obj: iri_fchain;
      (* associate tool versions *)
      Irimap.iter
        (fun _ version ->
           Grdfs.add_triple_iris ctx.ctx_rdf
             ~sub: iri_inst ~pred: Grdfs.genet_usetoolversion ~obj: version
        )
        comb;

      (* associate input files *)
      set_input_info ctx iri_inst input;
      Grdfs.set_creation_date_iri ctx.ctx_rdf iri_inst ();

      let g = create_flat_graph ctx iri_fchain in
      let dot = Chn_run.dot_of_graph ctx g in
      Misc.file_of_string ~file: "/tmp/inst.dot" dot;

      Chn_run.run ctx reporter ~inst: iri_inst ~fchain: iri_fchain input comb g ;
      reporter#pop_context;
      iri_inst
;;

let remove_inst_port ctx (reporter : Reporter.reporter) iri =
  let node = Rdf_term.Iri iri in
  let triples = ctx.ctx_rdf.wld_graph.Rdf_graph.find ~sub: node () in
  let f (_, pred, obj) =
    let p u = Rdf_iri.equal pred u in
    begin
      match pred with
      | _ when p Grdfs.rdf_type -> ()
      | _ when p Grdfs.genet_opfrom -> ()
      | _ when p Grdfs.genet_hastype -> ()
      | _ when p Grdfs.genet_filemd5 -> ()
      | _ when p Grdfs.genet_produces -> ()
      | _ when p Grdfs.genet_consumes -> ()
      | _ ->
          let msg = "Predicate "^(Rdf_iri.string pred)^" not handled for inst port; will remove triple anyway" in
          reporter#msg msg
    end;
    Grdfs.rem_triple ctx.ctx_rdf ~sub: node ~pred ~obj
  in
  List.iter f triples
;;

let remove_inst_opn ctx (reporter : Reporter.reporter) iri =
  let node = Rdf_term.Iri iri in
  let triples = ctx.ctx_rdf.wld_graph.Rdf_graph.find ~sub: node () in
  let f (_, pred, obj) =
    let p u = Rdf_iri.equal pred u in
    begin
      match pred with
      | _ when p Grdfs.rdf_type -> ()
      | _ when p Grdfs.genet_commandoutput -> ()
      | _ when p Grdfs.genet_startedon -> ()
      | _ when p Grdfs.genet_stoppedon -> ()
      | _ when p Grdfs.genet_opfrom -> ()
      | _ when p Grdfs.genet_consumes ->
          let iri = match obj with Rdf_term.Iri u -> u | _ -> assert false in
          remove_inst_port ctx reporter iri
      | _ when p Grdfs.genet_produces ->
          let iri = match obj with Rdf_term.Iri u -> u | _ -> assert false in
          remove_inst_port ctx reporter iri
      | _ ->
          let msg = "Predicate "^(Rdf_iri.string pred)^" not handled for inst opn; will remove triple anyway" in
          reporter#msg msg
    end;
    Grdfs.rem_triple ctx.ctx_rdf ~sub: node ~pred ~obj
  in
  List.iter f triples
;;

let remove_inst_chain ctx (reporter : Reporter.reporter) iri =
  let node = Rdf_term.Iri iri in
  let triples = ctx.ctx_rdf.wld_graph.Rdf_graph.find ~sub: node () in
  let f (_, pred, obj) =
    let p u = Rdf_iri.equal pred u in
    begin
      match pred with
      | _ when p Grdfs.rdf_type -> ()
      | _ when p Grdfs.genet_usetoolversion -> ()
      | _ when p Grdfs.genet_useinputcommitid -> ()
      | _ when p Grdfs.genet_useinput -> ()
      | _ when p Grdfs.genet_startedon -> ()
      | _ when p Grdfs.genet_createdon -> ()
      | _ when p Grdfs.genet_stoppedon -> ()
      | _ when p Grdfs.genet_opfrom -> ()
      | _ when p Grdfs.genet_instanciate -> ()
      | _ when p Grdfs.genet_consumes ->
          let iri = match obj with Rdf_term.Iri u -> u | _ -> assert false in
          remove_inst_port ctx reporter iri
      | _ when p Grdfs.genet_produces ->
          let iri = match obj with Rdf_term.Iri u -> u | _ -> assert false in
          remove_inst_port ctx reporter iri
      | _ when p Grdfs.genet_containsop ->
          begin
            match obj with
            | Rdf_term.Iri opn ->
                remove_inst_opn ctx reporter opn
            | _ ->  ()
          end
      | _ ->
          let msg = "Predicate "^(Rdf_iri.string pred)^" not handled for inst chain; will remove triple anyway" in
          reporter#msg msg
    end;
    Grdfs.rem_triple ctx.ctx_rdf ~sub: node ~pred ~obj
  in
  List.iter f triples
;;

let instanciate ctx (reporter:  Reporter.reporter) ?(force=false) iri_fchain input comb =
  match
    match inst_chain_exists ctx iri_fchain input comb with
      Some iri when not force -> Some iri
    | Some iri ->
        reporter#push_context ("Removing previous inst chain " ^ (Rdf_iri.string iri));
        remove_inst_chain ctx reporter iri;
        reporter#pop_context;
        None
    | None -> None
  with
    Some iri -> iri
  | None ->
      ctx.ctx_rdf.wld_graph.transaction_start ();
      try
        let iri = do_instanciate ctx reporter iri_fchain input comb in
        ctx.ctx_rdf.wld_graph.transaction_commit();
        iri
      with
      |  e ->
          ctx.ctx_rdf.wld_graph.transaction_rollback ();
          raise e
;;

let reference_insts ctx ~input ~chain =
  let input_refs = Grdfs.subject_iris ctx.ctx_rdf
    ~pred: Grdfs.genet_refinstfor ~obj: (Rdf_term.term_of_literal_string (Fname.rel_string input))
  in
  let chain_refs = Grdfs.subject_iris ctx.ctx_rdf
    ~pred: Grdfs.genet_refinstfor ~obj: (Rdf_term.Iri chain)
  in
  let set_input = List.fold_left
    (fun set iri -> Iriset.add iri set)
      Iriset.empty input_refs
  in
  List.filter (fun iri -> Iriset.mem iri set_input) chain_refs
;;

let reference_inst ctx ~input ~chain =
  match reference_insts ctx ~input ~chain with
  | [] -> None
  | h :: _ -> Some h
;;

let remove_ref_inst ctx ~input ~chain ~inst =
  Grdfs.rem_triple ctx.ctx_rdf
    ~sub: (Rdf_term.Iri inst)
    ~pred: Grdfs.genet_refinstfor
    ~obj: (Rdf_term.term_of_literal_string (Fname.rel_string input));
  Grdfs.rem_triple_iris ctx.ctx_rdf
    ~sub: inst ~pred: Grdfs.genet_refinstfor ~obj: chain
;;

let inst_is_reference ctx inst =
  match Grdfs.objects ctx.ctx_rdf
    ~sub: (Rdf_term.Iri inst) ~pred: Grdfs.genet_refinstfor
  with
    [] -> false
  | _ -> true
;;

let add_reference_inst ctx ~input ~chain ~inst =
  (* test the inst chain is an instanciation of the given chain *)
  let iri_chain =
    let inst_source =
      match instance_source ctx inst with
        None ->
          failwith
            (Printf.sprintf "No source flat chaint for inst chain %S" (Rdf_iri.string inst))
      | Some iri -> iri
    in
    let fchain_name =
      match Chn_types.is_iri_fchain ctx inst_source with
        None -> failwith (Printf.sprintf "Unknown flat chain %S" (Rdf_iri.string inst_source))
      | Some n -> n
    in
    let chain_name = Chn_types.fchain_chainname fchain_name in
    Chn_types.iri_chain ctx.ctx_cfg.Config.rest_api chain_name
  in
  if not (Rdf_iri.equal iri_chain chain) then
    (
     let msg = Printf.sprintf
       "Inst chain %S instanciates %S and cannot be used as reference for %S"
         (Rdf_iri.string inst) (Rdf_iri.string iri_chain) (Rdf_iri.string chain)
     in
     failwith msg
    );

  (* test the inst chain uses the given input *)
  begin
    match inst_input ctx inst with
      None ->
        let msg = Printf.sprintf "Inst chain %S has no associated input."
          (Rdf_iri.string inst)
        in
        failwith msg
    | Some (name, _) ->
        match Fname.compare name input with
          0 -> ()
        | _ ->
            let msg = Printf.sprintf
              "Inst chain %S uses input %S and cannot be used as reference for %S"
                (Rdf_iri.string inst) (Fname.rel_string name) (Fname.rel_string input)
            in
            failwith msg
  end;
  List.iter
    (fun inst -> remove_ref_inst ctx ~input ~chain ~inst)
    (reference_insts ctx ~input ~chain);
  Grdfs.add_triple ctx.ctx_rdf
    ~sub: (Rdf_term.Iri inst)
    ~pred: Grdfs.genet_refinstfor
    ~obj: (Rdf_term.term_of_literal_string (Fname.rel_string input));
  Grdfs.add_triple_iris ctx.ctx_rdf
    ~sub: inst ~pred: Grdfs.genet_refinstfor ~obj: chain
;;

let reference_inst_of_inst ctx inst =
  match inst_input ctx inst with
    None -> None
  | Some (input, _) ->
      match instance_source ctx inst with
        None -> None
      | Some inst_source ->
          match Chn_types.is_iri_fchain ctx inst_source with
            None -> None
          | Some fchain_name ->
              let chain_name = Chn_types.fchain_chainname fchain_name in
              let chain = Chn_types.iri_chain ctx.ctx_cfg.Config.rest_api chain_name in
              reference_inst ctx ~input ~chain
;;

let inst_chains wld =
  Grdfs.subject_iris wld
    ~pred: Grdfs.rdf_type
    ~obj: (Rdf_term.Iri Grdfs.genet_instchain)
;;

