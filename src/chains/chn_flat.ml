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

open Rdf_term;;
open Rdf_graph;;
open Grdf_types;;
open Chn_types;;
open Chn_ast;;

module Iriset = Rdf_iri.Iriset
module Irimap = Rdf_iri.Irimap

let dbg = Misc.create_log_fun
  ~prefix: "Chn_flat"
    "GENET_CHN_FLAT_DEBUG_LEVEL"
;;

let merge_port_maps =
  let merge key a b =
    match a, b with
      Some iri1, Some iri2 ->
        let msg = Printf.sprintf "port %s is mapped to %s and %s"
          (Rdf_iri.string key) (Rdf_iri.string iri1) (Rdf_iri.string iri2)
        in
        failwith msg
    | None, Some v
    | Some v, None -> Some v
    | None, None -> None
  in
  Irimap.merge merge
;;

module Chain_versions =
  Set.Make
  (struct
     type t = Chn_types.chain_modname * string (* git id *)
     let compare (n1,id1) (n2, id2) =
       match Chn_types.compare_chain_modname n1 n2 with
         0 -> Pervasives.compare id1 id2
       | n -> n
   end)
;;

let fchain_chain_versions ctx iri_fchain =
  let l = Grdfs.object_literals ctx.ctx_rdf
    ~sub: (Iri iri_fchain) ~pred: Grdfs.genet_useversion
  in
  let f acc s =
    match Misc.split_string s ['/'] with
      [modname ; id] -> Chain_versions.add (Chn_types.chain_modname_of_string modname, id) acc
    | _ -> failwith (Printf.sprintf "Invalid chain version %S" s)
  in
  List.fold_left f Chain_versions.empty l
;;

let remove_chain_version ctx iri_fchain (modname, id) =
  let obj = Rdf_term.term_of_literal_string
    (Printf.sprintf "%s/%s" (Chn_types.string_of_chain_modname modname) id)
  in
  Grdfs.rem_triple ctx.ctx_rdf ~sub: (Iri iri_fchain)
  ~pred: Grdfs.genet_useversion ~obj
;;

let add_chain_version ctx iri_fchain (modname, id) =
  let obj = Rdf_term.term_of_literal_string
    (Printf.sprintf "%s/%s" (Chn_types.string_of_chain_modname modname) id)
  in
  Grdfs.add_triple ctx.ctx_rdf ~sub: (Iri iri_fchain)
  ~pred: Grdfs.genet_useversion ~obj
;;

let flat_chains wld =
  Grdfs.subject_iris wld
    ~pred: Grdfs.rdf_type
    ~obj: (Iri Grdfs.genet_flatchain)
;;

let set_fchain_chain_versions ctx iri_fchain chain_versions =
  let previous = fchain_chain_versions ctx iri_fchain in
  Chain_versions.iter (remove_chain_version ctx iri_fchain) previous;
  Chain_versions.iter (add_chain_version ctx iri_fchain) chain_versions
;;

let fchain_exists ctx orig_fullname chain_versions =
  let iri_chain = Chn_types.iri_chain ctx.ctx_cfg.Config.rest_api orig_fullname in
  let fchains = Grdfs.object_iris ctx.ctx_rdf
    ~sub: (Iri iri_chain) ~pred: Grdfs.genet_flattenedto
  in
  let pred iri_fchain =
    let versions = fchain_chain_versions ctx iri_fchain in
    Chain_versions.equal versions chain_versions
  in
  try Some (List.find pred fchains)
  with Not_found -> None
;;

let get_ops ctx iri =
  let l = Grdfs.object_iris ctx.ctx_rdf ~sub: (Iri iri) ~pred: Grdfs.genet_containsop in
  dbg ~level: 3
  (fun() -> Printf.sprintf
     "length(get_ops(%s)) =%n" (Rdf_iri.string iri) (List.length l));
  l
;;

let rec get_op_origin ctx ?current iri =
  match Grdfs.object_iri ctx.ctx_rdf ~sub: (Iri iri) ~pred: Grdfs.genet_opfrom with
  | Some iri -> get_op_origin ctx ~current: iri iri
  | None ->
      match current with
        None ->
          failwith (Printf.sprintf "Operation %s has no 'from' link." (Rdf_iri.string iri))
      | Some x -> x
;;

let intfs_of_flat_chain ctx iri =
  let rec f acc iri =
    let iri_from = get_op_origin ctx iri in
    match Grdf_intf.intf_exists ctx.ctx_rdf iri_from with
      None ->
        let ops = get_ops ctx iri in
        List.fold_left f acc ops
    | Some _ ->
        Iriset.add iri_from acc
  in
  let ops = get_ops ctx iri in
  List.fold_left f Iriset.empty ops
;;

let check_tool_intfs ctx fchain =
  let intfs = intfs_of_flat_chain ctx fchain in
  let (tools, required_tools) =
    Iriset.fold
    (fun intf (tools, req) ->
       let l = Grdf_intf.additional_tools_used ctx.ctx_rdf intf in
       let req = List.fold_right Iriset.add l req in
       let tools = Iriset.add (Grdf_intf.tool_of_intf intf) tools in
       (tools, req)
    )
    intfs
    (Iriset.empty, Iriset.empty)
  in
  begin
    let f tool =
      match Iriset.mem tool tools with
        true -> ()
      | false ->
          let msg = Printf.sprintf
            "Tool %S is required by one interface in the chain %S,\
            but no interface of this tool is used in this chain."
            (Rdf_iri.string tool) (Rdf_iri.string fchain)
          in
        failwith msg
    in
    Iriset.iter f required_tools
  end;
;;


let add_containsop ctx ~src ~dst =
  Grdfs.add_triple_iris ctx.ctx_rdf
    ~sub: src ~pred: Grdfs.genet_containsop ~obj: dst;
  dbg ~level: 3
    (fun() -> Printf.sprintf "%s -containsop-> %s"
      (Rdf_iri.string src)(Rdf_iri.string dst));
;;

let get_op_name iri =
  match List.rev (Rdf_iri.path iri) with
    [] -> failwith "Not a flat chain operation iri: "^(Rdf_iri.string iri)
  | name :: _ -> name
;;

let port_consumers ctx iri =
  let iris = Grdfs.object_iris ctx.ctx_rdf ~sub: (Iri iri) ~pred: Grdfs.genet_produces in
  List.filter Grdfs.is_a_port iris
;;

let port_producers ctx iri =
  let iris = Grdfs.subject_iris ctx.ctx_rdf ~pred: Grdfs.genet_produces ~obj: (Iri iri) in
  List.filter Grdfs.is_a_port iris
;;



let rec import_flat_op ctx iri_src iri_dst path (map_in, map_out) =
  assert (path <> []);
  let dbg = dbg ~loc: "import_flat_op" in
  let iri_op = Grdfs.iri_fchain_op iri_dst path in
  let iri_parent = Grdfs.iri_parent iri_op in

  add_containsop ctx ~src: iri_parent ~dst: iri_op;

  Grdfs.add_triple_iris ctx.ctx_rdf
    ~sub: iri_op ~pred: Grdfs.genet_opfrom ~obj: iri_src;

  dbg ~level: 3
    (fun() -> Printf.sprintf "%s -opfrom-> %s"
      (Rdf_iri.string iri_op)(Rdf_iri.string iri_src));

  (* copy ports of the src flat chain to our target operation *)
  let (map_in2, map_out2) =
    Grdf_port.copy_ports ctx.ctx_rdf ~src: iri_src ~dst: iri_op
  in
  let map_in = merge_port_maps map_in map_in2 in
  let map_out = merge_port_maps map_out map_out2 in

  (* copy sub operations *)
  let sub_ops = get_ops ctx iri_src in
  List.fold_left (import_flat_sub_op ctx iri_dst path)
  (map_in, map_out) sub_ops

and import_flat_sub_op ctx iri_dst path (map_in, map_out) iri_sub_op =
  dbg ~loc: "import_flat_sub_op" ~level: 3
    (fun () ->
       Printf.sprintf "* iri_dst = %s\n \
                       * path = %s\n \
                       * iri_sub_op = %s"
     (Rdf_iri.string iri_dst) (String.concat "/" path)
     (Rdf_iri.string iri_sub_op)
     );
  let name = get_op_name iri_sub_op in
  let path = path @ [name] in
  import_flat_op ctx iri_sub_op iri_dst path (map_in, map_out)
;;

let add_intf ctx iri_op loc intf_spec =
  let dbg = dbg ~loc: "add_intf" in
  dbg ~level: 2
    (fun () ->
      Printf.sprintf "iri_op=%s intf_spec=%s" (Rdf_iri.string iri_op) intf_spec
    );
  let iri_intf = Chn_types.iri_intf_of_interface_spec
    ~prefix: ctx.ctx_cfg.Config.rest_api intf_spec
  in
  dbg ~level: 3
    (fun () -> Printf.sprintf "iri_intf=%s" (Rdf_iri.string iri_intf));
  Grdfs.add_triple_iris ctx.ctx_rdf
    ~sub: iri_op ~pred: Grdfs.genet_opfrom ~obj: iri_intf;
  ignore(Grdf_port.copy_ports ctx.ctx_rdf ~src: iri_intf ~dst: iri_op)
;;

let port_info wld iri =
  (Grdf_port.port_rank iri, Grdf_port.port_type wld iri, Some (Grdf_port.port_name wld iri))
;;

let build_implode_ports ctx iri_op op_name =
  (* we assume that the operation corresponding to the implode
     was already flattened, so that we can retrieve ports from it ;
     we must to so because, in case this operation is another chain,
     we must use the same version; reading the source file is less sure
     that using the already created flat operation *)
  let iri_orig = Rdf_iri.concat (Grdfs.iri_parent iri_op) op_name in
  let ports = Grdf_port.ports ctx.ctx_rdf iri_orig Grdf_port.Out in
  let in_ports = List.map (port_info ctx.ctx_rdf) ports in
  let out_port =
    let t =
      match in_ports with
        [] -> Grdf_types.Tuple []
      | [(_,t,_)] -> Grdf_types.Set t
      | _ -> Grdf_types.Set (Grdf_types.Tuple (List.map (fun (_, t, _) -> t) in_ports))
    in
    (1, t, Some "o")
  in
  (in_ports, [out_port])
;;

let build_explode_ports ctx iri_op op_name port_ref =
  (* we assume that the operation corresponding to the implode
     was already flattened, so that we can retrieve ports from it ;
     we must to so because, in case this operation is another chain,
     we must use the same version; reading the source file is less sure
     that using the already created flat operation *)
  let iri_orig = Rdf_iri.concat (Grdfs.iri_parent iri_op) op_name in
  let ports = Grdf_port.ports ctx.ctx_rdf iri_orig Grdf_port.In in
  let (port_type, name) =
    let rec iter = function
      [] -> failwith (Printf.sprintf "Exploded port not found for operation %s" (Rdf_iri.string iri_orig))
    | iri :: q ->
        let (rank, typ, name) = port_info ctx.ctx_rdf iri in
        let b =
          match port_ref with
            Pint n -> n = rank
          | Pname s -> name = Some s
        in
        if b then
          (typ, name)
        else
          iter q
    in
    iter ports
  in
  let in_port = (1, Grdf_types.Set port_type, name) in
  let out_port = (1, port_type, name) in
  ([in_port], [out_port])
;;

let add_special ctx iri_op loc spec =
  let dbg = dbg ~loc: "add_special" in
  dbg ~level: 2 (fun () -> Printf.sprintf "iri_op=%s" (Rdf_iri.string iri_op));
  let (typ, in_ports, out_ports) =
    match spec with
      Implode (op_name, _, _) ->
        let (in_ports, out_ports) = build_implode_ports ctx iri_op op_name in
        (Grdfs.genet_implode, in_ports, out_ports)
    | Explode (op_name, _, port_ref) ->
        let iri_implode =
          let parent = Grdfs.iri_parent iri_op in
          Rdf_iri.concat parent (Printf.sprintf "%s-implode" op_name)
        in
        Grdfs.add_triple_iris ctx.ctx_rdf
          ~sub: iri_op ~pred: Grdfs.genet_hasimplode ~obj: iri_implode;
        let (in_ports, out_ports) = build_explode_ports ctx iri_op op_name port_ref in
        (Grdfs.genet_explode, in_ports, out_ports)
  in
  Grdfs.add_type ctx.ctx_rdf ~sub: (Iri iri_op) ~obj: (Iri typ);
  let f = Grdf_port.set_ports ctx.ctx_rdf in
  f iri_op Grdf_port.In in_ports;
  f iri_op Grdf_port.Out out_ports ;
  Grdfs.add_triple_iris ctx.ctx_rdf
  ~sub: iri_op ~pred: Grdfs.genet_opfrom ~obj: typ
;;

let create_ports_from_chn ctx iri chn =
  let dbg = dbg ~loc: "create_port_from_chn" in
  dbg ~level: 2
    (fun () -> Printf.sprintf "iri=%s chn=%s"
      (Rdf_iri.string iri) (Chn_types.string_of_chain_basename chn.chn_name));
  let mk_port dir (rank, map, ports) p =
    dbg ~level: 3
      (fun () -> Printf.sprintf "mk_port rank=%d p=%s" rank p.p_name);
    let ports = (rank, p.p_ftype, Some p.p_name) :: ports in
    let map = Smap.add p.p_name rank map in
    (rank + 1, map, ports)
  in
  let set_ports dir t =
    let (n, map, ports) = Array.fold_left (mk_port dir) (1, Smap.empty, []) t in
    Grdf_port.set_ports ctx.ctx_rdf iri dir ports;
    map
  in
  let map_in = set_ports Grdf_port.In chn.chn_inputs in
  let map_out = set_ports Grdf_port.Out chn.chn_outputs in
  (map_in, map_out)
;;

let mk_port_map ctx iri dir =
  let wld = ctx.ctx_rdf in
  let ports = Grdf_port.ports wld iri dir in
  List.fold_left
  (fun map iri ->
     let rank = Grdf_port.port_rank iri in
     let name = Grdf_port.port_name wld iri in
     Smap.add name rank map
  )
  Smap.empty ports
;;

let find_port map dir edge_part =
  let (dir, op_name) =
    match dir, edge_part.ep_op with
    | Grdf_port.In, None -> (Grdf_port.Out, "")
    | Grdf_port.Out, None -> (Grdf_port.In, "")
    | _, Some s -> (dir, s)
  in
  let (iri, map_in, map_out) =
    try Smap.find op_name map
    with Not_found ->
        Loc.raise_problem edge_part.ep_loc
        (Printf.sprintf "No operation %S" op_name)
  in
  let rank =
    match edge_part.ep_port with
      Pint n -> n
    | Pname name ->
        let map =
          match dir with
            Grdf_port.In -> map_in
          | Grdf_port.Out -> map_out
        in
        try Smap.find name map
        with Not_found ->
            let msg = Printf.sprintf "Unknown port %s%s"
              (match op_name with "" -> "" | s -> s^".")
                name
            in
            Loc.raise_problem edge_part.ep_loc msg
  in
  let f =
    match dir with
      Grdf_port.In -> Grdfs.iri_intf_in_port
    | Grdf_port.Out -> Grdfs.iri_intf_out_port
  in
  f iri rank
;;

let types_are_compatible =
  let rec comp t1 t2 =
    match t1, t2 with
      Grdf_types.Var _, Grdf_types.Var _ -> true
    | Grdf_types.T u1, Grdf_types.T u2 -> String.compare u1 u2 = 0
    | Grdf_types.Set t1, Grdf_types.Set t2 -> comp t1 t2
    | Grdf_types.Tuple t1, Grdf_types.Tuple t2 ->
        (
         try List.for_all2 comp t1 t2
         with Invalid_argument _ -> false
        )
    | _ -> false
  in
  comp
;;

let create_data_edge ctx iri map edge =
  let iri_src = find_port map Grdf_port.Out edge.edge_src in
  let iri_dst = find_port map Grdf_port.In edge.edge_dst in
  let type_src = Grdf_port.port_type ctx.ctx_rdf iri_src in
  let type_dst = Grdf_port.port_type ctx.ctx_rdf iri_dst in
  let compat = types_are_compatible type_src type_dst in
  dbg ~level: 2
  (fun () ->
     Printf.sprintf "iri_src=%s type=%s\niri_dst=%s type=%s"
     (Rdf_iri.string iri_src) (Grdf_port.string_of_port_type (fun x -> x) type_src)
     (Rdf_iri.string iri_dst) (Grdf_port.string_of_port_type (fun x -> x) type_dst)
  );
  if not compat then
    Loc.raise_problem edge.edge_src.ep_loc
      (Printf.sprintf "Incompatible types: %s <--> %s"
        (Grdf_port.string_of_port_type (fun x -> x) type_src)
        (Grdf_port.string_of_port_type (fun x -> x) type_dst));
  (* TODO: check that two edges don't have the same destination *)
  Grdfs.add_triple_iris ctx.ctx_rdf
    ~sub: iri_src ~pred: Grdfs.genet_produces ~obj: iri_dst
;;

let create_data_edges ctx iri map chn =
  List.iter (create_data_edge ctx iri map) chn.chn_edges
;;

let add_edges_from_maps ctx map_in map_out =
  let map = merge_port_maps map_in map_out in
  let find p =
    try Irimap.find p map
    with Not_found ->
        failwith (Printf.sprintf "port %s could not be mapped" (Rdf_iri.string p))
  in
  let map_ports dir new_port ports =
    let ports = List.map find ports in
    let f =
      match dir with
        Grdf_port.In ->
          (fun sub -> Grdfs.add_triple_iris ctx.ctx_rdf
             ~sub ~pred: Grdfs.genet_produces ~obj: new_port)
      | Grdf_port.Out ->
          (fun obj -> Grdfs.add_triple_iris ctx.ctx_rdf
             ~sub: new_port ~pred: Grdfs.genet_produces ~obj)
    in
    List.iter f ports
  in
  let f_orig_port orig_port new_port =
    let producers = port_producers ctx orig_port in
    let consumers = port_consumers ctx orig_port in
    map_ports Grdf_port.In new_port producers;
    map_ports Grdf_port.Out new_port consumers
  in
  Irimap.iter f_orig_port map
;;

let remove_useless_ports ctx iri_fchain =
  let f_port iri_op p =
    let producers = port_producers ctx p in
    let consumers = port_consumers ctx p in
    match producers, consumers with
      [], []
    | _, []
    | [], _ -> ()
    | _ :: _ :: _, _ ->
        let msg = Printf.sprintf "Port %s has more than one producer!"
          (Rdf_iri.string p)
        in
        failwith msg
    | [producer], _ ->
        let f_succ p_succ =
          Grdfs.rem_triple_iris ctx.ctx_rdf
          ~sub: p ~pred: Grdfs.genet_produces ~obj: p_succ;
          Grdfs.rem_triple_iris ctx.ctx_rdf
          ~sub: producer ~pred: Grdfs.genet_produces ~obj: p;
          Grdfs.add_triple_iris ctx.ctx_rdf
          ~sub: producer ~pred: Grdfs.genet_produces ~obj: p_succ
        in
        List.iter f_succ consumers
  in
  let f_dir iri_op dir =
    let ports = Grdf_port.ports ctx.ctx_rdf iri_op dir in
    List.iter (f_port iri_op) ports
  in
  let f_op iri_op =
    f_dir iri_op Grdf_port.In;
    f_dir iri_op Grdf_port.Out;
  in
  List.iter f_op (get_ops ctx iri_fchain)
;;

let rec do_flatten ctx deps fullname =
  let dbg = dbg ~loc: "do_flatten" in
  dbg ~level: 2
    (fun () -> Printf.sprintf "fullname=%s"
      (Chn_types.string_of_chain_name fullname));

  let modname = Chn_types.chain_modname fullname in
  let name = Chn_types.chain_basename fullname in
  let file = Chn_io.file_of_modname ctx.ctx_cfg modname in
  let chain_versions =
    let files =
      try (Chn_ast.Cmap.find fullname deps).Chn_io.dep_files
      with Not_found ->
          failwith (Printf.sprintf "Unknown chain %S" (Chn_types.string_of_chain_name fullname))
    in
    (* if at least on module has no git id, we will use an empty set
      as constraints against original files; this means we are in
      test mode, this empty set will not be kept, but we can use
      it until the rollback not to flatten the same flat chain twice *)
    let f (file, st) set =
      match st with
        Misc.Git_id id ->
          let modname = Chn_io.modname_of_file file in
          Chain_versions.add (modname, id) set
      | _ -> set
    in
    let set = Chn_io.File_set.fold f files Chain_versions.empty in
    if Chain_versions.cardinal set <> Chn_io.File_set.cardinal files then
      Chain_versions.empty
    else
      set
  in
  match fchain_exists ctx fullname chain_versions with
    Some iri ->
      dbg ~level:3
      (fun () -> Printf.sprintf "fchain with iri=%s already exists for chain versions"
         (Rdf_iri.string iri));
      iri
  | None ->
      begin
        let id = Misc.unique_id () in
        let fchain_name = Chn_types.mk_fchain_name fullname id in
        let iri_fchain = Chn_types.iri_fchain ctx.ctx_cfg.Config.rest_api fchain_name in
        let chn_mod = Chn_io.chn_module_of_file file in
        let chn =
          match Chn_ast.get_chain chn_mod name with
            None -> failwith (Printf.sprintf "Unbound chain %s" (Chn_types.string_of_chain_name fullname))
          | Some chn -> chn
        in
        let chn = Chn_ast.flatten_foreaches ctx chn in
        let op_map =
          let (map_in, map_out) = create_ports_from_chn ctx iri_fchain chn in
          Smap.singleton "" (iri_fchain, map_in, map_out)
        in
        let op_map = List.fold_left (add_op ctx deps iri_fchain) op_map chn.chn_ops in
        create_data_edges ctx iri_fchain op_map chn;
        let sub = Chn_types.iri_chain ctx.ctx_cfg.Config.rest_api fullname in
        remove_useless_ports ctx iri_fchain;
        Grdfs.add_triple_iris ctx.ctx_rdf
        ~sub ~pred: Grdfs.genet_flattenedto ~obj: iri_fchain;
        set_fchain_chain_versions ctx iri_fchain chain_versions;
        Grdfs.set_creation_date_iri ctx.ctx_rdf iri_fchain ();
        Grdfs.add_type ctx.ctx_rdf
        ~sub: (Iri iri_fchain)
        ~obj:(Iri Grdfs.genet_flatchain);
        iri_fchain
      end

and add_op ctx deps iri_fchain map op =
  let dbg = dbg ~loc: "add_op" in
  dbg ~level: 2
  (fun () ->
    Printf.sprintf "iri_fchain=%s op=%s"
      (Rdf_iri.string iri_fchain) op.op_name
  );
  let path = [op.op_name] in
  let iri_op = Grdfs.iri_fchain_op iri_fchain path in
  dbg ~level: 3
     (fun () -> Printf.sprintf "iri_op=%s" (Rdf_iri.string iri_op));
  let add = Grdfs.add_triple_iris ctx.ctx_rdf in
  add_containsop ctx ~src: iri_fchain ~dst: iri_op;
  let (map_in, map_out) =
    match op.op_from with
      Interface s ->
        dbg ~level: 2
          (fun () -> Printf.sprintf "add_op: Interface %s" s);
        add_intf ctx iri_op op.op_from_loc s;
        (mk_port_map ctx iri_op Grdf_port.In,
         mk_port_map ctx iri_op Grdf_port.Out)
    | Special sp ->
        add_special ctx iri_op op.op_from_loc sp;
        (mk_port_map ctx iri_op Grdf_port.In,
         mk_port_map ctx iri_op Grdf_port.Out)
    | Chain fullname ->
        dbg ~level: 2
          (fun () -> Printf.sprintf "add_op: Chain %s"
             (Chn_types.string_of_chain_name fullname)
          );
        let src = do_flatten ctx deps fullname in
        add ~sub: iri_op ~pred: Grdfs.genet_opfrom ~obj: src;
        let (map_in, map_out) =
          import_flat_op ctx src iri_fchain path (Irimap.empty, Irimap.empty)
        in
        add_edges_from_maps ctx map_in map_out;
        (Smap.empty, Smap.empty)
    | Foreach _ ->
        assert false
  in
  Smap.add op.op_name (iri_op, map_in, map_out) map
;;

let flatten ctx fullname =
  ctx.ctx_rdf.wld_graph.transaction_start ();
  let deps = Chn_io.get_chain_deps_files ctx fullname in
  let files =
    try (Chn_ast.Cmap.find fullname deps).Chn_io.dep_files
    with Not_found -> assert false
  in
  let test_mode =
    let f (file, st) acc =
      match st with
        Misc.Git_id _ -> acc
      | _ -> file :: acc
    in
    match Chn_io.File_set.fold f files [] with
      [] -> false
    | files ->
        let msg = Printf.sprintf
          "The following files are not commited:\n%s\n=> The flattened chains will no be kept (test mode)"
          (String.concat "\n" (List.map Fname.abs_string files))
        in
        Checks.print_warning msg;
        true
  in
  try
    let iri = do_flatten ctx deps fullname in
    check_tool_intfs ctx iri;
    if test_mode then
      ctx.ctx_rdf.wld_graph.transaction_rollback ()
    else
      ctx.ctx_rdf.wld_graph.transaction_commit();
    iri
  with
    e ->
      ctx.ctx_rdf.wld_graph.transaction_rollback ();
      raise e
;;


class fchain_dot_printer =
  let dotp = new Chn_ast.chain_dot_printer in
  object(self)
    method id s = "n"^(Digest.to_hex (Digest.string s))
    method iri_id iri = self#id (Rdf_iri.string iri)

    method color_of_port_dir = function
    | Grdf_port.In -> dotp#color_in
    | Grdf_port.Out -> dotp#color_out

    method port_producers = port_producers
    method port_consumers = port_consumers

    method print_port_edges ctx b iri =
      let ports = self#port_consumers ctx iri in
      let src = self#iri_id iri in
      let f p =
        Printf.bprintf b "%s -> %s ;\n" src (self#iri_id p)
      in
      List.iter f ports

    method get_port_type_iri ctx t =
      Grdf_port.port_file_type_iri ctx.ctx_cfg.Config.rest_api t


    method port_link_and_name ctx iri =
      let ptype = Grdf_port.port_type ctx.ctx_rdf iri in
      let link = self#get_port_type_iri ctx ptype in
      let name = Grdf_port.string_of_port_type (fun x -> x) ptype in
      (link, name)

    method print_port ctx b ?(all=false) iri =
      match self#port_producers ctx iri, self#port_consumers ctx iri with
        [], [] when not all -> false
      | _ ->
          let dir = Grdf_port.port_dir iri in
          let id = self#iri_id iri in
          let (link, name) = self#port_link_and_name ctx iri in
          let label =
            match Grdf_port.port_name ctx.ctx_rdf iri with
              "" -> string_of_int (Grdf_port.port_rank iri)
            | s -> s
          in
          Printf.bprintf b "%s [color=\"black\" fillcolor=\"%s\" \
                            style=\"filled\" shape=\"box\" \
                            href=\"%s\" label=\"%s:%s\" rank=%S];\n"
          id (self#color_of_port_dir dir)
            (match link with None -> "" | Some iri -> Rdf_iri.string iri)
             label name
          (match dir with Grdf_port.In -> "min" | Grdf_port.Out -> "max");
          true

    method do_print_op ctx b ~maxdepth ~depth ~cluster acc iri =
      dbg ~level: 3 (fun () -> Printf.sprintf "#do_print_op iri = %S" (Rdf_iri.string iri));
      let root = depth <= 0 in
      let clustering = cluster iri in
      if not root && clustering then
        begin
          let (color, label, href) =
            let iri_from = get_op_origin ctx iri in
            match Grdf_intf.intf_exists ctx.ctx_rdf iri_from with
              None -> dotp#color_chain, get_op_name iri, iri
            | Some name ->
              let tool = Grdf_intf.tool_of_intf iri_from in
              let name = Printf.sprintf "%s / %s" (Grdf_tool.name ctx.ctx_rdf tool) name in
              dotp#color_interface, name, iri_from
          in
          Printf.bprintf b "subgraph cluster_%s {\n\
             label=%S;\n color=\"black\" fillcolor=%S;\n\
             style=\"filled\"; href=%S;\n"
          (self#iri_id iri) label color
          (Rdf_iri.string href)
        end;
      let f (acc, node) dir =
        if root then
          Printf.bprintf b "subgraph cluster_%s {\n"
          (Grdf_port.string_of_dir dir);
        let ports = Grdf_port.ports ctx.ctx_rdf iri dir in
        dbg ~level: 3 (fun () -> Printf.sprintf "#do_print_op ports=%d" (List.length ports));
        let printed_ports = List.filter
          (self#print_port ~all: (root || clustering) ctx b) ports
        in
        dbg ~level: 3 (fun () -> Printf.sprintf "#do_print_op printed_ports=%d" (List.length printed_ports));
        begin
          match node with
            None -> ()
          | Some in_p ->
              List.iter
              (fun out_p ->
                 Printf.bprintf b "%s -> %s [style=\"invis\"];\n"
                 (self#iri_id in_p) (self#iri_id out_p)
              )
              printed_ports
        end;
        let node = match ports with [] -> None | h :: _ -> Some h in
        if root then Buffer.add_string b "}\n";
        (List.fold_right Iriset.add ports acc, node)
      in
      let (acc, _) = List.fold_left f (acc, None) [ Grdf_port.In ; Grdf_port.Out ] in
      let acc = List.fold_left
        (self#print_op ctx b ~maxdepth ~depth: (depth+1) ~cluster)
        acc (get_ops ctx iri)
      in
      if (not root) && clustering then Buffer.add_string b "}\n";
      acc

    method print_op ctx b ?(maxdepth=max_int) ~depth ~cluster acc iri =
      dbg ~level: 2 (fun () ->  Printf.sprintf "print_op iri=%s" (Rdf_iri.string iri));
      if depth > maxdepth then
        acc
      else
        self#do_print_op ctx b ~maxdepth ~depth ~cluster acc iri

    method dot_of_fchain ctx ?debug iri =
      let b = Buffer.create 256 in
      Buffer.add_string b "digraph g {\nrankdir=LR;\nfontsize=10;\n";
      let ports =
        let maxdepth, cluster =
          match debug with
            None ->
              let cluster iri =
                List.exists
                (fun dir ->
                   List.exists
                   (fun p -> (self#port_consumers ctx p <> []) ||
                      (self#port_producers ctx p <> []))
                    (Grdf_port.ports ctx.ctx_rdf iri dir)
                )
                [ Grdf_port.In ; Grdf_port.Out ]
              in
              (None, cluster)
          | Some _ -> debug, (fun _ -> true)
        in
        self#print_op ctx b ?maxdepth ~depth: 0 ~cluster Iriset.empty iri
      in
      Iriset.iter (self#print_port_edges ctx b) ports;
      Buffer.add_string b "}\n";
      Buffer.contents b
  end

let flat_chains_of_chain ctx chain_name =
  let iri_chain = Chn_types.iri_chain ctx.ctx_cfg.Config.rest_api chain_name in
  Grdfs.object_iris ctx.ctx_rdf
    ~sub: (Iri iri_chain) ~pred: Grdfs.genet_flattenedto
;;

let fchain_creation_date ctx iri =
  match Grdfs.creation_date_iri ctx.ctx_rdf iri with
    None -> None
  | Some d -> Some (Netdate.mk_mail_date (Netdate.since_epoch d))
;;



