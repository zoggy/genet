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

open Grdf_types;;
open Rdf_term;;
open Rdf_graph;;

module Iriset = Rdf_iri.Iriset;;

let dbg = Misc.create_log_fun
  ~prefix: "Grdfs"
    "GENET_GRDFS_DEBUG_LEVEL"
;;

(** {2 Vocabulary} *)

let genet = Rdf_iri.iri "https://raw.github.com/zoggy/genet/master/doc/genet.ttl";;
let genet_ s = Rdf_iri.set_fragment genet (Some s);;

let rdf_ = Rdf_rdf.rdf_ ;;
let rdfs_ = Rdf_rdfs.rdfs_;;
let dc_ = Rdf_iri.concat (Rdf_iri.iri "http://purl.org/dc/elements/1.1");;
let owl_ s = Rdf_iri.set_fragment (Rdf_iri.iri "http://www.w3.org/2002/07/owl") (Some s);;
let xsd_ s = Rdf_iri.set_fragment (Rdf_iri.iri "http://www.w3.org/2001/XMLSchema") (Some s);;
let foaf_ = Rdf_foaf.foaf_ ;;

let datatype_integer = xsd_"integer";;

let namespaces = [ genet_"", "genet" ; rdfs_"", "rdfs" ; dc_"", "dc" ; xsd_"", "xsd" ]

let li_ n =
  if n <= 0 then
    raise (Invalid_argument (Printf.sprintf "li: n=%d <= 0" n))
  else
    rdf_ (Printf.sprintf "li%n" n)
;;

let iri_parent iri =
  match List.rev (Rdf_iri.path iri) with
    [] -> iri
  | _ :: q -> Rdf_iri.set_path iri q
;;

let rdf_type = rdf_"type";;

let genet_tool = genet_ "Tool";;
let genet_branch = genet_ "Branch";;
let genet_intf = genet_ "Interface";;
let genet_version = genet_ "Version";;
let genet_filetype = genet_ "Filetype";;
let genet_chain = genet_ "Chain";;
let genet_flatchain = genet_ "Flatchain";;
let genet_instchain = genet_ "Instchain";;
let genet_instopn = genet_ "InstOperation";;
let genet_implode = genet_"Implode";;
let genet_explode = genet_"Explode";;
let genet_subgraph = genet_"Subgraph";;
let genet_diffcommand = genet_"DiffCommand";;

let string_of_class = function
  s when Rdf_iri.equal s genet_tool -> "tool"
| s when Rdf_iri.equal s genet_branch -> "branch"
| s when Rdf_iri.equal s genet_intf -> "interface"
| s when Rdf_iri.equal s genet_version -> "version"
| s when Rdf_iri.equal s genet_filetype -> "filetype"
| s when Rdf_iri.equal s genet_chain -> "chain"
| s when Rdf_iri.equal s genet_flatchain -> "flatchain"
| s when Rdf_iri.equal s genet_instchain -> "instchain"
| s when Rdf_iri.equal s genet_instopn -> "instopn"
| s when Rdf_iri.equal s genet_diffcommand -> "diffcommand"
| _ -> ""
;;

let genet_name = genet_"name";;
let genet_desc = dc_"description";;
let genet_file_ext = genet_"file-extension";;
let genet_hasbranch = genet_"hasBranch";;
let genet_nointf = genet_"noInterface";;
let genet_haspath = genet_"hasPath";;
let genet_usetool = genet_"useTool";;
let genet_hasversion = genet_"hasVersion";;
let genet_consumes = genet_"consumes";;
let genet_produces = genet_"produces";;
let genet_hasdiffcom = genet_"hasDiffCommand";;
let genet_hasintf = genet_"hasInterface";;
let genet_hastype = genet_"hasFiletype";;
let genet_versionid = genet_"versionId";;
let genet_flattenedto = genet_"flattenedTo";;
let genet_opfrom = genet_"operationFrom";;
let genet_containsop = genet_"containsOperation";;
let genet_createdon = genet_"createdOn";;
let genet_startedon = genet_"startedOn";;
let genet_stoppedon = genet_"stoppedOn";;
let genet_isactive = genet_"isActive";;
let genet_instanciate = genet_"instanciate";;
let genet_hascommitid = genet_"hasCommitId";;
let genet_useinput = genet_"useInput";;
let genet_useinputcommitid = genet_"useInputCommitId";;
let genet_useversion = genet_"useVersion";;
let genet_filemd5 = genet_"fileMd5";;
let genet_hasimplode = genet_"hasImplode";;
let genet_returncode = genet_"returnCode";;
let genet_commandoutput = genet_"commandOutput";;
let genet_failedcommand = genet_"failedCommand";;
let genet_refinstfor = genet_"referenceInstFor";;

let add_triple wld ~sub ~pred ~obj =
  wld.wld_graph.add_triple ~sub ~pred ~obj
;;

let rem_triple wld ~sub ~pred ~obj =
  wld.wld_graph.rem_triple ~sub ~pred ~obj
;;

let add_type wld ~sub ~obj =
  let pred = rdf_type in
  add_triple wld ~sub ~pred ~obj
;;

let add_triple_iris wld ~sub ~pred ~obj =
  add_triple wld ~sub: (Iri sub) ~pred ~obj: (Iri obj)
;;

let rem_triple_iris wld ~sub ~pred ~obj =
  rem_triple wld ~sub: (Iri sub) ~pred ~obj: (Iri obj)
;;


(** {2 Iris of manipulated elements} *)

(** {3 Tools} *)

let suffix_tools = "tools";;
let iri_tools prefix = Rdf_iri.concat prefix suffix_tools;;
let iri_tool ~prefix ~tool =
  Rdf_iri.concat (iri_tools prefix) tool;;

(** {3 Chains} *)

let suffix_chains = "chains";;
let iri_chains prefix = Rdf_iri.concat prefix suffix_chains;;

let concat_chain_name modname name = Printf.sprintf "%s.%s" modname name;;
let split_chain_name s =
  match List.rev (Misc.split_string s ['/']) with
    [] -> failwith ("Invalid name "^s)
  | name :: rest ->
      if Misc.is_capitalized name then
        `Modname s
      else
        match rest with
          [] -> `Chainname name
        | _ -> `Fullname (String.concat "/" (List.rev rest), name)
;;

let iri_chain_module ~prefix modname =
  Rdf_iri.concat (iri_chains prefix) modname;;

let iri_chain ~prefix ~modname name  =
  Rdf_iri.concat (iri_chain_module ~prefix modname) name
;;

let is_in_ f_prefix prefix iri =
  let iri = Rdf_iri.string iri in
  let prefix = Rdf_iri.string (f_prefix prefix) in
  if Misc.is_prefix prefix iri then
    begin
      let len = String.length prefix in
      let base = String.sub iri len (String.length iri - len) in
      Some base
    end
  else
    None
;;

let is_in_chains = is_in_ iri_chains;;

let is_iri_chain_module prefix iri =
  match is_in_chains prefix iri with
    None -> None
  | Some base ->
      match split_chain_name base with
        `Modname modname -> Some modname
      | _ -> None
;;

let is_iri_chain prefix iri =
  match is_in_chains prefix iri with
    None -> None
  | Some base ->
      match split_chain_name base with
        `Fullname fullname -> Some fullname
      | _ -> None
;;

let chain_module_of_iri prefix iri =
  match is_iri_chain_module prefix iri with
    Some modname -> modname
  | None -> failwith (Printf.sprintf "%s is not a chain module iri" (Rdf_iri.string iri))
;;

(** {3 Flat chains} *)

let suffix_fchains = "flat-chains";;
let iri_fchains prefix = Rdf_iri.concat prefix suffix_fchains;;

let iri_fchain_module ~prefix modname =
  Rdf_iri.concat (iri_fchains prefix) modname;;

let split_fchain_name s =
  match Misc.split_string s ['/'] with
    [] -> failwith ("Invalid name "^s)
  | l ->
      let rec iter acc = function
        [] -> (List.rev acc, [])
      | s :: q ->
          if Misc.is_capitalized s then
            iter (s :: acc) q
          else
            (List.rev acc, s :: q)
      in
      let (modname, rest) = iter [] l in
      match rest with
        [] -> `Modname s
      | [name] -> `Fullname ((modname, name), "")
      | [name ; id] -> `Fullname ((modname, name), id)
      | name :: id :: path ->
          `Fchain_op ((modname, name), id, path)
;;
let iri_fchain ~prefix ~modname ?id ~name  =
  let iri = Rdf_iri.concat (iri_fchain_module ~prefix modname) name in
  match id with None -> iri | Some id -> Rdf_iri.concat iri id
;;

let  iri_fchain_op fchain path =
  List.fold_left Rdf_iri.concat fchain path
;;

let is_in_fchains = is_in_ iri_fchains;;

let is_iri_fchain_module prefix iri =
  match is_in_fchains prefix iri with
    None -> None
  | Some base ->
      match split_fchain_name base with
        `Modname modname -> Some modname
      | _ -> None
;;

let is_iri_fchain prefix iri =
  (*prerr_endline (Printf.sprintf "is_iri_fchain %S" (Rdf_iri.string iri));*)
  match is_in_fchains prefix iri with
    None -> None
  | Some base ->
      (*prerr_endline (Printf.sprintf "base=%s" base);*)
      match split_fchain_name base with
        `Fullname (fullname, id) -> Some (fullname, id)
      | _ -> None
;;

(** {3 Instanciated chains} *)

let suffix_ichains = "inst-chains";;
let suffix_producers_of = "producers-of";;
let suffix_consumers_of = "consumers-of";;

let iri_ichains prefix = Rdf_iri.concat prefix suffix_ichains;;

let iri_ichain_module ~prefix modname =
  Rdf_iri.concat (iri_ichains prefix) modname;;

let iri_ichain ~prefix ~modname ~name ~id  =
  let iri = Rdf_iri.concat (iri_ichain_module ~prefix modname) name in
  Rdf_iri.concat iri id
;;

let iri_ichain_file inst_iri file =
  List.fold_left Rdf_iri.concat inst_iri [ "files" ; file ]
;;

let ichain_in_file_rank iri =
  match List.rev (Rdf_iri.path iri) with
    [] -> failwith "inchain_in_file: empty iri"
  | n :: _ ->
      try int_of_string n
      with _ ->
          let msg = Printf.sprintf "Bad in_file iri: %s" (Rdf_iri.string iri) in
          failwith msg
;;

let iri_ichain_in_file iri rank =
  Rdf_iri.concat (Rdf_iri.concat iri "input") (string_of_int rank)
;;

let is_in_ichains = is_in_ iri_ichains;;

let is_iri_ichain prefix iri =
  (*prerr_endline (Printf.sprintf "is_iri_fchain %S" (Rdf_iri.string iri));*)
  match is_in_ichains prefix iri with
    None -> None
  | Some base ->
      (*prerr_endline (Printf.sprintf "base=%s" base);*)
      match split_fchain_name base with
        `Fullname (fullname, id) -> Some (fullname, id)
      | _ -> None
;;

let iri_ichains_producers_of prefix path =
  List.fold_left Rdf_iri.concat prefix
  (suffix_ichains :: suffix_producers_of :: path)
;;

let iri_ichains_consumers_of prefix path =
  List.fold_left Rdf_iri.concat prefix
  (suffix_ichains :: suffix_consumers_of :: path)
;;

(** {3 Versions} *)

let suffix_versions = "versions";;
let iri_versions prefix = Rdf_iri.concat prefix suffix_versions;;
let iri_version ~tool ~version =
  Rdf_iri.concat (iri_versions tool) version;;
let iri_tool_of_version iri = iri_parent (iri_parent iri);;

(** {3 Diff commands} *)

let suffix_diffcommands = "diff-commands"
let iri_diffcommands ~prefix = Rdf_iri.concat prefix suffix_diffcommands
let iri_diffcommand ~prefix ~name = Rdf_iri.concat (iri_diffcommands ~prefix) name

(** {3 Interfaces} *)

let suffix_intfs = "interfaces"
let iri_intfs ~tool = Rdf_iri.concat tool suffix_intfs;;
let iri_intf ~tool ~intf = Rdf_iri.concat (iri_intfs tool) intf;;
let iri_tool_of_intf iri = iri_parent (iri_parent iri);;
let iri_intf_in intf = Rdf_iri.concat intf "in";;
let iri_intf_out intf = Rdf_iri.concat intf "out";;
let iri_intf_in_port intf n =
  Rdf_iri.concat (iri_intf_in intf) (string_of_int n)
let iri_intf_out_port intf n =
  Rdf_iri.concat (iri_intf_out intf) (string_of_int n)
let port_rank iri =
  match List.rev (Rdf_iri.path iri) with
    [] -> failwith ("Invalid port iri: "^(Rdf_iri.string iri))
  | n :: _ ->
      try int_of_string n
      with _ -> failwith ("Invalid port number in iri: "^(Rdf_iri.string iri))
;;
let port_dir_string iri =
  match List.rev (Rdf_iri.path (iri_parent iri)) with
    [] -> failwith ("Bad port iri; "^(Rdf_iri.string iri))
  | s :: _ -> s
;;
let is_a_port iri =
  try ignore(port_rank iri) ; ignore(port_dir_string iri) ; true
  with _ -> false
;;
let port_container iri =
  if not (is_a_port iri) then
    failwith (Printf.sprintf "%s is not a port" (Rdf_iri.string iri));
  iri_parent (iri_parent iri)
;;

(** {3 Filetypes} *)

let suffix_filetypes = "filetypes" ;;
let iri_filetypes prefix = Rdf_iri.concat prefix suffix_filetypes;;
let iri_filetype ~prefix name =
  Rdf_iri.concat (iri_filetypes prefix) name;;

(** {3 Branches} *)

let suffix_branches = "branches";;
let iri_branches parent = Rdf_iri.concat parent suffix_branches;;
let iri_branch_from_parent_branch parent name = Rdf_iri.concat parent name;;
let iri_branch_from_parent_tool parent name =
  Rdf_iri.concat (Rdf_iri.concat parent suffix_branches) name;;

let remove_prefix prefix iri =
  let prefix = Rdf_iri.string prefix in
  let iri = Rdf_iri.string iri in
  if Misc.is_prefix prefix iri then
    begin
      let len_iri = String.length iri in
      let len_prefix = String.length prefix in
      let len_prefix =
        if len_prefix > 0 && prefix.[len_prefix - 1] = '/' then
          len_prefix - 1
        else
          len_prefix
      in
      String.sub iri len_prefix (len_iri - len_prefix)
    end
  else
    iri
;;

(** {3 Diffs} *)

let suffix_diff = "diff";;



(** Files and directories *)

let suffix_out = "out";;
let suffix_in = "in";;
let suffix_raw = "raw";;
let iri_outfile_path ?(raw=false) prefix path =
  List.fold_left Rdf_iri.concat prefix
    (if raw then suffix_out :: suffix_raw :: path else suffix_out :: path)
;;

let iri_input_path ?(raw=false) prefix path =
  List.fold_left Rdf_iri.concat prefix
  (if raw then suffix_in :: suffix_raw :: path else suffix_in :: path)
;;

let iri_input_file_path ?(raw=false) prefix path file_path =
  List.fold_left Rdf_iri.concat prefix
  ((if raw then suffix_in :: suffix_raw :: path else suffix_in :: path) @ file_path)
;;

(** {2 Utilities} *)

let iriset_of_list = List.fold_left (fun set x -> Iriset.add x set) Iriset.empty ;;

let iri_basename iri =
  match List.rev (Rdf_iri.path iri) with
    [] -> ""
  | s :: _ -> s
;;

let is_a wld ~sub ~obj =
  dbg ~level: 1 (fun () -> "Grdfs.is_a");
  let pred = rdf_type in
  dbg ~level: 2 (fun () -> "contains_statement ?");
  wld.wld_graph.exists_t (sub, pred, obj)
;;

let is_a_ iri =
  fun wld sub ->
    dbg ~level: 1 (fun () -> "Grdfs.is_a_ iri="^(Rdf_iri.string iri));
    let obj = Iri iri in
    let sub = Iri sub in
    is_a wld ~sub ~obj
;;

let is_a_tool = is_a_ genet_tool;;
let is_a_branch = is_a_ genet_branch;;
let is_a_version = is_a_ genet_version;;
let is_a_intf = is_a_ genet_intf;;
let is_a_filetype = is_a_ genet_filetype;;
let is_a_instopn = is_a_ genet_instopn;;
let is_a_diffcommand = is_a_ genet_diffcommand;;

let is_iri_for_ f_is string wld iri =
  match iri_basename iri with
    s when s = string ->
      begin
        let iri = iri_parent iri in
        if f_is wld iri then
          Some iri
        else
          None
      end
  | _ -> None;;

let is_iri_tool_ = is_iri_for_ is_a_tool;;
let is_iri_tool_versions = is_iri_tool_ "versions";;
let is_iri_tool_interfaces = is_iri_tool_ "interfaces";;
let is_iri_tool_branches = is_iri_tool_ "branches";;

let is_iri_branch_ = is_iri_for_ is_a_branch;;
let is_iri_branch_versions = is_iri_branch_ "versions";;
let is_iri_branch_interfaces = is_iri_branch_ "interfaces";;
let is_iri_branch_branches = is_iri_branch_ "branches";;

let is_iri_version_interfaces = is_iri_for_ is_a_version "interfaces";;

let add_name wld sub name =
  let pred = genet_name in
  let obj = Rdf_term.term_of_literal_string name in
  add_triple wld ~sub ~pred ~obj
;;

let add_desc wld sub desc =
  let pred = genet_desc in
  let obj = Rdf_term.term_of_literal_string desc in
  add_triple wld ~sub ~pred ~obj
;;

let object_literals wld ~sub ~pred =
  let l = wld.wld_graph.objects_of ~sub ~pred in
  let f acc = function
    Literal lit -> lit.lit_value :: acc
  | _ -> acc
  in
  List.fold_left f [] l
;;

let object_literal wld ~sub ~pred =
  match object_literals wld ~sub ~pred with
    [] -> None
  | s :: _ -> Some s
;;

let name wld source =
  Misc.string_of_opt (object_literal wld source genet_name)
;;

let name_of_iri_string wld source =
  let iri = Rdf_term.term_of_iri_string source in
  name wld iri
;;

let desc wld source =
  Misc.string_of_opt (object_literal wld source genet_desc)
;;

let subjects wld ~pred ~obj = wld.wld_graph.subjects_of ~pred ~obj;;
let objects wld ~sub ~pred = wld.wld_graph.objects_of ~sub ~pred;;

let subject_iris wld ~pred ~obj =
  let l = wld.wld_graph.subjects_of ~pred ~obj in
  let f acc = function
    Iri iri -> iri :: acc
  | _ -> acc
  in
  List.fold_left f [] l
;;

let subject_iri wld ~pred ~obj =
  match subject_iris wld ~pred ~obj with
    [] -> None
  | iri :: _ -> Some iri
;;

let object_iris wld ~sub ~pred =
  let l = wld.wld_graph.objects_of ~sub ~pred in
  let f acc = function
    Iri iri -> iri :: acc
  | _  -> acc
  in
  List.fold_left f [] l
;;

let object_iri wld ~sub ~pred =
  match object_iris wld ~sub ~pred with
    [] -> None
  | iri :: _ -> Some iri
;;

let remove_date_iri_ pred wld sub =
  let sub = Iri sub in
  let l = wld.wld_graph.objects_of ~sub ~pred in
  let f obj = rem_triple wld ~sub ~pred ~obj in
  List.iter f l
;;

let set_date_iri_ pred wld sub ?d () =
  remove_date_iri_ pred wld sub;
  add_triple wld
    ~sub: (Iri sub) ~pred
    ~obj: (Rdf_term.term_of_datetime ?d ())
;;
let date_iri_ pred wld sub =
  match wld.wld_graph.objects_of ~sub:(Iri sub) ~pred with
    [] -> None
  | [Literal lit] ->
      Some (Rdf_term.datetime_of_literal lit)
  | _ -> failwith (Printf.sprintf "Invalid %s object" (Rdf_iri.string pred))

let remove_creation_date_iri = remove_date_iri_ genet_createdon;;
let set_creation_date_iri = set_date_iri_ genet_createdon;;
let creation_date_iri = date_iri_ genet_createdon ;;

let remove_start_date_iri = remove_date_iri_ genet_startedon;;
let set_start_date_iri = set_date_iri_ genet_startedon;;
let start_date_iri = date_iri_ genet_startedon ;;

let remove_stop_date_iri = remove_date_iri_ genet_stoppedon;;
let set_stop_date_iri = set_date_iri_ genet_stoppedon;;
let stop_date_iri = date_iri_ genet_stoppedon ;;

let remove_command_output wld iri =
  let sub = Iri iri in
  let pred = genet_commandoutput in
  let l = wld.wld_graph.objects_of ~sub ~pred in
  let f obj = rem_triple wld ~sub ~pred ~obj in
  List.iter f l
;;
let command_output wld iri =
  let sub = Iri iri in
  let pred = genet_commandoutput in
  object_literal wld ~sub ~pred
;;
let set_command_output wld iri s =
   remove_command_output wld iri;
   let sub = Iri iri in
   let pred = genet_commandoutput in
   let obj = Rdf_term.term_of_literal_string s in
   add_triple wld ~sub ~pred ~obj
;;

let remove_is_active_iri wld sub =
  let sub = Iri sub in
  let pred = genet_isactive in
  let l = wld.wld_graph.objects_of ~sub ~pred in
  let f obj = rem_triple wld ~sub ~pred ~obj in
  List.iter f l
;;

let set_is_active_iri wld sub b =
  remove_is_active_iri wld sub;
  add_triple wld
    ~sub: (Iri sub) ~pred: genet_isactive
    ~obj: (Rdf_term.term_of_bool b)
;;

let is_active_iri wld sub =
  let pred = genet_isactive in
  match wld.wld_graph.objects_of ~sub:(Iri sub) ~pred with
    [] -> true
  | [Literal lit] -> Rdf_term.bool_of_literal lit
  | _ -> failwith "Invalid isactive object"
;;

(*
let fold_target_sequence f acc wld ~source ~pred =
  let sparql =
    { Rdf_sparql.select_proj = [ "seq_index" ; "iri" ];
      select_distinct = None ;
      select_where =
      ([
         (`I source, [ `I pred, [`V "seq"] ]) ;
         (`V "seq", [ `V "seq_index", [`V "iri"] ]) ;
       ], None)
    }
  in
  let qf acc qr =
    match
      Rdf_query_results.get_binding_value_by_name qr "seq_index",
      Rdf_query_results.get_binding_value_by_name qr "iri"
    with
    | None, _ | _, None -> acc
    | Some index_node, Some iri_node ->
        let n = Rdf_node.get_li_ordinal index_node in
        f acc n iri_node
  in
  Rdf_sparql.select_and_fold wld.wld_world wld.wld_model sparql qf acc
;;
*)

(*
let delete_from_sparql wld query =
  prerr_endline
  (Printf.sprintf "delete from sparql query=%s" (Rdf_sparql.string_of_construct query));
  let _stream = Rdf_sparql.exec_construct wld query in
  assert false
  (*Rdf_stream.iter
  (fun st ->
     prerr_endline (Rdf_statement.to_string st);
     Rdf_model.remove_statement wld.wld_model st
  )
  stream*)
;;
*)

let class_of wld sub =
  object_iri wld ~sub ~pred: rdf_type;;

let class_of_iri_string wld iri =
  class_of wld (Rdf_term.term_of_iri_string iri)
;;


