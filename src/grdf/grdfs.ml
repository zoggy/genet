(*********************************************************************************)
(*                Genet                                                          *)
(*                                                                               *)
(*    Copyright (C) 2012 Institut National de Recherche en Informatique          *)
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
open Rdf_node;;
open Rdf_graph;;

let dbg = Misc.create_log_fun
  ~prefix: "Grdfs"
    "GENET_GRDFS_DEBUG_LEVEL"
;;

(** {2 Vocabulary} *)

let genet = Rdf_uri.uri "https://raw.github.com/zoggy/genet/master/doc/genet.ttl";;
let genet_ = Rdf_uri.set_fragment genet;;

let rdf_ = Rdf_uri.set_fragment (Rdf_uri.uri "http://www.w3.org/1999/02/22-rdf-syntax-ns");;
let rdfs_ = Rdf_uri.set_fragment (Rdf_uri.uri "http://www.w3.org/2000/01/rdf-schema");;
let dc_ = Rdf_uri.concat (Rdf_uri.uri "http://purl.org/dc/elements/1.1");;
let owl_ = Rdf_uri.set_fragment (Rdf_uri.uri "http://www.w3.org/2002/07/owl");;
let xsd_ = Rdf_uri.set_fragment (Rdf_uri.uri "http://www.w3.org/2001/XMLSchema");;
let foaf_ = Rdf_uri.concat (Rdf_uri.uri "http://xmlns.com/foaf/0.1");;

let datatype_integer = xsd_"integer";;

let namespaces = [ genet_"", "genet" ; rdfs_"", "rdfs" ; dc_"", "dc" ; xsd_"", "xsd" ]

let li_ n =
  if n <= 0 then
    raise (Invalid_argument (Printf.sprintf "li: n=%d <= 0" n))
  else
    rdf_ (Printf.sprintf "li%n" n)
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

let string_of_class = function
  s when Rdf_uri.equal s genet_tool -> "tool"
| s when Rdf_uri.equal s genet_branch -> "branch"
| s when Rdf_uri.equal s genet_intf -> "interface"
| s when Rdf_uri.equal s genet_version -> "version"
| s when Rdf_uri.equal s genet_filetype -> "filetype"
| s when Rdf_uri.equal s genet_chain -> "chain"
| s when Rdf_uri.equal s genet_flatchain -> "flatchain"
| s when Rdf_uri.equal s genet_instchain -> "instchain"
| s when Rdf_uri.equal s genet_instopn -> "instopn"
| _ -> ""
;;

let genet_name = genet_"name";;
let genet_desc = dc_"description";;
let genet_file_ext = genet_"file-extension";;
let genet_hasbranch = genet_"hasBranch";;
let genet_nointf = genet_"noInterface";;
let genet_haspath = genet_"hasPath";;
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

let add_triple wld ~sub ~pred ~obj =
  wld.wld_graph.add_triple ~sub ~pred ~obj
;;

let rem_triple wld ~sub ~pred ~obj =
  wld.wld_graph.rem_triple ~sub ~pred ~obj
;;

let add_type wld ~sub ~obj =
  let pred = Uri rdf_type in
  add_triple wld ~sub ~pred ~obj
;;

let add_triple_uris wld ~sub ~pred ~obj =
  add_triple wld ~sub: (Uri sub) ~pred: (Uri pred) ~obj: (Uri obj)
;;

let rem_triple_uris wld ~sub ~pred ~obj =
  rem_triple wld ~sub: (Uri sub) ~pred: (Uri pred) ~obj: (Uri obj)
;;


(** {2 Uris of manipulated elements} *)

(** {3 Tools} *)

let suffix_tools = "tools";;
let uri_tools prefix = Rdf_uri.concat prefix suffix_tools;;
let uri_tool ~prefix ~tool =
  Rdf_uri.concat (uri_tools prefix) tool;;

(** {3 Chains} *)

let suffix_chains = "chains";;
let uri_chains prefix = Rdf_uri.concat prefix suffix_chains;;

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

let uri_chain_module ~prefix modname =
  Rdf_uri.concat (uri_chains prefix) modname;;

let uri_chain ~prefix ~modname name  =
  Rdf_uri.concat (uri_chain_module ~prefix modname) name
;;

let is_in_ f_prefix prefix uri =
  let uri = Rdf_uri.string uri in
  let prefix = Rdf_uri.string (f_prefix prefix) in
  if Misc.is_prefix prefix uri then
    begin
      let len = String.length prefix in
      let base = String.sub uri len (String.length uri - len) in
      Some base
    end
  else
    None
;;

let is_in_chains = is_in_ uri_chains;;

let is_uri_chain_module prefix uri =
  match is_in_chains prefix uri with
    None -> None
  | Some base ->
      match split_chain_name base with
        `Modname modname -> Some modname
      | _ -> None
;;

let is_uri_chain prefix uri =
  match is_in_chains prefix uri with
    None -> None
  | Some base ->
      match split_chain_name base with
        `Fullname fullname -> Some fullname
      | _ -> None
;;

let chain_module_of_uri prefix uri =
  match is_uri_chain_module prefix uri with
    Some modname -> modname
  | None -> failwith (Printf.sprintf "%s is not a chain module uri" (Rdf_uri.string uri))
;;

(** {3 Flat chains} *)

let suffix_fchains = "flat-chains";;
let uri_fchains prefix = Rdf_uri.concat prefix suffix_fchains;;

let uri_fchain_module ~prefix modname =
  Rdf_uri.concat (uri_fchains prefix) modname;;

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
let uri_fchain ~prefix ~modname ?id ~name  =
  let uri = Rdf_uri.concat (uri_fchain_module ~prefix modname) name in
  match id with None -> uri | Some id -> Rdf_uri.concat uri id
;;

let  uri_fchain_op fchain path =
  List.fold_left Rdf_uri.concat fchain path
;;

let is_in_fchains = is_in_ uri_fchains;;

let is_uri_fchain_module prefix uri =
  match is_in_fchains prefix uri with
    None -> None
  | Some base ->
      match split_fchain_name base with
        `Modname modname -> Some modname
      | _ -> None
;;

let is_uri_fchain prefix uri =
  prerr_endline (Printf.sprintf "is_uri_fchain %S" (Rdf_uri.string uri));
  match is_in_fchains prefix uri with
    None -> None
  | Some base ->
      prerr_endline (Printf.sprintf "base=%s" base);
      match split_fchain_name base with
        `Fullname (fullname, id) -> Some (fullname, id)
      | _ -> None
;;

(** {3 Instanciated chains} *)

let suffix_ichains = "inst-chains";;
let suffix_producers_of = "producers-of";;
let suffix_consumers_of = "consumers-of";;

let uri_ichains prefix = Rdf_uri.concat prefix suffix_ichains;;

let uri_ichain_module ~prefix modname =
  Rdf_uri.concat (uri_ichains prefix) modname;;

let uri_ichain ~prefix ~modname ~name ~id  =
  let uri = Rdf_uri.concat (uri_ichain_module ~prefix modname) name in
  Rdf_uri.concat uri id
;;

let uri_ichain_file inst_uri file =
  List.fold_left Rdf_uri.concat inst_uri [ "files" ; file ]
;;

let ichain_in_file_rank uri =
  match List.rev (Rdf_uri.path uri) with
    [] -> failwith "inchain_in_file: empty uri"
  | n :: _ ->
      try int_of_string n
      with _ ->
          let msg = Printf.sprintf "Bad in_file uri: %s" (Rdf_uri.string uri) in
          failwith msg
;;

let uri_ichain_in_file uri rank =
  Rdf_uri.concat (Rdf_uri.concat uri "input") (string_of_int rank)
;;

let is_in_ichains = is_in_ uri_ichains;;

let is_uri_ichain prefix uri =
  prerr_endline (Printf.sprintf "is_uri_fchain %S" (Rdf_uri.string uri));
  match is_in_ichains prefix uri with
    None -> None
  | Some base ->
      prerr_endline (Printf.sprintf "base=%s" base);
      match split_fchain_name base with
        `Fullname (fullname, id) -> Some (fullname, id)
      | _ -> None
;;

let uri_ichains_producers_of prefix path =
  List.fold_left Rdf_uri.concat prefix
  (suffix_ichains :: suffix_producers_of :: path)
;;

let uri_ichains_consumers_of prefix path =
  List.fold_left Rdf_uri.concat prefix
  (suffix_ichains :: suffix_consumers_of :: path)
;;

(** {3 Versions} *)

let suffix_versions = "versions";;
let uri_versions prefix = Rdf_uri.concat prefix suffix_versions;;
let uri_version ~tool ~version =
  Rdf_uri.concat (uri_versions tool) version;;
let uri_tool_of_version uri = Rdf_uri.parent (Rdf_uri.parent uri);;

(** {3 Interfaces} *)

let suffix_intfs = "interfaces"
let uri_intfs ~tool = Rdf_uri.concat tool suffix_intfs;;
let uri_intf ~tool ~intf = Rdf_uri.concat (uri_intfs tool) intf;;
let uri_tool_of_intf uri = Rdf_uri.parent (Rdf_uri.parent uri);;
let uri_intf_in intf = Rdf_uri.concat intf "in";;
let uri_intf_out intf = Rdf_uri.concat intf "out";;
let uri_intf_in_port intf n =
  Rdf_uri.concat (uri_intf_in intf) (string_of_int n)
let uri_intf_out_port intf n =
  Rdf_uri.concat (uri_intf_out intf) (string_of_int n)
let port_rank uri =
  match List.rev (Rdf_uri.path uri) with
    [] -> failwith ("Invalid port uri: "^(Rdf_uri.string uri))
  | n :: _ ->
      try int_of_string n
      with _ -> failwith ("Invalid port number in uri: "^(Rdf_uri.string uri))
;;
let port_dir_string uri =
  match List.rev (Rdf_uri.path (Rdf_uri.parent uri)) with
    [] -> failwith ("Bad port uri; "^(Rdf_uri.string uri))
  | s :: _ -> s
;;
let is_a_port uri =
  try ignore(port_rank uri) ; ignore(port_dir_string uri) ; true
  with _ -> false
;;
let port_container uri =
  if not (is_a_port uri) then
    failwith (Printf.sprintf "%s is not a port" (Rdf_uri.string uri));
  Rdf_uri.parent (Rdf_uri.parent uri)
;;

(** {3 Filetypes} *)

let suffix_filetypes = "filetypes" ;;
let uri_filetypes prefix = Rdf_uri.concat prefix suffix_filetypes;;
let uri_filetype ~prefix name =
  Rdf_uri.concat (uri_filetypes prefix) name;;

(** {3 Branches} *)

let suffix_branches = "branches";;
let uri_branches parent = Rdf_uri.concat parent suffix_branches;;
let uri_branch_from_parent_branch parent name = Rdf_uri.concat parent name;;
let uri_branch_from_parent_tool parent name =
  Rdf_uri.concat (Rdf_uri.concat parent suffix_branches) name;;

let remove_prefix prefix uri =
  let prefix = Rdf_uri.string prefix in
  let uri = Rdf_uri.string uri in
  if Misc.is_prefix prefix uri then
    begin
      let len_uri = String.length uri in
      let len_prefix = String.length prefix in
      let len_prefix =
        if len_prefix > 0 && prefix.[len_prefix - 1] = '/' then
          len_prefix - 1
        else
          len_prefix
      in
      String.sub uri len_prefix (len_uri - len_prefix)
    end
  else
    uri
;;

(** Files and directories *)

let suffix_out = "out";;
let suffix_in = "in";;
let suffix_raw = "raw";;
let uri_outfile_path ?(raw=false) prefix path =
  List.fold_left Rdf_uri.concat prefix
    (if raw then suffix_out :: suffix_raw :: path else suffix_out :: path)
;;

let uri_input_path prefix path =
  List.fold_left Rdf_uri.concat prefix (suffix_in :: path)
;;

let uri_input_file_path ?(raw=false) prefix path file_path =
  List.fold_left Rdf_uri.concat prefix
  ((if raw then suffix_in :: suffix_raw :: path else suffix_in :: path) @ file_path)
;;

(** {2 Utilities} *)

let uri_basename uri =
  match List.rev (Rdf_uri.path uri) with
    [] -> ""
  | s :: _ -> s
;;

let is_a wld ~sub ~obj =
  dbg ~level: 1 (fun () -> "Grdfs.is_a");
  let pred = Uri rdf_type in
  dbg ~level: 2 (fun () -> "contains_statement ?");
  wld.wld_graph.exists_t (sub, pred, obj)
;;

let is_a_ uri =
  fun wld sub ->
    dbg ~level: 1 (fun () -> "Grdfs.is_a_ uri="^(Rdf_uri.string uri));
    let obj = Uri uri in
    let sub = Uri sub in
    is_a wld ~sub ~obj
;;

let is_a_tool = is_a_ genet_tool;;
let is_a_branch = is_a_ genet_branch;;
let is_a_version = is_a_ genet_version;;
let is_a_intf = is_a_ genet_intf;;
let is_a_filetype = is_a_ genet_filetype;;
let is_a_instopn = is_a_ genet_instopn;;

let is_uri_for_ f_is string wld uri =
  match uri_basename uri with
    s when s = string ->
      begin
        let uri = Rdf_uri.parent uri in
        if f_is wld uri then
          Some uri
        else
          None
      end
  | _ -> None;;

let is_uri_tool_ = is_uri_for_ is_a_tool;;
let is_uri_tool_versions = is_uri_tool_ "versions";;
let is_uri_tool_interfaces = is_uri_tool_ "interfaces";;
let is_uri_tool_branches = is_uri_tool_ "branches";;

let is_uri_branch_ = is_uri_for_ is_a_branch;;
let is_uri_branch_versions = is_uri_branch_ "versions";;
let is_uri_branch_interfaces = is_uri_branch_ "interfaces";;
let is_uri_branch_branches = is_uri_branch_ "branches";;

let is_uri_version_interfaces = is_uri_for_ is_a_version "interfaces";;

let add_name wld sub name =
  let pred = Uri genet_name in
  let obj = Rdf_node.node_of_literal_string name in
  add_triple wld ~sub ~pred ~obj
;;

let add_desc wld sub desc =
  let pred = Uri genet_desc in
  let obj = Rdf_node.node_of_literal_string desc in
  add_triple wld ~sub ~pred ~obj
;;

let object_literals wld ~sub ~pred =
  let pred = Uri pred in
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

let name_of_uri_string wld source =
  let uri = Rdf_node.node_of_uri_string source in
  name wld uri
;;

let desc wld source =
  Misc.string_of_opt (object_literal wld source genet_desc)
;;

let subjects wld ~pred ~obj = wld.wld_graph.subjects_of ~pred ~obj;;
let objects wld ~sub ~pred = wld.wld_graph.objects_of ~sub ~pred;;

let subject_uris wld ~pred ~obj =
  let pred = Uri pred in
  let l = wld.wld_graph.subjects_of ~pred ~obj in
  let f acc = function
    Uri uri -> uri :: acc
  | _ -> acc
  in
  List.fold_left f [] l
;;

let subject_uri wld ~pred ~obj =
  match subject_uris wld ~pred ~obj with
    [] -> None
  | uri :: _ -> Some uri
;;

let object_uris wld ~sub ~pred =
  let pred = Uri pred in
  let l = wld.wld_graph.objects_of ~sub ~pred in
  let f acc = function
    Uri uri -> uri :: acc
  | _  -> acc
  in
  List.fold_left f [] l
;;

let object_uri wld ~sub ~pred =
  match object_uris wld ~sub ~pred with
    [] -> None
  | uri :: _ -> Some uri
;;

let remove_date_uri_ pred wld sub =
  let sub = Uri sub in
  let pred = Uri pred in
  let l = wld.wld_graph.objects_of ~sub ~pred in
  let f obj = rem_triple wld ~sub ~pred ~obj in
  List.iter f l
;;

let set_date_uri_ pred wld sub ?d () =
  remove_date_uri_ pred wld sub;
  add_triple wld
    ~sub: (Uri sub) ~pred: (Uri pred)
    ~obj: (Rdf_node.node_of_datetime ?d ())
;;
let date_uri_ pred wld sub =
  let uri_pred = Uri pred in
  match wld.wld_graph.objects_of ~sub:(Uri sub) ~pred: uri_pred with
    [] -> None
  | [Literal lit] ->
      Some (Rdf_node.datetime_of_literal lit)
  | _ -> failwith (Printf.sprintf "Invalid %s object" (Rdf_uri.string pred))

let remove_creation_date_uri = remove_date_uri_ genet_createdon;;
let set_creation_date_uri = set_date_uri_ genet_createdon;;
let creation_date_uri = date_uri_ genet_createdon ;;

let remove_start_date_uri = remove_date_uri_ genet_startedon;;
let set_start_date_uri = set_date_uri_ genet_startedon;;
let start_date_uri = date_uri_ genet_startedon ;;

let remove_stop_date_uri = remove_date_uri_ genet_stoppedon;;
let set_stop_date_uri = set_date_uri_ genet_stoppedon;;
let stop_date_uri = date_uri_ genet_stoppedon ;;


let remove_is_active_uri wld sub =
  let sub = Uri sub in
  let pred = Uri genet_isactive in
  let l = wld.wld_graph.objects_of ~sub ~pred in
  let f obj = rem_triple wld ~sub ~pred ~obj in
  List.iter f l
;;

let set_is_active_uri wld sub b =
  remove_is_active_uri wld sub;
  add_triple wld
    ~sub: (Uri sub) ~pred: (Uri genet_isactive)
    ~obj: (Rdf_node.node_of_bool b)
;;

let is_active_uri wld sub =
  let pred = Uri genet_isactive in
  match wld.wld_graph.objects_of ~sub:(Uri sub) ~pred with
    [] -> false
  | [Literal lit] -> Rdf_node.bool_of_literal lit
  | _ -> failwith "Invalid isactive object"
;;

(*
let fold_target_sequence f acc wld ~source ~pred =
  let sparql =
    { Rdf_sparql.select_proj = [ "seq_index" ; "uri" ];
      select_distinct = None ;
      select_where =
      ([
         (`I source, [ `I pred, [`V "seq"] ]) ;
         (`V "seq", [ `V "seq_index", [`V "uri"] ]) ;
       ], None)
    }
  in
  let qf acc qr =
    match
      Rdf_query_results.get_binding_value_by_name qr "seq_index",
      Rdf_query_results.get_binding_value_by_name qr "uri"
    with
    | None, _ | _, None -> acc
    | Some index_node, Some uri_node ->
        let n = Rdf_node.get_li_ordinal index_node in
        f acc n uri_node
  in
  Rdf_sparql.select_and_fold wld.wld_world wld.wld_model sparql qf acc
;;
*)


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


let class_of wld sub =
  object_uri wld ~sub ~pred: rdf_type;;

let class_of_uri_string wld uri =
  class_of wld (Rdf_node.node_of_uri_string uri)
;;


