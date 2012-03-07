(** *)

open Grdf_types;;

let dbg = Misc.create_log_fun
  ~prefix: "Grdfs"
    "GENET_GRDFS_DEBUG_LEVEL"
;;

(** {2 Vocabulary} *)

let genet = "http://gitorious.org/genet/genet/blobs/raw/master/doc/genet.rdf";;
let genet_ s = genet^"#"^s;;

let rdf_ s = "http://www.w3.org/1999/02/22-rdf-syntax-ns#" ^ s ;;
let rdfs_ s = "http://www.w3.org/2000/01/rdf-schema#" ^ s;;
let dc_ s = "http://purl.org/dc/elements/1.1/" ^ s;;
let owl_ s = "http://www.w3.org/2002/07/owl#" ^ s;;
let xsd_ s = "http://www.w3.org/2001/XMLSchema#" ^ s;;
let foaf_ s = "http://xmlns.com/foaf/0.1/" ^ s;;

let li = rdf_ "_";;
let li_ n =
  if n <= 0 then
    raise (Invalid_argument (Printf.sprintf "li: n=%d <= 0" n))
  else
    Printf.sprintf "%s%n" li n
;;

let rdf_type = rdf_"type";;

let genet_tool = genet_ "Tool";;
let genet_branch = genet_ "Branch";;
let genet_intf = genet_ "Interface";;
let genet_version = genet_ "Version";;
let genet_filetype = genet_ "Filetype";;

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
let genet_listof = genet_"listOf";;

let add_stmt world model ~sub ~pred ~obj =
  Rdf_model.add_statement model
  (Rdf_statement.new_from_nodes world ~sub ~pred ~obj)
;;

let remove_stmt world model ~sub ~pred ~obj =
  Rdf_model.remove_statement model
  (Rdf_statement.new_from_nodes world ~sub ~pred ~obj)
;;

let add_type world model ~sub ~obj =
  let pred = Rdf_node.new_from_uri_string world rdf_type in
  add_stmt world model ~sub ~pred ~obj
;;

(** {2 Uris of manipulated elements} *)

let uri_tools = "/tools";;
let uri_tool ~tool =
  Printf.sprintf "%s/%s" uri_tools tool;;

let uri_chains = "/chains";;
let uri_chain ~chain =
  Printf.sprintf "%s/%s" uri_chains chain;;

let uri_versions ~tool = Printf.sprintf "%s/versions" tool;;
let uri_version ~tool ~version =
  Printf.sprintf "%s/%s" (uri_versions tool) version;;

let uri_intfs ~tool = Printf.sprintf "%s/interfaces" tool;;
let uri_intf ~tool ~intf =
  Printf.sprintf "%s/%s" (uri_intfs tool) intf;;

let uri_filetypes = "/filetypes";;
let uri_filetype name =
  Printf.sprintf "%s/%s" uri_filetypes name;;

let uri_branch_from_parent_branch parent name = Printf.sprintf "%s/%s" parent name;;
let uri_branch_from_parent_tool parent name = Printf.sprintf "%s/branches/%s" parent name;;

(** {2 Utilities} *)



let is_a world model ~sub ~obj =
  dbg ~level: 1 (fun () -> "Grdfs.is_a");
  let pred = Rdf_node.new_from_uri_string world rdf_type in
  let stmt = Rdf_statement.new_from_nodes world ~sub ~pred ~obj in
  dbg ~level: 2 (fun () -> "contains_statement ?");
  Rdf_model.contains_statement model stmt
;;

let is_a_ uri =
  fun wld sub ->
    dbg ~level: 1 (fun () -> "Grdfs.is_a_ uri="^uri);
    let obj = Rdf_node.new_from_uri_string wld.wld_world uri in
    is_a wld.wld_world wld.wld_model ~sub ~obj
;;

let is_a_tool = is_a_ genet_tool;;
let is_a_branch = is_a_ genet_branch;;
let is_a_version = is_a_ genet_version;;
let is_a_intf = is_a_ genet_intf;;

let add_name wld sub name =
  let pred = Rdf_node.new_from_uri_string wld.wld_world genet_name in
  let obj = Rdf_node.new_from_literal wld.wld_world name in
  add_stmt wld.wld_world wld.wld_model ~sub ~pred ~obj
;;

let add_desc wld sub desc =
  let pred = Rdf_node.new_from_uri_string wld.wld_world genet_desc in
  let obj = Rdf_node.new_from_literal wld.wld_world desc in
  add_stmt wld.wld_world wld.wld_model ~sub ~pred ~obj
;;

let target_literals wld source pred =
  let arc = Rdf_node.new_from_uri_string wld.wld_world pred in
  let iterator = Rdf_model.get_targets wld.wld_model ~source ~arc in
  let f acc node =
    match Rdf_node.get_literal_value node with
      None -> acc
    | Some s -> s :: acc
  in
  Rdf_iterator.fold_objects iterator Rdf_node.copy_node f
;;

let target_literal wld source pred =
  match target_literals wld source pred with
    [] -> None
  | s :: _ -> Some s
;;

let name wld source =
  Misc.string_of_opt (target_literal wld source genet_name)
;;

let desc wld source =
  Misc.string_of_opt (target_literal wld source genet_desc)
;;

let source_uri wld pred target =
  let arc = Rdf_node.new_from_uri_string wld.wld_world pred in
  let iterator = Rdf_model.get_sources wld.wld_model ~target ~arc in
  if Rdf_iterator.is_at_end iterator then
    None
  else
    match Rdf_iterator.get_object iterator Rdf_node.copy_node with
      None -> None
    | Some node ->
        match Rdf_node.get_uri node with
          None -> None
        | Some uri -> Some (Rdf_uri.as_string uri)
;;

let target_uris wld source pred =
  let arc = Rdf_node.new_from_uri_string wld.wld_world pred in
  let iterator = Rdf_model.get_targets wld.wld_model ~source ~arc in
  let f acc node =
    match Rdf_node.get_uri node with
      None -> acc
    | Some uri ->
        let s_uri = Rdf_uri.as_string uri in
        s_uri :: acc
  in
  Rdf_iterator.fold_objects iterator Rdf_node.copy_node f
;;

let target_uri wld source pred =
  match target_uris wld source pred with
    [] -> None
  | uri :: _ -> Some uri
;;

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
      None, _ | _, None -> acc
    | Some index_node, Some uri_node ->
        let n = Rdf_node.get_li_ordinal index_node in
        f acc n uri_node
  in
  Rdf_sparql.select_and_fold wld.wld_world wld.wld_model sparql qf acc
;;

(*
let delete_from_sparql wld query =
  prerr_endline
  (Printf.sprintf "delete from sparql query=%s" (Rdf_sparql.string_of_construct query));
  let stream = Rdf_sparql.exec_construct wld.wld_world wld.wld_model query in
  Rdf_stream.iter
  (fun st ->
     prerr_endline (Rdf_statement.to_string st);
     Rdf_model.remove_statement wld.wld_model st
  )
  stream
;;
*)

let class_of wld sub =
  target_uri wld sub rdf_type;;


