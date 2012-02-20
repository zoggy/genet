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

let add_stmt world model ~sub ~pred ~obj =
  Rdf_model.add_statement model
  (Rdf_statement.new_from_nodes world ~sub ~pred ~obj)
;;

let add_type world model ~sub ~obj =
  let pred = Rdf_node.new_from_uri_string world rdf_type in
  add_stmt world model ~sub ~pred ~obj
;;

(** {2 Uris of manipulated elements} *)

let uri_tool ~pref ~tool =
  Printf.sprintf "%s/tools/%s" pref tool;;

let uri_chain ~pref ~chain =
  Printf.sprintf "%s/chains/%s" pref chain;;

let uri_version ~tool ~version =
  Printf.sprintf "%s/versions/%s" tool version;;

let uri_intf ~tool ~intf =
  Printf.sprintf "%s/interfaces/%s" tool intf;;

let uri_filetype ~pref name =
  Printf.sprintf "%s/filetypes/%s" pref name;;

let uri_branch_from_parent_branch parent name = Printf.sprintf "%s/%s" parent name;;
let uri_branch_from_parent_tool parent name = Printf.sprintf "%s/branches/%s" parent name;;

(** {2 Utilities} *)

let is_a world model ~sub ~obj =
  dbg ~level: 1 (fun () -> "Grdfs.is_a");
  let pred = Rdf_node.new_from_uri_string world rdf_type in
  let stmt = Rdf_statement.new_from_nodes world ~sub ~pred ~obj in
  dbg ~level: 2 (fun () -> "contains_statement ?");
  let stream = Rdf_model.find_statements model stmt in
  not (Rdf_stream.is_at_end stream)
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


