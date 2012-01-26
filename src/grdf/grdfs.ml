(** *)

open Grdf_types;;

let dbg = Misc.create_log_fun
  ~prefix: "Algo_types"
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

let add_name wld sub name =
  let pred = Rdf_node.new_from_uri_string wld.wld_world genet_name in
  (* FIXME convert to UTF 8 ?*)
  let obj = Rdf_node.new_from_literal wld.wld_world name in
  add_stmt wld.wld_world wld.wld_model ~sub ~pred ~obj
;;


