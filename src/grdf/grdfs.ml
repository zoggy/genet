(** *)

(** {2 Vocabulary} *)

let genet = "http://gitorious.org/genet/genet/blobs/raw/master/doc/genet.rdf";;
let genet_ s = genet^"#"^s;;

let rdf_ s = "http://www.w3.org/1999/02/22-rdf-syntax-ns#" ^ s ;;
let rdfs_ s = "http://www.w3.org/2000/01/rdf-schema#" ^ s;;
let dc_ s = "http://purl.org/dc/elements/1.1/" ^ s;;
let owl_ s = "http://www.w3.org/2002/07/owl#" ^ s;;
let xsd_ s = "http://www.w3.org/2001/XMLSchema#" ^ s;;
let foaf_ s = "http://xmlns.com/foaf/0.1/" ^ s;;

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
  let pred = Rdf_node.new_from_uri_string world (rdf_"type") in
  add_stmt world model ~sub ~pred ~obj
;;

(** {2 Uris of manipulated elements} *)

let uri_tool ~pref ~tool =
  Printf.sprintf "%s/tools/%s" pref tool;;

let uri_chain ~pref ~chain =
  Printf.sprintf "%s/chains/%s" pref chain;;
