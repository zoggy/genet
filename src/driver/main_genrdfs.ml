(** Main module of the program generating the RDFschema of genet. *)

let add_stmt world model ~sub ~pred ~obj =
  Rdf_model.add_statement model
  (Rdf_statement.new_from_nodes world ~sub ~pred ~obj)
;;

let add_type world model ~sub ~obj =
  let pred = Rdf_node.new_from_uri_string world (Grdfs.rdf_"type") in
  add_stmt world model ~sub ~pred ~obj
;;

let add_istype_class world model sub =
  let obj = Rdf_node.new_from_uri_string world (Grdfs.rdfs_"Class") in
  add_type world model ~sub ~obj
;;

let add_vocabulary world model =
  let uri_tool = Rdf_node.new_from_uri_string world Grdfs.genet_tool in
  add_istype_class world model uri_tool
;;

let options =
  Options.option_version "Genet-genrdfs"::
  []
;;

let main () =
  let _options = Options.parse options in
  let world = Rdf_init.new_world () in

  Rdf_init.open_world world;
  let storage = Rdf_storage.new_storage world ~factory: "memory" ~name: "test" in
  let model = Rdf_model.new_model world storage in
  add_vocabulary world model;
  match Rdf_model.to_string model ~name: "ntriples" with
    None -> failwith "Failed to serialize model"
  | Some string -> print_string string
;;

let _ = Misc.safe_main main;;