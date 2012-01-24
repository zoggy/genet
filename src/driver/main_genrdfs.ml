(** Main module of the program generating the RDFschema of genet. *)


let add_stmt world model ~sub ~pred ~obj =
  Rdf_model.add_statement model
  (Rdf_statement.new_from_nodes world ~sub ~pred ~obj)
;;

let add_isdefined_by_genet world model sub =
  let pred = Rdf_node.new_from_uri_string world (Grdfs.rdfs_"isDefinedBy") in
  let obj = Rdf_node.new_from_uri_string world Grdfs.genet in
  add_stmt world model ~sub ~pred ~obj
;;

let add_label world model sub label =
  let pred = Rdf_node.new_from_uri_string world (Grdfs.rdfs_"label") in
  let obj = Rdf_node.new_from_literal world label in
  add_stmt world model ~sub ~pred ~obj
;;

let add_comment world model sub label =
  let pred = Rdf_node.new_from_uri_string world (Grdfs.rdfs_"comment") in
  let obj = Rdf_node.new_from_literal world label in
  add_stmt world model ~sub ~pred ~obj
;;

let add_type world model ~sub ~obj =
  let pred = Rdf_node.new_from_uri_string world (Grdfs.rdf_"type") in
  add_stmt world model ~sub ~pred ~obj
;;

let add_istype_class world model sub =
  let obj = Rdf_node.new_from_uri_string world (Grdfs.rdfs_"Class") in
  add_type world model ~sub ~obj
;;

let add_istype_property world model sub =
  let obj = Rdf_node.new_from_uri_string world (Grdfs.rdfs_"Property") in
  add_type world model ~sub ~obj
;;

let add_property world model
  ~label ~comment ~definedby ?domain ?range sub =
  add_istype_property world model sub;
  add_label world model sub label;
  add_comment world model sub comment;

  let pred = Rdf_node.new_from_uri_string world (Grdfs.rdfs_"isDefinedBy") in
  add_stmt world model ~sub ~pred ~obj: definedby;

  (match domain with
     None -> ()
   | Some obj ->
       let pred = Rdf_node.new_from_uri_string world (Grdfs.rdfs_"domain") in
       add_stmt world model ~sub ~pred ~obj
  );

  (match range with
     None -> ()
   | Some obj ->
       let pred = Rdf_node.new_from_uri_string world (Grdfs.rdfs_"range") in
       add_stmt world model ~sub ~pred ~obj
  )
;;

let add_tool_class world model =
  let uri_tool = Rdf_node.new_from_uri_string world Grdfs.genet_tool in
  add_istype_class world model uri_tool;
  add_isdefined_by_genet world model uri_tool;
  add_label world model uri_tool "Tool"
;;

let add_branch_class world model =
  let uri_branch = Rdf_node.new_from_uri_string world Grdfs.genet_branch in
  add_istype_class world model uri_branch;
  add_isdefined_by_genet world model uri_branch;
  add_label world model uri_branch "Branch"
;;

let add_intf_class world model =
  let uri_intf = Rdf_node.new_from_uri_string world Grdfs.genet_intf in
  add_istype_class world model uri_intf;
  add_isdefined_by_genet world model uri_intf;
  add_label world model uri_intf "Interface"
;;

let add_version_class world model =
  let uri_version = Rdf_node.new_from_uri_string world Grdfs.genet_version in
  add_istype_class world model uri_version;
  add_isdefined_by_genet world model uri_version;
  add_label world model uri_version "Version"
;;

let add_filetype_class world model =
  let uri_ft = Rdf_node.new_from_uri_string world Grdfs.genet_filetype in
  add_istype_class world model uri_ft;
  add_isdefined_by_genet world model uri_ft;
  add_label world model uri_ft "Filetype"
;;

let add_vocabulary world model =
  add_tool_class world model;
  add_branch_class world model;
  add_intf_class world model;
  add_version_class world model;
  add_filetype_class world model;

  let node_literal = Rdf_node.new_from_uri_string world (Grdfs.rdfs_"Literal") in
  let node_branch = Rdf_node.new_from_uri_string world Grdfs.genet_branch in
  let node_intf = Rdf_node.new_from_uri_string world Grdfs.genet_intf in
  let node_version = Rdf_node.new_from_uri_string world Grdfs.genet_version in
  let node_filetype = Rdf_node.new_from_uri_string world Grdfs.genet_filetype in
  let node_seq = Rdf_node.new_from_uri_string world (Grdfs.rdf_"Seq") in
  let props = [
      (Grdfs.genet_name, "has name", "Name", None, Some node_literal) ;
      (Grdfs.genet_hasbranch, "has branch", "Has a branch", None, Some node_branch) ;
      (Grdfs.genet_nointf, "has no interface", "Does not implement the interface", None, Some node_intf) ;
      (Grdfs.genet_hasintf, "has interface", "Implement the interface", None, Some node_intf) ;
      (Grdfs.genet_haspath, "has path", "The path of the command", Some node_intf, Some node_literal) ;
      (Grdfs.genet_hasdiffcom, "has diff command", "The path of the command to compute diffs",
       Some node_filetype, Some node_literal) ;
      (Grdfs.genet_hasversion, "has version", "Has version", None, Some node_version) ;
      (Grdfs.genet_consumes, "consumes", "Filetypes consumed, ordered", Some node_intf, Some node_seq);
      (Grdfs.genet_produces, "produces", "Filetypes produced, ordered", Some node_intf, Some node_seq);
    ]
  in
  let definedby = Rdf_node.new_from_uri_string world Grdfs.genet in
  List.iter
  (fun (uri, label, comment, domain, range) ->
     add_property world model
     ~label ~comment ~definedby ?domain ?range
     (Rdf_node.new_from_uri_string world uri)
  )
  props
;;

let output_format = ref "rdfxml";;

let options =
  Options.option_version "Genet-genrdfs"::
  ("--ntriples", Arg.Unit (fun () -> output_format := "ntriples"), " output in ntriples format") ::
  ("--turtle", Arg.Unit (fun () -> output_format := "turtle"), " output in turtle format") ::
  []
;;

let main () =
  let _options = Options.parse options in
  let world = Rdf_init.new_world () in

  Rdf_init.open_world world;
  let storage = Rdf_storage.new_storage world ~factory: "memory" ~name: "test" in
  let model = Rdf_model.new_model world storage in
  add_vocabulary world model;
  match Rdf_model.to_string model ~name: !output_format with
    None -> failwith "Failed to serialize model"
  | Some string -> print_string string
;;

let _ = Misc.safe_main main;;