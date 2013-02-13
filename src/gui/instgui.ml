(** Gui elements to display instanciated chains. *)

open Chn_types;;

class tool_version_selection ctx uri =
  let versions = Grdf_version.versions_of ctx.ctx_rdf ~recur:true uri in
  let map = List.fold_left
    (fun acc uri ->
      let name = Grdf_version.name ctx.ctx_rdf uri in
      Smap.add name uri acc
    )
    Smap.empty versions
  in
  let wcombo = GEdit.combo
    ~enable_arrow_keys: true
    ~value_in_list: true
    ~allow_empty: true
    ~case_sensitive: true
    ()
  in
  object(self)
    method coerce = wcombo#coerce
    method get =
      match wcombo#entry#text with
        "" -> None
      | s ->
        try Some (Smap.find s map)
        with Not_found -> None

    method tool = uri

    initializer
      let choices = Smap.fold (fun name _ acc -> name :: acc) map [] in
      let choices = "" :: (List.rev choices) in
      wcombo#set_popdown_strings choices
  end
;;

class tool_selection ctx =
  let tools = Grdf_tool.tools ctx.ctx_rdf in
  let wtable = GPack.table ~columns: 2 ~rows: (List.length tools) () in
  object
    val mutable tool_versions = []
    method coerce = wtable#coerce
    method selection =
      List.fold_left
        (fun acc sel ->
           match sel#get with
             None -> acc
           | Some version -> (sel#tool, version) :: acc
        )
        []
        tool_versions

    initializer
      let add_tool i uri =
        let name = Grdf_tool.name ctx.ctx_rdf uri in
        let label = GMisc.label ~xpad: 3 ~xalign: 1. ~text: name () in
        wtable#attach ~left: 0 ~top: i label#coerce;
        let version_sel = new tool_version_selection ctx uri in
        wtable#attach ~left: 1 ~top: i ~expand: `X version_sel#coerce;
        version_sel
      in
      tool_versions <- List.mapi add_tool tools
  end
;;

class input_selection ctx =
  let wcombo = GEdit.combo
    ~enable_arrow_keys: true
    ~value_in_list: true
    ~allow_empty: true
    ~case_sensitive: true
    ()
  in
  object(self)
    method coerce = wcombo#coerce
    method selection = (None : string option)
  end
;;

class fchain_selection ctx =
  let wcombo = GEdit.combo
    ~enable_arrow_keys: true
    ~value_in_list: true
    ~allow_empty: true
    ~case_sensitive: true
    ()
  in
  object(self)
    method coerce = wcombo#coerce
    method selection = (None : Rdf_uri.uri option)
  end
;;


type inst_filter =
  { filt_input : string option ;
    filt_fchain : Rdf_uri.uri option ;
    filt_tools : (Rdf_uri.uri * Rdf_uri.uri) list ;
  }

class inst_chain_box ctx =
  let vbox = GPack.vbox () in
  let wtable = GPack.table ~columns: 2 ~rows: 3 () in

  object(self)
    val mutable input_sel = new input_selection ctx
    val mutable fchain_sel = new fchain_selection ctx
    val mutable tool_sel = new tool_selection ctx

    method coerce = vbox#coerce

    method fill_table =
      wtable#attach ~left: 0 ~top: 0
        (GMisc.label ~text: "Input" ~xpad: 3 ~xalign: 1. ())#coerce;
      wtable#attach ~left: 1 ~right: 2 ~top: 0 ~expand: `X input_sel#coerce ;

      wtable#attach ~left: 0 ~top: 1
        (GMisc.label ~text: "Chain" ~xpad: 3 ~xalign: 1. ())#coerce;
      wtable#attach ~left: 1 ~right: 2 ~top: 1 ~expand: `X fchain_sel#coerce ;

      wtable#attach ~left: 0 ~right: 2 ~top: 2 ~expand: `BOTH tool_sel#coerce


    method selection =
      { filt_input = input_sel#selection ;
        filt_fchain = fchain_sel#selection ;
        filt_tools = tool_sel#selection ;
      }

    initializer
      vbox#pack ~expand: false ~fill: true wtable#coerce;
      self#fill_table
  end
;;

class box ctx =
  let paned = GPack.paned `VERTICAL () in
  let hbox = GPack.hbox () in
  let diffbox = GPack.vbox () in
  let instbox1 = new inst_chain_box ctx in
  let instbox2 = new inst_chain_box ctx in
  object(self)

    method coerce = paned#coerce

    initializer
      let wf1 = GBin.frame ~label: "Instanciation 1" () in
      hbox#pack ~expand: true ~fill: true wf1#coerce;
      wf1#add instbox1#coerce;

      let wf2 = GBin.frame ~label: "Instanciation 2" () in
      hbox#pack ~expand: true ~fill: true wf2#coerce;
      wf2#add instbox2#coerce;

      paned#add1 hbox#coerce ;
      paned#add2 diffbox#coerce ;
      paned#set_position 400;
  end;;
  