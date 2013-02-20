(** Gui elements to display instanciated chains. *)

open Chn_types ;;
open Chn_ast ;;

class tool_version_selection ctx uri on_change =
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
      wcombo#set_popdown_strings choices;
      ignore(wcombo#entry#connect#changed on_change)
  end
;;

class tool_selection ctx on_change =
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
        let version_sel = new tool_version_selection ctx uri on_change in
        wtable#attach ~left: 1 ~top: i ~expand: `X version_sel#coerce;
        version_sel
      in
      tool_versions <- List.mapi add_tool tools
  end
;;

class input_selection ctx on_change =
  let wcombo = GEdit.combo
    ~enable_arrow_keys: true
    ~value_in_list: true
    ~allow_empty: true
    ~case_sensitive: true
    ()
  in
  object(self)
    method coerce = wcombo#coerce
    method selection =
      match wcombo#entry#text with
        "" -> None
      | s -> Some s

    initializer
      let inputs = Ind_io.list_inputs ctx.ctx_cfg in
      let inputs = List.sort Pervasives.compare inputs in
      wcombo#set_popdown_strings ("" :: inputs);
      ignore(wcombo#entry#connect#changed on_change)
  end
;;

class fchain_selection ctx on_change =
  let wcombo = GEdit.combo
    ~enable_arrow_keys: true
    ~value_in_list: true
    ~allow_empty: true
    ~case_sensitive: true
    ()
  in
  object(self)
    val mutable map = Smap.empty
    method coerce = wcombo#coerce
    method selection =
      match wcombo#entry#text with
        "" -> None
      | s ->
          try Some (Smap.find s map)
          with Not_found -> None

    initializer
      let f_fchain acc uri =
        match Chn_types.is_uri_fchain ctx uri with
        | None -> acc
        | Some name ->
            match Chn_types.fchain_id name with
              None -> acc
            | Some id ->
                let s = " "^id in
                map <- Smap.add s uri map;
                s :: acc
      in
      let f_chain modname acc chn =
        let name = Chn_types.mk_chain_name modname chn.Chn_ast.chn_name in
        let uri = Chn_types.uri_chain ctx.ctx_cfg.Config.rest_api name in
        let s_name = Chn_types.string_of_chain_name name in
        map <- Smap.add s_name uri map ;
        let acc = s_name :: acc in
        let flats = Chn_flat.flat_chains_of_chain ctx name in
        List.fold_left f_fchain acc flats
      in
      let f_mod acc m =
        let chains = List.sort
          (fun c1 c2 -> Pervasives.compare c1.chn_name c2.chn_name)
            m.cmod_chains
        in
        List.fold_left (f_chain m.cmod_name) acc chains
      in

      let chain_files = Chn_io.chain_files ctx.ctx_cfg in
      let modules = List.map Chn_io.chn_module_of_file chain_files in
      let modules = List.sort
        (fun m1 m2 ->
           Chn_types.compare_chain_modname
             m1.cmod_name m2.cmod_name)
           modules
      in
      let choices = List.rev (List.fold_left f_mod [] modules) in
      wcombo#set_popdown_strings ("" :: choices);
      ignore(wcombo#entry#connect#changed on_change)
  end
;;

let inst_chain_date ctx uri =
  match Grdfs.creation_date_uri ctx.ctx_rdf uri with
    None -> "??"
  | Some d -> Netdate.mk_mail_date (Netdate.since_epoch d)
;;

let inst_chain_name ctx uri =
  match Chn_types.is_uri_ichain ctx.ctx_cfg.Config.rest_api uri with
    None -> Printf.sprintf "%S is not an inst. chain" (Rdf_uri.string uri)
  | Some icname -> Chn_types.string_of_ichain_name icname
;;

class inst_list ctx on_sel_change =
  object(self)
    inherit [Rdf_uri.uri] Gmylist.plist `SINGLE
      [ None, Gmylist.String (inst_chain_name ctx);
        None, Gmylist.String (inst_chain_date ctx) ;
      ]
      false
    method coerce = self#box
    method on_select _ = on_sel_change ()
    method on_deselect _ = on_sel_change ()
  end
;;

let empty_dot_graph =
  {
    Odot.strict = false ;
    kind = Odot.Digraph ;
    id = None ;
    stmt_list = [] ;
  }

class inst_fig ctx =
  object(self)
    inherit Odot_gtk.box ~tmp_hash: "genetinstgui" ()
    val mutable graph = empty_dot_graph

    method coerce = self#box#coerce
    method build_graph = graph

    method refresh_data = ()
    method on_button1_press ~x ~y _ = ()
    method set_dot dot_code =
      try
        graph <- Odot.parse_string dot_code;
        self#refresh ()
      with
        Odot.Parse_error (line, char) ->
          failwith (Printf.sprintf "Dot parse error line %d, character %d" line char)

    method display_inst = function
      None -> graph <- empty_dot_graph ; self#refresh ()
    | Some uri ->
        let ichain_name =
          match Chn_types.is_uri_ichain ctx.ctx_cfg.Config.rest_api uri with
            None -> failwith (Printf.sprintf "Not an instanciated chain uri: %S" (Rdf_uri.string uri))
          | Some n -> n
        in
        let dot = Rest_xpage.dot_of_ichain ctx ichain_name in
        self#set_dot dot
  end
;;

type inst_filter =
  { filt_input : string option ;
    filt_fchain : Rdf_uri.uri option ;
    filt_tools : (Rdf_uri.uri * Rdf_uri.uri) list ;
  }

class inst_chain_box ctx on_sel_change =
  let vbox = GPack.vbox () in
  let wtable = GPack.table ~columns: 2 ~rows: 3 () in
  let on_selection_changed = ref (fun () -> ()) in
  let inst_fig = new inst_fig ctx in
  let inst_list = new inst_list ctx (fun () -> !on_selection_changed ()) in
  let on_filter_change = ref (fun () -> ()) in
  object(self)
    val mutable input_sel = new input_selection ctx (fun () -> !on_filter_change())
    val mutable fchain_sel = new fchain_selection ctx (fun () -> !on_filter_change())
    val mutable tool_sel = new tool_selection ctx (fun () -> !on_filter_change())

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

    method selected =
      match inst_list#selection with
        [] -> None
      | i :: _ -> Some i

    initializer
      vbox#pack ~expand: false ~fill: true wtable#coerce;
      let paned = GPack.paned
        `VERTICAL ~packing: (vbox#pack ~expand: true ~fill: true) ()
      in
      paned#add1 inst_list#coerce;
      paned#add2 inst_fig#coerce;
      paned#set_position 150;

      self#fill_table;

      let filter_change () =
        let filter = self#selection in
        let input =
          match filter.filt_input with
            None -> None
          | Some s -> Some (Misc.split_filename s, None)
        in
        let tools = List.fold_left
          (fun map (tool, version) -> Urimap.add tool version map)
          Urimap.empty filter.filt_tools
        in
        let list = Chn_inst_query.query_instances ctx
          ?input: input
            ?chain: filter.filt_fchain
            ~tools
        in
        inst_list#update_data list
      in
      on_filter_change := filter_change;

      let selection_changed () =
        inst_fig#display_inst self#selected ;
        on_sel_change ();
      in
      on_selection_changed := selection_changed ;
  end
;;

class diff_box ctx =
  let vbox = GPack.vbox () in
  let wscroll = GBin.scrolled_window
    ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC
    ~packing: (vbox#pack ~expand: true ~fill: true)
    ()
  in
  let view = GSourceView2.source_view
    ~show_line_numbers: true
    ~packing: wscroll#add_with_viewport ()
  in
  object(self)
    method coerce = vbox#coerce
    method display_diff diff =
      let b = view#source_buffer in
      b#delete ~start: b#start_iter ~stop: b#end_iter;
      b#insert diff


    initializer
      view#source_buffer#set_language
        (Gtksv_utils.source_language_by_name "Diff");
      Gtksv_utils.set_source_style_scheme
        (Gtksv_utils.read_style_scheme_selection ());

      Gtksv_utils.register_source_view view;
      Gtksv_utils.apply_sourceview_props view (Gtksv_utils.read_sourceview_props ()) ;
  end

let escape_amp =
  Str.global_replace (Str.regexp_string "&") "&amp;"
;;


class box ctx =
  let paned = GPack.paned `VERTICAL () in
  let hbox = GPack.hbox () in
  let on_inst_sel_change = ref (fun () -> ()) in
  let instbox1 = new inst_chain_box ctx (fun () -> !on_inst_sel_change ()) in
  let instbox2 = new inst_chain_box ctx (fun () -> !on_inst_sel_change ()) in
  let vbox_diff = GPack.vbox () in
  let hbox_url = GPack.hbox ~packing: (vbox_diff#pack ~expand: false ~fill: true) () in
  let _ = GMisc.label ~text: "URL:" ~xpad: 3
    ~packing: (hbox_url#pack ~expand: false ~fill: true) ()
  in
  let wurl = GMisc.label ~selectable: true ~markup: ""
     ~packing: (hbox_url#pack ~expand: true ~fill: true) ()
  in
  let diffbox = new diff_box ctx in
  object(self)

    method coerce = paned#coerce

    method update_diff () =
      match instbox1#selected, instbox2#selected with
        None, _
      | _, None ->
          wurl#set_text "";
          diffbox#display_diff ""
      | Some inst1, Some inst2->
          let url = Chn_diff.diff_url ctx ~inst1 ~inst2 () in
          let url = escape_amp url in
          wurl#set_text (Printf.sprintf "<a href=%S>%s</a>" url url);
          wurl#set_use_markup true;
          let diff = Chn_diff.diff ctx inst1 inst2 in
          diffbox#display_diff diff


    initializer
      on_inst_sel_change := self#update_diff;
      let wf1 = GBin.frame ~label: "Instanciation 1" () in
      hbox#pack ~expand: true ~fill: true wf1#coerce;
      wf1#add instbox1#coerce;

      let wf2 = GBin.frame ~label: "Instanciation 2" () in
      hbox#pack ~expand: true ~fill: true wf2#coerce;
      wf2#add instbox2#coerce;

      paned#add1 hbox#coerce ;
      paned#add2 vbox_diff#coerce ;
      paned#set_position 400;

      vbox_diff#pack ~expand: true ~fill: true diffbox#coerce;


  end;;
  