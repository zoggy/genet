(** Main module of Gui for Chains.*)

open Chn_types

let location_of_iter iter =
  let char = iter#offset in
  let line_start = (iter#set_line_offset 0)#offset in
  (iter#line, char - line_start)
;;
let location_in_buffer (b : GSourceView2.source_buffer) =
  let iter = b#get_iter `INSERT in
  location_of_iter iter
;;

class mod_box ctx ~on_select ~on_unselect =
  object(self)
    inherit [ [`Absolute] Fname.filename] Gmylist.plist `SINGLE
      [None, Gmylist.String (fun f -> Filename.basename (Fname.abs_string f))] false

    method on_select = on_select
    method on_deselect = on_unselect
  end
;;

let create_buffer ctx =
  let language = Gtksv_utils.source_language_by_name "Genet" in
  let buffer = GSourceView2.source_buffer
    ?language
    ~highlight_matching_brackets: true
    ~highlight_syntax: true
    ()
  in
  Gtksv_utils.register_source_buffer buffer;
  buffer

class buffer ctx (file : [`Absolute] Fname.filename) =
  let buf = create_buffer ctx in
  let s = Misc.string_of_file (Fname.abs_string file) in
  let _ = buf#insert s in
  object(self)
    method source_buffer = buf
    method file = file
  end
;;

class fig_box ctx =
  let image = GMisc.image () in

  object(self)
    method box = image#coerce
    method display_file file = image#set_file file
    method reset = image#clear
  end
;;

class code_box ctx =
  let paned = GPack.paned `VERTICAL () in
  let wscroll = GBin.scrolled_window
    ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC ()
  in
  let dummy_buf = GSourceView2.source_buffer () in
  let view = GSourceView2.source_view
    ~source_buffer: dummy_buf
    ~show_line_numbers: true
    ~packing: wscroll#add_with_viewport ()
  in
  let vbox = GPack.vbox () in
  let wlabel = GMisc.label ~text: "" ~packing: (vbox#pack ~expand: false ~fill: true) () in
  let fig_box = new fig_box ctx in
  object(self)
    val svg_tmp = Filename.temp_file "genetgui" ".svg"
    val mutable buffer = (None : buffer option)
    val mutable update_fig_cb_id = None

    method box = paned#coerce
    method set_buffer = function
      None ->
        buffer <- None ;
        view#set_buffer (dummy_buf :> GText.buffer);
        view#misc#set_sensitive false
    | Some buf ->
        buffer <- Some buf ;
        view#set_buffer (buf#source_buffer :> GText.buffer) ;
        view#misc#set_sensitive true

    method show_fig chn =
      let p = new Chn_ast.chain_dot_printer in
      let dot = p#dot_of_chain ~prefix: ctx.ctx_cfg.Config.rest_api chn in
      let svg = Grdf_dot.dot_to_svg dot in
      Misc.file_of_string ~file: svg_tmp (Xtmpl.string_of_xmls svg) ;
      fig_box#display_file svg_tmp;
      (try Sys.remove svg_tmp with _ -> ())

    method update_fig () =
      try
        wlabel#set_text "";
        match buffer with
          None -> fig_box#reset ()
        | Some buf ->
            let modname = Chn_io.modname_of_file buf#file in
            let code = buf#source_buffer#get_text () in
            let chn_mod = Chn_io.chn_module_of_string 
              ~file: (Fname.abs_string buf#file) modname code
            in
            let (line, char) = location_in_buffer buf#source_buffer in
            let chn =
              List.find
                (fun chn ->
                  let loc = chn.Chn_ast.chn_loc in
                   line >= loc.Loc.loc_start.Lexing.pos_lnum &&
                     line <= loc.Loc.loc_end.Lexing.pos_lnum
                )
                chn_mod.Chn_ast.cmod_chains
            in
            self#show_fig chn
      with
        e ->
          let msg =
            match e with
              Sys_error s | Failure s -> s
            | Loc.Problem pb -> Loc.string_of_problem pb
            | _ -> Printexc.to_string e
          in
          wlabel#set_text msg ;
          fig_box#reset ()

    initializer
      vbox#pack ~expand: true ~fill: true fig_box#box ;
      paned#add1 wscroll#coerce;
      paned#add2 vbox#coerce;
      paned#set_position 300 ;
      Gtksv_utils.set_source_style_scheme
        (Gtksv_utils.read_style_scheme_selection ());

      Gtksv_utils.register_source_view view;
      Gtksv_utils.apply_sourceview_props view (Gtksv_utils.read_sourceview_props ()) ;

      ignore(view#event#connect#focus_in
        (fun _ ->
          match update_fig_cb_id with
            None ->
                let id = GMain.Timeout.add ~ms:2000 ~callback:(fun () -> self#update_fig(); true) in
                update_fig_cb_id <- Some id; false
            | Some _ -> false
         )
      );
      ignore(view#event#connect#focus_out
        (fun _ ->
          match update_fig_cb_id with
            None -> false
          | Some id -> GMain.Timeout.remove id; update_fig_cb_id <- None; false
         )
      )
  end
;;

class box ctx =
  let paned = GPack.paned `HORIZONTAL () in

  let on_mod_select = ref (fun _ -> ()) in
  let on_mod_unselect = ref (fun _ -> ()) in
  let mod_box = new mod_box ctx
    ~on_select: (fun s -> !on_mod_select s)
    ~on_unselect: (fun s -> !on_mod_unselect s)
  in
  let code_box = new code_box ctx in
  object(self)
    val mutable buffers = Smap.empty

    method coerce = paned#coerce

    method update_module_list =
      let files = Chn_io.chain_files ctx.ctx_cfg in
      List.iter
        (fun file ->
          let file_s = Fname.abs_string file in
          try ignore(Smap.find file_s buffers)
          with Not_found ->
            let b = new buffer ctx file in
            buffers <- Smap.add file_s b buffers
        )
        files;
      mod_box#update_data files

    method on_mod_select file =
      try
        let b = Smap.find (Fname.abs_string file) buffers in
        code_box#set_buffer (Some b)
      with Not_found -> assert false

    method on_mod_unselect s =
      code_box#set_buffer None

    initializer
      on_mod_select := self#on_mod_select ;
      on_mod_unselect := self#on_mod_unselect ;
      self#update_module_list ;
      paned#add1 mod_box#box ;
      paned#add2 code_box#box ;
      paned#set_position 120 ;



  end


