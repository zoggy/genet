(** Main module of Gui for Chains.*)

open Chn_types

class mod_box ctx ~on_select ~on_unselect =
  object(self)
    inherit [string] Gmylist.plist `SINGLE
      [None, Gmylist.String Filename.basename] false

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

class buffer ctx file =
  let buf = create_buffer ctx in
  let s = Misc.string_of_file file in
  let _ = buf#insert s in
  object(self)
    method source_buffer = buf
  end
;;

class code_box ctx =
  let wscroll = GBin.scrolled_window
    ~hpolicy: `AUTOMATIC ~vpolicy: `AUTOMATIC ()
  in
  let view = GSourceView2.source_view
    ~packing: wscroll#add_with_viewport ()
  in
  object(self)
    method box = wscroll#coerce
    method set_buffer buf = view#set_buffer buf

    initializer
      Gtksv_utils.set_source_style_scheme
        (Gtksv_utils.read_style_scheme_selection ());

      Gtksv_utils.register_source_view view;
      Gtksv_utils.apply_sourceview_props view (Gtksv_utils.read_sourceview_props ()) ;
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
          try ignore(Smap.find file buffers)
          with Not_found ->
            let b = new buffer ctx file in
            buffers <- Smap.add file b buffers
        )
        files;
      mod_box#update_data files

    method on_mod_select file =
      try
        let b = Smap.find file buffers in
        code_box#set_buffer (b#source_buffer :> GText.buffer)
      with Not_found -> assert false
    method on_mod_unselect s = ()

    initializer
      on_mod_select := self#on_mod_select ;
      on_mod_unselect := self#on_mod_unselect ;
      self#update_module_list ;
      paned#add1 mod_box#box ;
      paned#add2 code_box#box
  end


