(** Main module of GUI. *)


class main ctx =
  let main = new Gui_base.win_main
      ~file: Gui_install.glade_file
      ~autoconnect:false ()
  in
  let wnote = main#wnote in
  let chnbox = new Chngui.box ctx in
  let toolbox = new Toolgui.box ctx in
  let instbox = new Instgui.box ctx in
  object(self)

    initializer
      List.iter (fun _ -> wnote#remove_page 0) wnote#children;
      let add_tab (text, b) =
        ignore(
         wnote#append_page
           ~tab_label: (GMisc.label ~text())#coerce
           b
        )
      in
      List.iter add_tab
        [ "Tools", toolbox#coerce ;
          "Chains", chnbox#coerce ;
          "Inst chains", instbox#coerce ;
        ];

      ignore(main#menuquit#connect#activate GMain.Main.quit);
      ignore(main#win_main#connect#destroy GMain.Main.quit);
  end
;;

let make_gui ctx =
  let _gui = new main ctx in
  ignore(GMain.Main.main())
;;
